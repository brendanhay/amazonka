{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.NielsenActiveWatermarkProcessType
import Network.AWS.MediaConvert.Types.NielsenSourceWatermarkStatusType
import Network.AWS.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
import qualified Network.AWS.Prelude as Prelude

-- | Ignore these settings unless you are using Nielsen non-linear
-- watermarking. Specify the values that MediaConvert uses to generate and
-- place Nielsen watermarks in your output audio. In addition to specifying
-- these values, you also need to set up your cloud TIC server. These
-- settings apply to every output in your job. The MediaConvert
-- implementation is currently with the following Nielsen versions: Nielsen
-- Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7
-- Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
--
-- /See:/ 'newNielsenNonLinearWatermarkSettings' smart constructor.
data NielsenNonLinearWatermarkSettings = NielsenNonLinearWatermarkSettings'
  { -- | Use the asset name that you provide to Nielsen for this asset. Required
    -- for all Nielsen non-linear watermarking.
    assetName :: Prelude.Maybe Prelude.Text,
    -- | Choose the type of Nielsen watermarks that you want in your outputs.
    -- When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value
    -- for the setting SID (sourceId). When you choose CBET (CBET), you must
    -- provide a value for the setting CSID (cbetSourceId). When you choose
    -- NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values
    -- for both of these settings.
    activeWatermarkProcess :: Prelude.Maybe NielsenActiveWatermarkProcessType,
    -- | Required. Specify whether your source content already contains Nielsen
    -- non-linear watermarks. When you set this value to Watermarked
    -- (WATERMARKED), the service fails the job. Nielsen requires that you add
    -- non-linear watermarking to only clean content that doesn\'t already have
    -- non-linear Nielsen watermarks.
    sourceWatermarkStatus :: Prelude.Maybe NielsenSourceWatermarkStatusType,
    -- | Specify the endpoint for the TIC server that you have deployed and
    -- configured in the AWS Cloud. Required for all Nielsen non-linear
    -- watermarking. MediaConvert can\'t connect directly to a TIC server.
    -- Instead, you must use API Gateway to provide a RESTful interface between
    -- MediaConvert and a TIC server that you deploy in your AWS account. For
    -- more information on deploying a TIC server in your AWS account and the
    -- required API Gateway, contact Nielsen support.
    ticServerUrl :: Prelude.Maybe Prelude.Text,
    -- | Use the SID that Nielsen provides to you. This source ID should be
    -- unique to your Nielsen account but common to all of your output assets.
    -- Required for all Nielsen non-linear watermarking. This ID should be
    -- unique to your Nielsen account but common to all of your output assets.
    -- Required for all Nielsen non-linear watermarking.
    sourceId :: Prelude.Maybe Prelude.Natural,
    -- | Use the CSID that Nielsen provides to you. This CBET source ID should be
    -- unique to your Nielsen account but common to all of your output assets
    -- that have CBET watermarking. Required when you choose a value for the
    -- setting Watermark types (ActiveWatermarkProcess) that includes CBET.
    cbetSourceId :: Prelude.Maybe Prelude.Text,
    -- | Optional. If this asset uses an episode ID with Nielsen, provide it
    -- here.
    episodeId :: Prelude.Maybe Prelude.Text,
    -- | Specify the Amazon S3 location where you want MediaConvert to save your
    -- Nielsen non-linear metadata .zip file. This Amazon S3 bucket must be in
    -- the same Region as the one where you do your MediaConvert transcoding.
    -- If you want to include an ADI file in this .zip file, use the setting
    -- ADI file (adiFilename) to specify it. MediaConvert delivers the Nielsen
    -- metadata .zip files only to your metadata destination Amazon S3 bucket.
    -- It doesn\'t deliver the .zip files to Nielsen. You are responsible for
    -- delivering the metadata .zip files to Nielsen.
    metadataDestination :: Prelude.Maybe Prelude.Text,
    -- | To create assets that have the same TIC values in each audio track, keep
    -- the default value Share TICs (SAME_TICS_PER_TRACK). To create assets
    -- that have unique TIC values for each audio track, choose Use unique TICs
    -- (RESERVE_UNIQUE_TICS_PER_TRACK).
    uniqueTicPerAudioTrack :: Prelude.Maybe NielsenUniqueTicPerAudioTrackType,
    -- | Optional. Use this setting when you want the service to include an ADI
    -- file in the Nielsen metadata .zip file. To provide an ADI file, store it
    -- in Amazon S3 and provide a URL to it here. The URL should be in the
    -- following format: S3:\/\/bucket\/path\/ADI-file. For more information
    -- about the metadata .zip file, see the setting Metadata destination
    -- (metadataDestination).
    adiFilename :: Prelude.Maybe Prelude.Text,
    -- | Use the asset ID that you provide to Nielsen to uniquely identify this
    -- asset. Required for all Nielsen non-linear watermarking.
    assetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NielsenNonLinearWatermarkSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetName', 'nielsenNonLinearWatermarkSettings_assetName' - Use the asset name that you provide to Nielsen for this asset. Required
-- for all Nielsen non-linear watermarking.
--
-- 'activeWatermarkProcess', 'nielsenNonLinearWatermarkSettings_activeWatermarkProcess' - Choose the type of Nielsen watermarks that you want in your outputs.
-- When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value
-- for the setting SID (sourceId). When you choose CBET (CBET), you must
-- provide a value for the setting CSID (cbetSourceId). When you choose
-- NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values
-- for both of these settings.
--
-- 'sourceWatermarkStatus', 'nielsenNonLinearWatermarkSettings_sourceWatermarkStatus' - Required. Specify whether your source content already contains Nielsen
-- non-linear watermarks. When you set this value to Watermarked
-- (WATERMARKED), the service fails the job. Nielsen requires that you add
-- non-linear watermarking to only clean content that doesn\'t already have
-- non-linear Nielsen watermarks.
--
-- 'ticServerUrl', 'nielsenNonLinearWatermarkSettings_ticServerUrl' - Specify the endpoint for the TIC server that you have deployed and
-- configured in the AWS Cloud. Required for all Nielsen non-linear
-- watermarking. MediaConvert can\'t connect directly to a TIC server.
-- Instead, you must use API Gateway to provide a RESTful interface between
-- MediaConvert and a TIC server that you deploy in your AWS account. For
-- more information on deploying a TIC server in your AWS account and the
-- required API Gateway, contact Nielsen support.
--
-- 'sourceId', 'nielsenNonLinearWatermarkSettings_sourceId' - Use the SID that Nielsen provides to you. This source ID should be
-- unique to your Nielsen account but common to all of your output assets.
-- Required for all Nielsen non-linear watermarking. This ID should be
-- unique to your Nielsen account but common to all of your output assets.
-- Required for all Nielsen non-linear watermarking.
--
-- 'cbetSourceId', 'nielsenNonLinearWatermarkSettings_cbetSourceId' - Use the CSID that Nielsen provides to you. This CBET source ID should be
-- unique to your Nielsen account but common to all of your output assets
-- that have CBET watermarking. Required when you choose a value for the
-- setting Watermark types (ActiveWatermarkProcess) that includes CBET.
--
-- 'episodeId', 'nielsenNonLinearWatermarkSettings_episodeId' - Optional. If this asset uses an episode ID with Nielsen, provide it
-- here.
--
-- 'metadataDestination', 'nielsenNonLinearWatermarkSettings_metadataDestination' - Specify the Amazon S3 location where you want MediaConvert to save your
-- Nielsen non-linear metadata .zip file. This Amazon S3 bucket must be in
-- the same Region as the one where you do your MediaConvert transcoding.
-- If you want to include an ADI file in this .zip file, use the setting
-- ADI file (adiFilename) to specify it. MediaConvert delivers the Nielsen
-- metadata .zip files only to your metadata destination Amazon S3 bucket.
-- It doesn\'t deliver the .zip files to Nielsen. You are responsible for
-- delivering the metadata .zip files to Nielsen.
--
-- 'uniqueTicPerAudioTrack', 'nielsenNonLinearWatermarkSettings_uniqueTicPerAudioTrack' - To create assets that have the same TIC values in each audio track, keep
-- the default value Share TICs (SAME_TICS_PER_TRACK). To create assets
-- that have unique TIC values for each audio track, choose Use unique TICs
-- (RESERVE_UNIQUE_TICS_PER_TRACK).
--
-- 'adiFilename', 'nielsenNonLinearWatermarkSettings_adiFilename' - Optional. Use this setting when you want the service to include an ADI
-- file in the Nielsen metadata .zip file. To provide an ADI file, store it
-- in Amazon S3 and provide a URL to it here. The URL should be in the
-- following format: S3:\/\/bucket\/path\/ADI-file. For more information
-- about the metadata .zip file, see the setting Metadata destination
-- (metadataDestination).
--
-- 'assetId', 'nielsenNonLinearWatermarkSettings_assetId' - Use the asset ID that you provide to Nielsen to uniquely identify this
-- asset. Required for all Nielsen non-linear watermarking.
newNielsenNonLinearWatermarkSettings ::
  NielsenNonLinearWatermarkSettings
newNielsenNonLinearWatermarkSettings =
  NielsenNonLinearWatermarkSettings'
    { assetName =
        Prelude.Nothing,
      activeWatermarkProcess = Prelude.Nothing,
      sourceWatermarkStatus = Prelude.Nothing,
      ticServerUrl = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      cbetSourceId = Prelude.Nothing,
      episodeId = Prelude.Nothing,
      metadataDestination = Prelude.Nothing,
      uniqueTicPerAudioTrack = Prelude.Nothing,
      adiFilename = Prelude.Nothing,
      assetId = Prelude.Nothing
    }

-- | Use the asset name that you provide to Nielsen for this asset. Required
-- for all Nielsen non-linear watermarking.
nielsenNonLinearWatermarkSettings_assetName :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_assetName = Lens.lens (\NielsenNonLinearWatermarkSettings' {assetName} -> assetName) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {assetName = a} :: NielsenNonLinearWatermarkSettings)

-- | Choose the type of Nielsen watermarks that you want in your outputs.
-- When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value
-- for the setting SID (sourceId). When you choose CBET (CBET), you must
-- provide a value for the setting CSID (cbetSourceId). When you choose
-- NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values
-- for both of these settings.
nielsenNonLinearWatermarkSettings_activeWatermarkProcess :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe NielsenActiveWatermarkProcessType)
nielsenNonLinearWatermarkSettings_activeWatermarkProcess = Lens.lens (\NielsenNonLinearWatermarkSettings' {activeWatermarkProcess} -> activeWatermarkProcess) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {activeWatermarkProcess = a} :: NielsenNonLinearWatermarkSettings)

-- | Required. Specify whether your source content already contains Nielsen
-- non-linear watermarks. When you set this value to Watermarked
-- (WATERMARKED), the service fails the job. Nielsen requires that you add
-- non-linear watermarking to only clean content that doesn\'t already have
-- non-linear Nielsen watermarks.
nielsenNonLinearWatermarkSettings_sourceWatermarkStatus :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe NielsenSourceWatermarkStatusType)
nielsenNonLinearWatermarkSettings_sourceWatermarkStatus = Lens.lens (\NielsenNonLinearWatermarkSettings' {sourceWatermarkStatus} -> sourceWatermarkStatus) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {sourceWatermarkStatus = a} :: NielsenNonLinearWatermarkSettings)

-- | Specify the endpoint for the TIC server that you have deployed and
-- configured in the AWS Cloud. Required for all Nielsen non-linear
-- watermarking. MediaConvert can\'t connect directly to a TIC server.
-- Instead, you must use API Gateway to provide a RESTful interface between
-- MediaConvert and a TIC server that you deploy in your AWS account. For
-- more information on deploying a TIC server in your AWS account and the
-- required API Gateway, contact Nielsen support.
nielsenNonLinearWatermarkSettings_ticServerUrl :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_ticServerUrl = Lens.lens (\NielsenNonLinearWatermarkSettings' {ticServerUrl} -> ticServerUrl) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {ticServerUrl = a} :: NielsenNonLinearWatermarkSettings)

-- | Use the SID that Nielsen provides to you. This source ID should be
-- unique to your Nielsen account but common to all of your output assets.
-- Required for all Nielsen non-linear watermarking. This ID should be
-- unique to your Nielsen account but common to all of your output assets.
-- Required for all Nielsen non-linear watermarking.
nielsenNonLinearWatermarkSettings_sourceId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Natural)
nielsenNonLinearWatermarkSettings_sourceId = Lens.lens (\NielsenNonLinearWatermarkSettings' {sourceId} -> sourceId) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {sourceId = a} :: NielsenNonLinearWatermarkSettings)

-- | Use the CSID that Nielsen provides to you. This CBET source ID should be
-- unique to your Nielsen account but common to all of your output assets
-- that have CBET watermarking. Required when you choose a value for the
-- setting Watermark types (ActiveWatermarkProcess) that includes CBET.
nielsenNonLinearWatermarkSettings_cbetSourceId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_cbetSourceId = Lens.lens (\NielsenNonLinearWatermarkSettings' {cbetSourceId} -> cbetSourceId) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {cbetSourceId = a} :: NielsenNonLinearWatermarkSettings)

-- | Optional. If this asset uses an episode ID with Nielsen, provide it
-- here.
nielsenNonLinearWatermarkSettings_episodeId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_episodeId = Lens.lens (\NielsenNonLinearWatermarkSettings' {episodeId} -> episodeId) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {episodeId = a} :: NielsenNonLinearWatermarkSettings)

-- | Specify the Amazon S3 location where you want MediaConvert to save your
-- Nielsen non-linear metadata .zip file. This Amazon S3 bucket must be in
-- the same Region as the one where you do your MediaConvert transcoding.
-- If you want to include an ADI file in this .zip file, use the setting
-- ADI file (adiFilename) to specify it. MediaConvert delivers the Nielsen
-- metadata .zip files only to your metadata destination Amazon S3 bucket.
-- It doesn\'t deliver the .zip files to Nielsen. You are responsible for
-- delivering the metadata .zip files to Nielsen.
nielsenNonLinearWatermarkSettings_metadataDestination :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_metadataDestination = Lens.lens (\NielsenNonLinearWatermarkSettings' {metadataDestination} -> metadataDestination) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {metadataDestination = a} :: NielsenNonLinearWatermarkSettings)

-- | To create assets that have the same TIC values in each audio track, keep
-- the default value Share TICs (SAME_TICS_PER_TRACK). To create assets
-- that have unique TIC values for each audio track, choose Use unique TICs
-- (RESERVE_UNIQUE_TICS_PER_TRACK).
nielsenNonLinearWatermarkSettings_uniqueTicPerAudioTrack :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe NielsenUniqueTicPerAudioTrackType)
nielsenNonLinearWatermarkSettings_uniqueTicPerAudioTrack = Lens.lens (\NielsenNonLinearWatermarkSettings' {uniqueTicPerAudioTrack} -> uniqueTicPerAudioTrack) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {uniqueTicPerAudioTrack = a} :: NielsenNonLinearWatermarkSettings)

-- | Optional. Use this setting when you want the service to include an ADI
-- file in the Nielsen metadata .zip file. To provide an ADI file, store it
-- in Amazon S3 and provide a URL to it here. The URL should be in the
-- following format: S3:\/\/bucket\/path\/ADI-file. For more information
-- about the metadata .zip file, see the setting Metadata destination
-- (metadataDestination).
nielsenNonLinearWatermarkSettings_adiFilename :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_adiFilename = Lens.lens (\NielsenNonLinearWatermarkSettings' {adiFilename} -> adiFilename) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {adiFilename = a} :: NielsenNonLinearWatermarkSettings)

-- | Use the asset ID that you provide to Nielsen to uniquely identify this
-- asset. Required for all Nielsen non-linear watermarking.
nielsenNonLinearWatermarkSettings_assetId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_assetId = Lens.lens (\NielsenNonLinearWatermarkSettings' {assetId} -> assetId) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {assetId = a} :: NielsenNonLinearWatermarkSettings)

instance
  Core.FromJSON
    NielsenNonLinearWatermarkSettings
  where
  parseJSON =
    Core.withObject
      "NielsenNonLinearWatermarkSettings"
      ( \x ->
          NielsenNonLinearWatermarkSettings'
            Prelude.<$> (x Core..:? "assetName")
            Prelude.<*> (x Core..:? "activeWatermarkProcess")
            Prelude.<*> (x Core..:? "sourceWatermarkStatus")
            Prelude.<*> (x Core..:? "ticServerUrl")
            Prelude.<*> (x Core..:? "sourceId")
            Prelude.<*> (x Core..:? "cbetSourceId")
            Prelude.<*> (x Core..:? "episodeId")
            Prelude.<*> (x Core..:? "metadataDestination")
            Prelude.<*> (x Core..:? "uniqueTicPerAudioTrack")
            Prelude.<*> (x Core..:? "adiFilename")
            Prelude.<*> (x Core..:? "assetId")
      )

instance
  Prelude.Hashable
    NielsenNonLinearWatermarkSettings

instance
  Prelude.NFData
    NielsenNonLinearWatermarkSettings

instance
  Core.ToJSON
    NielsenNonLinearWatermarkSettings
  where
  toJSON NielsenNonLinearWatermarkSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("assetName" Core..=) Prelude.<$> assetName,
            ("activeWatermarkProcess" Core..=)
              Prelude.<$> activeWatermarkProcess,
            ("sourceWatermarkStatus" Core..=)
              Prelude.<$> sourceWatermarkStatus,
            ("ticServerUrl" Core..=) Prelude.<$> ticServerUrl,
            ("sourceId" Core..=) Prelude.<$> sourceId,
            ("cbetSourceId" Core..=) Prelude.<$> cbetSourceId,
            ("episodeId" Core..=) Prelude.<$> episodeId,
            ("metadataDestination" Core..=)
              Prelude.<$> metadataDestination,
            ("uniqueTicPerAudioTrack" Core..=)
              Prelude.<$> uniqueTicPerAudioTrack,
            ("adiFilename" Core..=) Prelude.<$> adiFilename,
            ("assetId" Core..=) Prelude.<$> assetId
          ]
      )
