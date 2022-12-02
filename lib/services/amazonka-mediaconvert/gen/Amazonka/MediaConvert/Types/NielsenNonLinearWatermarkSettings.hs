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
-- Module      : Amazonka.MediaConvert.Types.NielsenNonLinearWatermarkSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.NielsenNonLinearWatermarkSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.NielsenActiveWatermarkProcessType
import Amazonka.MediaConvert.Types.NielsenSourceWatermarkStatusType
import Amazonka.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
import qualified Amazonka.Prelude as Prelude

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
  { -- | Specify the endpoint for the TIC server that you have deployed and
    -- configured in the AWS Cloud. Required for all Nielsen non-linear
    -- watermarking. MediaConvert can\'t connect directly to a TIC server.
    -- Instead, you must use API Gateway to provide a RESTful interface between
    -- MediaConvert and a TIC server that you deploy in your AWS account. For
    -- more information on deploying a TIC server in your AWS account and the
    -- required API Gateway, contact Nielsen support.
    ticServerUrl :: Prelude.Maybe Prelude.Text,
    -- | Required. Specify whether your source content already contains Nielsen
    -- non-linear watermarks. When you set this value to Watermarked
    -- (WATERMARKED), the service fails the job. Nielsen requires that you add
    -- non-linear watermarking to only clean content that doesn\'t already have
    -- non-linear Nielsen watermarks.
    sourceWatermarkStatus :: Prelude.Maybe NielsenSourceWatermarkStatusType,
    -- | Use the asset ID that you provide to Nielsen to uniquely identify this
    -- asset. Required for all Nielsen non-linear watermarking.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | Use the SID that Nielsen provides to you. This source ID should be
    -- unique to your Nielsen account but common to all of your output assets.
    -- Required for all Nielsen non-linear watermarking. This ID should be
    -- unique to your Nielsen account but common to all of your output assets.
    -- Required for all Nielsen non-linear watermarking.
    sourceId :: Prelude.Maybe Prelude.Natural,
    -- | Use the asset name that you provide to Nielsen for this asset. Required
    -- for all Nielsen non-linear watermarking.
    assetName :: Prelude.Maybe Prelude.Text,
    -- | Optional. If this asset uses an episode ID with Nielsen, provide it
    -- here.
    episodeId :: Prelude.Maybe Prelude.Text,
    -- | Choose the type of Nielsen watermarks that you want in your outputs.
    -- When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value
    -- for the setting SID (sourceId). When you choose CBET (CBET), you must
    -- provide a value for the setting CSID (cbetSourceId). When you choose
    -- NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values
    -- for both of these settings.
    activeWatermarkProcess :: Prelude.Maybe NielsenActiveWatermarkProcessType,
    -- | To create assets that have the same TIC values in each audio track, keep
    -- the default value Share TICs (SAME_TICS_PER_TRACK). To create assets
    -- that have unique TIC values for each audio track, choose Use unique TICs
    -- (RESERVE_UNIQUE_TICS_PER_TRACK).
    uniqueTicPerAudioTrack :: Prelude.Maybe NielsenUniqueTicPerAudioTrackType,
    -- | Use the CSID that Nielsen provides to you. This CBET source ID should be
    -- unique to your Nielsen account but common to all of your output assets
    -- that have CBET watermarking. Required when you choose a value for the
    -- setting Watermark types (ActiveWatermarkProcess) that includes CBET.
    cbetSourceId :: Prelude.Maybe Prelude.Text,
    -- | Optional. Use this setting when you want the service to include an ADI
    -- file in the Nielsen metadata .zip file. To provide an ADI file, store it
    -- in Amazon S3 and provide a URL to it here. The URL should be in the
    -- following format: S3:\/\/bucket\/path\/ADI-file. For more information
    -- about the metadata .zip file, see the setting Metadata destination
    -- (metadataDestination).
    adiFilename :: Prelude.Maybe Prelude.Text,
    -- | Specify the Amazon S3 location where you want MediaConvert to save your
    -- Nielsen non-linear metadata .zip file. This Amazon S3 bucket must be in
    -- the same Region as the one where you do your MediaConvert transcoding.
    -- If you want to include an ADI file in this .zip file, use the setting
    -- ADI file (adiFilename) to specify it. MediaConvert delivers the Nielsen
    -- metadata .zip files only to your metadata destination Amazon S3 bucket.
    -- It doesn\'t deliver the .zip files to Nielsen. You are responsible for
    -- delivering the metadata .zip files to Nielsen.
    metadataDestination :: Prelude.Maybe Prelude.Text
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
-- 'ticServerUrl', 'nielsenNonLinearWatermarkSettings_ticServerUrl' - Specify the endpoint for the TIC server that you have deployed and
-- configured in the AWS Cloud. Required for all Nielsen non-linear
-- watermarking. MediaConvert can\'t connect directly to a TIC server.
-- Instead, you must use API Gateway to provide a RESTful interface between
-- MediaConvert and a TIC server that you deploy in your AWS account. For
-- more information on deploying a TIC server in your AWS account and the
-- required API Gateway, contact Nielsen support.
--
-- 'sourceWatermarkStatus', 'nielsenNonLinearWatermarkSettings_sourceWatermarkStatus' - Required. Specify whether your source content already contains Nielsen
-- non-linear watermarks. When you set this value to Watermarked
-- (WATERMARKED), the service fails the job. Nielsen requires that you add
-- non-linear watermarking to only clean content that doesn\'t already have
-- non-linear Nielsen watermarks.
--
-- 'assetId', 'nielsenNonLinearWatermarkSettings_assetId' - Use the asset ID that you provide to Nielsen to uniquely identify this
-- asset. Required for all Nielsen non-linear watermarking.
--
-- 'sourceId', 'nielsenNonLinearWatermarkSettings_sourceId' - Use the SID that Nielsen provides to you. This source ID should be
-- unique to your Nielsen account but common to all of your output assets.
-- Required for all Nielsen non-linear watermarking. This ID should be
-- unique to your Nielsen account but common to all of your output assets.
-- Required for all Nielsen non-linear watermarking.
--
-- 'assetName', 'nielsenNonLinearWatermarkSettings_assetName' - Use the asset name that you provide to Nielsen for this asset. Required
-- for all Nielsen non-linear watermarking.
--
-- 'episodeId', 'nielsenNonLinearWatermarkSettings_episodeId' - Optional. If this asset uses an episode ID with Nielsen, provide it
-- here.
--
-- 'activeWatermarkProcess', 'nielsenNonLinearWatermarkSettings_activeWatermarkProcess' - Choose the type of Nielsen watermarks that you want in your outputs.
-- When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value
-- for the setting SID (sourceId). When you choose CBET (CBET), you must
-- provide a value for the setting CSID (cbetSourceId). When you choose
-- NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values
-- for both of these settings.
--
-- 'uniqueTicPerAudioTrack', 'nielsenNonLinearWatermarkSettings_uniqueTicPerAudioTrack' - To create assets that have the same TIC values in each audio track, keep
-- the default value Share TICs (SAME_TICS_PER_TRACK). To create assets
-- that have unique TIC values for each audio track, choose Use unique TICs
-- (RESERVE_UNIQUE_TICS_PER_TRACK).
--
-- 'cbetSourceId', 'nielsenNonLinearWatermarkSettings_cbetSourceId' - Use the CSID that Nielsen provides to you. This CBET source ID should be
-- unique to your Nielsen account but common to all of your output assets
-- that have CBET watermarking. Required when you choose a value for the
-- setting Watermark types (ActiveWatermarkProcess) that includes CBET.
--
-- 'adiFilename', 'nielsenNonLinearWatermarkSettings_adiFilename' - Optional. Use this setting when you want the service to include an ADI
-- file in the Nielsen metadata .zip file. To provide an ADI file, store it
-- in Amazon S3 and provide a URL to it here. The URL should be in the
-- following format: S3:\/\/bucket\/path\/ADI-file. For more information
-- about the metadata .zip file, see the setting Metadata destination
-- (metadataDestination).
--
-- 'metadataDestination', 'nielsenNonLinearWatermarkSettings_metadataDestination' - Specify the Amazon S3 location where you want MediaConvert to save your
-- Nielsen non-linear metadata .zip file. This Amazon S3 bucket must be in
-- the same Region as the one where you do your MediaConvert transcoding.
-- If you want to include an ADI file in this .zip file, use the setting
-- ADI file (adiFilename) to specify it. MediaConvert delivers the Nielsen
-- metadata .zip files only to your metadata destination Amazon S3 bucket.
-- It doesn\'t deliver the .zip files to Nielsen. You are responsible for
-- delivering the metadata .zip files to Nielsen.
newNielsenNonLinearWatermarkSettings ::
  NielsenNonLinearWatermarkSettings
newNielsenNonLinearWatermarkSettings =
  NielsenNonLinearWatermarkSettings'
    { ticServerUrl =
        Prelude.Nothing,
      sourceWatermarkStatus = Prelude.Nothing,
      assetId = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      assetName = Prelude.Nothing,
      episodeId = Prelude.Nothing,
      activeWatermarkProcess = Prelude.Nothing,
      uniqueTicPerAudioTrack = Prelude.Nothing,
      cbetSourceId = Prelude.Nothing,
      adiFilename = Prelude.Nothing,
      metadataDestination = Prelude.Nothing
    }

-- | Specify the endpoint for the TIC server that you have deployed and
-- configured in the AWS Cloud. Required for all Nielsen non-linear
-- watermarking. MediaConvert can\'t connect directly to a TIC server.
-- Instead, you must use API Gateway to provide a RESTful interface between
-- MediaConvert and a TIC server that you deploy in your AWS account. For
-- more information on deploying a TIC server in your AWS account and the
-- required API Gateway, contact Nielsen support.
nielsenNonLinearWatermarkSettings_ticServerUrl :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_ticServerUrl = Lens.lens (\NielsenNonLinearWatermarkSettings' {ticServerUrl} -> ticServerUrl) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {ticServerUrl = a} :: NielsenNonLinearWatermarkSettings)

-- | Required. Specify whether your source content already contains Nielsen
-- non-linear watermarks. When you set this value to Watermarked
-- (WATERMARKED), the service fails the job. Nielsen requires that you add
-- non-linear watermarking to only clean content that doesn\'t already have
-- non-linear Nielsen watermarks.
nielsenNonLinearWatermarkSettings_sourceWatermarkStatus :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe NielsenSourceWatermarkStatusType)
nielsenNonLinearWatermarkSettings_sourceWatermarkStatus = Lens.lens (\NielsenNonLinearWatermarkSettings' {sourceWatermarkStatus} -> sourceWatermarkStatus) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {sourceWatermarkStatus = a} :: NielsenNonLinearWatermarkSettings)

-- | Use the asset ID that you provide to Nielsen to uniquely identify this
-- asset. Required for all Nielsen non-linear watermarking.
nielsenNonLinearWatermarkSettings_assetId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_assetId = Lens.lens (\NielsenNonLinearWatermarkSettings' {assetId} -> assetId) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {assetId = a} :: NielsenNonLinearWatermarkSettings)

-- | Use the SID that Nielsen provides to you. This source ID should be
-- unique to your Nielsen account but common to all of your output assets.
-- Required for all Nielsen non-linear watermarking. This ID should be
-- unique to your Nielsen account but common to all of your output assets.
-- Required for all Nielsen non-linear watermarking.
nielsenNonLinearWatermarkSettings_sourceId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Natural)
nielsenNonLinearWatermarkSettings_sourceId = Lens.lens (\NielsenNonLinearWatermarkSettings' {sourceId} -> sourceId) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {sourceId = a} :: NielsenNonLinearWatermarkSettings)

-- | Use the asset name that you provide to Nielsen for this asset. Required
-- for all Nielsen non-linear watermarking.
nielsenNonLinearWatermarkSettings_assetName :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_assetName = Lens.lens (\NielsenNonLinearWatermarkSettings' {assetName} -> assetName) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {assetName = a} :: NielsenNonLinearWatermarkSettings)

-- | Optional. If this asset uses an episode ID with Nielsen, provide it
-- here.
nielsenNonLinearWatermarkSettings_episodeId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_episodeId = Lens.lens (\NielsenNonLinearWatermarkSettings' {episodeId} -> episodeId) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {episodeId = a} :: NielsenNonLinearWatermarkSettings)

-- | Choose the type of Nielsen watermarks that you want in your outputs.
-- When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value
-- for the setting SID (sourceId). When you choose CBET (CBET), you must
-- provide a value for the setting CSID (cbetSourceId). When you choose
-- NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values
-- for both of these settings.
nielsenNonLinearWatermarkSettings_activeWatermarkProcess :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe NielsenActiveWatermarkProcessType)
nielsenNonLinearWatermarkSettings_activeWatermarkProcess = Lens.lens (\NielsenNonLinearWatermarkSettings' {activeWatermarkProcess} -> activeWatermarkProcess) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {activeWatermarkProcess = a} :: NielsenNonLinearWatermarkSettings)

-- | To create assets that have the same TIC values in each audio track, keep
-- the default value Share TICs (SAME_TICS_PER_TRACK). To create assets
-- that have unique TIC values for each audio track, choose Use unique TICs
-- (RESERVE_UNIQUE_TICS_PER_TRACK).
nielsenNonLinearWatermarkSettings_uniqueTicPerAudioTrack :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe NielsenUniqueTicPerAudioTrackType)
nielsenNonLinearWatermarkSettings_uniqueTicPerAudioTrack = Lens.lens (\NielsenNonLinearWatermarkSettings' {uniqueTicPerAudioTrack} -> uniqueTicPerAudioTrack) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {uniqueTicPerAudioTrack = a} :: NielsenNonLinearWatermarkSettings)

-- | Use the CSID that Nielsen provides to you. This CBET source ID should be
-- unique to your Nielsen account but common to all of your output assets
-- that have CBET watermarking. Required when you choose a value for the
-- setting Watermark types (ActiveWatermarkProcess) that includes CBET.
nielsenNonLinearWatermarkSettings_cbetSourceId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_cbetSourceId = Lens.lens (\NielsenNonLinearWatermarkSettings' {cbetSourceId} -> cbetSourceId) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {cbetSourceId = a} :: NielsenNonLinearWatermarkSettings)

-- | Optional. Use this setting when you want the service to include an ADI
-- file in the Nielsen metadata .zip file. To provide an ADI file, store it
-- in Amazon S3 and provide a URL to it here. The URL should be in the
-- following format: S3:\/\/bucket\/path\/ADI-file. For more information
-- about the metadata .zip file, see the setting Metadata destination
-- (metadataDestination).
nielsenNonLinearWatermarkSettings_adiFilename :: Lens.Lens' NielsenNonLinearWatermarkSettings (Prelude.Maybe Prelude.Text)
nielsenNonLinearWatermarkSettings_adiFilename = Lens.lens (\NielsenNonLinearWatermarkSettings' {adiFilename} -> adiFilename) (\s@NielsenNonLinearWatermarkSettings' {} a -> s {adiFilename = a} :: NielsenNonLinearWatermarkSettings)

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

instance
  Data.FromJSON
    NielsenNonLinearWatermarkSettings
  where
  parseJSON =
    Data.withObject
      "NielsenNonLinearWatermarkSettings"
      ( \x ->
          NielsenNonLinearWatermarkSettings'
            Prelude.<$> (x Data..:? "ticServerUrl")
            Prelude.<*> (x Data..:? "sourceWatermarkStatus")
            Prelude.<*> (x Data..:? "assetId")
            Prelude.<*> (x Data..:? "sourceId")
            Prelude.<*> (x Data..:? "assetName")
            Prelude.<*> (x Data..:? "episodeId")
            Prelude.<*> (x Data..:? "activeWatermarkProcess")
            Prelude.<*> (x Data..:? "uniqueTicPerAudioTrack")
            Prelude.<*> (x Data..:? "cbetSourceId")
            Prelude.<*> (x Data..:? "adiFilename")
            Prelude.<*> (x Data..:? "metadataDestination")
      )

instance
  Prelude.Hashable
    NielsenNonLinearWatermarkSettings
  where
  hashWithSalt
    _salt
    NielsenNonLinearWatermarkSettings' {..} =
      _salt `Prelude.hashWithSalt` ticServerUrl
        `Prelude.hashWithSalt` sourceWatermarkStatus
        `Prelude.hashWithSalt` assetId
        `Prelude.hashWithSalt` sourceId
        `Prelude.hashWithSalt` assetName
        `Prelude.hashWithSalt` episodeId
        `Prelude.hashWithSalt` activeWatermarkProcess
        `Prelude.hashWithSalt` uniqueTicPerAudioTrack
        `Prelude.hashWithSalt` cbetSourceId
        `Prelude.hashWithSalt` adiFilename
        `Prelude.hashWithSalt` metadataDestination

instance
  Prelude.NFData
    NielsenNonLinearWatermarkSettings
  where
  rnf NielsenNonLinearWatermarkSettings' {..} =
    Prelude.rnf ticServerUrl
      `Prelude.seq` Prelude.rnf sourceWatermarkStatus
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf assetName
      `Prelude.seq` Prelude.rnf episodeId
      `Prelude.seq` Prelude.rnf activeWatermarkProcess
      `Prelude.seq` Prelude.rnf uniqueTicPerAudioTrack
      `Prelude.seq` Prelude.rnf cbetSourceId
      `Prelude.seq` Prelude.rnf adiFilename
      `Prelude.seq` Prelude.rnf metadataDestination

instance
  Data.ToJSON
    NielsenNonLinearWatermarkSettings
  where
  toJSON NielsenNonLinearWatermarkSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ticServerUrl" Data..=) Prelude.<$> ticServerUrl,
            ("sourceWatermarkStatus" Data..=)
              Prelude.<$> sourceWatermarkStatus,
            ("assetId" Data..=) Prelude.<$> assetId,
            ("sourceId" Data..=) Prelude.<$> sourceId,
            ("assetName" Data..=) Prelude.<$> assetName,
            ("episodeId" Data..=) Prelude.<$> episodeId,
            ("activeWatermarkProcess" Data..=)
              Prelude.<$> activeWatermarkProcess,
            ("uniqueTicPerAudioTrack" Data..=)
              Prelude.<$> uniqueTicPerAudioTrack,
            ("cbetSourceId" Data..=) Prelude.<$> cbetSourceId,
            ("adiFilename" Data..=) Prelude.<$> adiFilename,
            ("metadataDestination" Data..=)
              Prelude.<$> metadataDestination
          ]
      )
