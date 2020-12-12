{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
  ( NielsenNonLinearWatermarkSettings (..),

    -- * Smart constructor
    mkNielsenNonLinearWatermarkSettings,

    -- * Lenses
    nnlwsEpisodeId,
    nnlwsActiveWatermarkProcess,
    nnlwsSourceId,
    nnlwsCbetSourceId,
    nnlwsTicServerURL,
    nnlwsMetadataDestination,
    nnlwsAssetName,
    nnlwsAdiFilename,
    nnlwsAssetId,
    nnlwsUniqueTicPerAudioTrack,
    nnlwsSourceWatermarkStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.NielsenActiveWatermarkProcessType
import Network.AWS.MediaConvert.Types.NielsenSourceWatermarkStatusType
import Network.AWS.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
import qualified Network.AWS.Prelude as Lude

-- | Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
--
-- /See:/ 'mkNielsenNonLinearWatermarkSettings' smart constructor.
data NielsenNonLinearWatermarkSettings = NielsenNonLinearWatermarkSettings'
  { episodeId ::
      Lude.Maybe Lude.Text,
    activeWatermarkProcess ::
      Lude.Maybe
        NielsenActiveWatermarkProcessType,
    sourceId ::
      Lude.Maybe Lude.Natural,
    cbetSourceId ::
      Lude.Maybe Lude.Text,
    ticServerURL ::
      Lude.Maybe Lude.Text,
    metadataDestination ::
      Lude.Maybe Lude.Text,
    assetName ::
      Lude.Maybe Lude.Text,
    adiFilename ::
      Lude.Maybe Lude.Text,
    assetId ::
      Lude.Maybe Lude.Text,
    uniqueTicPerAudioTrack ::
      Lude.Maybe
        NielsenUniqueTicPerAudioTrackType,
    sourceWatermarkStatus ::
      Lude.Maybe
        NielsenSourceWatermarkStatusType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NielsenNonLinearWatermarkSettings' with the minimum fields required to make a request.
--
-- * 'activeWatermarkProcess' - Choose the type of Nielsen watermarks that you want in your outputs. When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value for the setting SID (sourceId). When you choose CBET (CBET), you must provide a value for the setting CSID (cbetSourceId). When you choose NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values for both of these settings.
-- * 'adiFilename' - Optional. Use this setting when you want the service to include an ADI file in the Nielsen  metadata .zip file. To provide an ADI file, store it in Amazon S3 and provide a URL to it  here. The URL should be in the following format: S3://bucket/path/ADI-file. For more information about the metadata .zip file, see the setting Metadata destination (metadataDestination).
-- * 'assetId' - Use the asset ID that you provide to Nielsen to uniquely identify this asset. Required for all Nielsen non-linear watermarking.
-- * 'assetName' - Use the asset name that you provide to Nielsen for this asset. Required for all Nielsen non-linear watermarking.
-- * 'cbetSourceId' - Use the CSID that Nielsen provides to you. This CBET source ID should be unique to your Nielsen account but common to all of your output assets that have CBET watermarking. Required when you choose a value for the setting Watermark types (ActiveWatermarkProcess) that includes CBET.
-- * 'episodeId' - Optional. If this asset uses an episode ID with Nielsen, provide it here.
-- * 'metadataDestination' - Specify the Amazon S3 location where you want MediaConvert to save your Nielsen non-linear metadata .zip file. This Amazon S3 bucket must be in the same Region as the one where you do your MediaConvert transcoding. If you want to include an ADI file in this .zip file, use the setting ADI file (adiFilename) to specify it. MediaConvert delivers the Nielsen metadata .zip files only to your metadata destination Amazon S3 bucket. It doesn't deliver the .zip files to Nielsen. You are responsible for delivering the metadata .zip files to Nielsen.
-- * 'sourceId' - Use the SID that Nielsen provides to you. This source ID should be unique to your Nielsen account but common to all of your output assets. Required for all Nielsen non-linear watermarking. This ID should be unique to your Nielsen account but common to all of your output assets. Required for all Nielsen non-linear watermarking.
-- * 'sourceWatermarkStatus' - Required. Specify whether your source content already contains Nielsen non-linear watermarks. When you set this value to Watermarked (WATERMARKED), the service fails the job. Nielsen requires that you add non-linear watermarking to only clean content that doesn't already  have non-linear Nielsen watermarks.
-- * 'ticServerURL' - Specify the endpoint for the TIC server that you have deployed and configured in the AWS Cloud. Required for all Nielsen non-linear watermarking. MediaConvert can't connect directly to a TIC server. Instead, you must use API Gateway to provide a RESTful interface between MediaConvert and a TIC server that you deploy in your AWS account. For more information on deploying a TIC server in your AWS account and the required API Gateway, contact Nielsen support.
-- * 'uniqueTicPerAudioTrack' - To create assets that have the same TIC values in each audio track, keep the default value Share TICs (SAME_TICS_PER_TRACK). To create assets that have unique TIC values for each audio track, choose Use unique TICs (RESERVE_UNIQUE_TICS_PER_TRACK).
mkNielsenNonLinearWatermarkSettings ::
  NielsenNonLinearWatermarkSettings
mkNielsenNonLinearWatermarkSettings =
  NielsenNonLinearWatermarkSettings'
    { episodeId = Lude.Nothing,
      activeWatermarkProcess = Lude.Nothing,
      sourceId = Lude.Nothing,
      cbetSourceId = Lude.Nothing,
      ticServerURL = Lude.Nothing,
      metadataDestination = Lude.Nothing,
      assetName = Lude.Nothing,
      adiFilename = Lude.Nothing,
      assetId = Lude.Nothing,
      uniqueTicPerAudioTrack = Lude.Nothing,
      sourceWatermarkStatus = Lude.Nothing
    }

-- | Optional. If this asset uses an episode ID with Nielsen, provide it here.
--
-- /Note:/ Consider using 'episodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsEpisodeId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Lude.Maybe Lude.Text)
nnlwsEpisodeId = Lens.lens (episodeId :: NielsenNonLinearWatermarkSettings -> Lude.Maybe Lude.Text) (\s a -> s {episodeId = a} :: NielsenNonLinearWatermarkSettings)
{-# DEPRECATED nnlwsEpisodeId "Use generic-lens or generic-optics with 'episodeId' instead." #-}

-- | Choose the type of Nielsen watermarks that you want in your outputs. When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value for the setting SID (sourceId). When you choose CBET (CBET), you must provide a value for the setting CSID (cbetSourceId). When you choose NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values for both of these settings.
--
-- /Note:/ Consider using 'activeWatermarkProcess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsActiveWatermarkProcess :: Lens.Lens' NielsenNonLinearWatermarkSettings (Lude.Maybe NielsenActiveWatermarkProcessType)
nnlwsActiveWatermarkProcess = Lens.lens (activeWatermarkProcess :: NielsenNonLinearWatermarkSettings -> Lude.Maybe NielsenActiveWatermarkProcessType) (\s a -> s {activeWatermarkProcess = a} :: NielsenNonLinearWatermarkSettings)
{-# DEPRECATED nnlwsActiveWatermarkProcess "Use generic-lens or generic-optics with 'activeWatermarkProcess' instead." #-}

-- | Use the SID that Nielsen provides to you. This source ID should be unique to your Nielsen account but common to all of your output assets. Required for all Nielsen non-linear watermarking. This ID should be unique to your Nielsen account but common to all of your output assets. Required for all Nielsen non-linear watermarking.
--
-- /Note:/ Consider using 'sourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsSourceId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Lude.Maybe Lude.Natural)
nnlwsSourceId = Lens.lens (sourceId :: NielsenNonLinearWatermarkSettings -> Lude.Maybe Lude.Natural) (\s a -> s {sourceId = a} :: NielsenNonLinearWatermarkSettings)
{-# DEPRECATED nnlwsSourceId "Use generic-lens or generic-optics with 'sourceId' instead." #-}

-- | Use the CSID that Nielsen provides to you. This CBET source ID should be unique to your Nielsen account but common to all of your output assets that have CBET watermarking. Required when you choose a value for the setting Watermark types (ActiveWatermarkProcess) that includes CBET.
--
-- /Note:/ Consider using 'cbetSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsCbetSourceId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Lude.Maybe Lude.Text)
nnlwsCbetSourceId = Lens.lens (cbetSourceId :: NielsenNonLinearWatermarkSettings -> Lude.Maybe Lude.Text) (\s a -> s {cbetSourceId = a} :: NielsenNonLinearWatermarkSettings)
{-# DEPRECATED nnlwsCbetSourceId "Use generic-lens or generic-optics with 'cbetSourceId' instead." #-}

-- | Specify the endpoint for the TIC server that you have deployed and configured in the AWS Cloud. Required for all Nielsen non-linear watermarking. MediaConvert can't connect directly to a TIC server. Instead, you must use API Gateway to provide a RESTful interface between MediaConvert and a TIC server that you deploy in your AWS account. For more information on deploying a TIC server in your AWS account and the required API Gateway, contact Nielsen support.
--
-- /Note:/ Consider using 'ticServerURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsTicServerURL :: Lens.Lens' NielsenNonLinearWatermarkSettings (Lude.Maybe Lude.Text)
nnlwsTicServerURL = Lens.lens (ticServerURL :: NielsenNonLinearWatermarkSettings -> Lude.Maybe Lude.Text) (\s a -> s {ticServerURL = a} :: NielsenNonLinearWatermarkSettings)
{-# DEPRECATED nnlwsTicServerURL "Use generic-lens or generic-optics with 'ticServerURL' instead." #-}

-- | Specify the Amazon S3 location where you want MediaConvert to save your Nielsen non-linear metadata .zip file. This Amazon S3 bucket must be in the same Region as the one where you do your MediaConvert transcoding. If you want to include an ADI file in this .zip file, use the setting ADI file (adiFilename) to specify it. MediaConvert delivers the Nielsen metadata .zip files only to your metadata destination Amazon S3 bucket. It doesn't deliver the .zip files to Nielsen. You are responsible for delivering the metadata .zip files to Nielsen.
--
-- /Note:/ Consider using 'metadataDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsMetadataDestination :: Lens.Lens' NielsenNonLinearWatermarkSettings (Lude.Maybe Lude.Text)
nnlwsMetadataDestination = Lens.lens (metadataDestination :: NielsenNonLinearWatermarkSettings -> Lude.Maybe Lude.Text) (\s a -> s {metadataDestination = a} :: NielsenNonLinearWatermarkSettings)
{-# DEPRECATED nnlwsMetadataDestination "Use generic-lens or generic-optics with 'metadataDestination' instead." #-}

-- | Use the asset name that you provide to Nielsen for this asset. Required for all Nielsen non-linear watermarking.
--
-- /Note:/ Consider using 'assetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsAssetName :: Lens.Lens' NielsenNonLinearWatermarkSettings (Lude.Maybe Lude.Text)
nnlwsAssetName = Lens.lens (assetName :: NielsenNonLinearWatermarkSettings -> Lude.Maybe Lude.Text) (\s a -> s {assetName = a} :: NielsenNonLinearWatermarkSettings)
{-# DEPRECATED nnlwsAssetName "Use generic-lens or generic-optics with 'assetName' instead." #-}

-- | Optional. Use this setting when you want the service to include an ADI file in the Nielsen  metadata .zip file. To provide an ADI file, store it in Amazon S3 and provide a URL to it  here. The URL should be in the following format: S3://bucket/path/ADI-file. For more information about the metadata .zip file, see the setting Metadata destination (metadataDestination).
--
-- /Note:/ Consider using 'adiFilename' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsAdiFilename :: Lens.Lens' NielsenNonLinearWatermarkSettings (Lude.Maybe Lude.Text)
nnlwsAdiFilename = Lens.lens (adiFilename :: NielsenNonLinearWatermarkSettings -> Lude.Maybe Lude.Text) (\s a -> s {adiFilename = a} :: NielsenNonLinearWatermarkSettings)
{-# DEPRECATED nnlwsAdiFilename "Use generic-lens or generic-optics with 'adiFilename' instead." #-}

-- | Use the asset ID that you provide to Nielsen to uniquely identify this asset. Required for all Nielsen non-linear watermarking.
--
-- /Note:/ Consider using 'assetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsAssetId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Lude.Maybe Lude.Text)
nnlwsAssetId = Lens.lens (assetId :: NielsenNonLinearWatermarkSettings -> Lude.Maybe Lude.Text) (\s a -> s {assetId = a} :: NielsenNonLinearWatermarkSettings)
{-# DEPRECATED nnlwsAssetId "Use generic-lens or generic-optics with 'assetId' instead." #-}

-- | To create assets that have the same TIC values in each audio track, keep the default value Share TICs (SAME_TICS_PER_TRACK). To create assets that have unique TIC values for each audio track, choose Use unique TICs (RESERVE_UNIQUE_TICS_PER_TRACK).
--
-- /Note:/ Consider using 'uniqueTicPerAudioTrack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsUniqueTicPerAudioTrack :: Lens.Lens' NielsenNonLinearWatermarkSettings (Lude.Maybe NielsenUniqueTicPerAudioTrackType)
nnlwsUniqueTicPerAudioTrack = Lens.lens (uniqueTicPerAudioTrack :: NielsenNonLinearWatermarkSettings -> Lude.Maybe NielsenUniqueTicPerAudioTrackType) (\s a -> s {uniqueTicPerAudioTrack = a} :: NielsenNonLinearWatermarkSettings)
{-# DEPRECATED nnlwsUniqueTicPerAudioTrack "Use generic-lens or generic-optics with 'uniqueTicPerAudioTrack' instead." #-}

-- | Required. Specify whether your source content already contains Nielsen non-linear watermarks. When you set this value to Watermarked (WATERMARKED), the service fails the job. Nielsen requires that you add non-linear watermarking to only clean content that doesn't already  have non-linear Nielsen watermarks.
--
-- /Note:/ Consider using 'sourceWatermarkStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsSourceWatermarkStatus :: Lens.Lens' NielsenNonLinearWatermarkSettings (Lude.Maybe NielsenSourceWatermarkStatusType)
nnlwsSourceWatermarkStatus = Lens.lens (sourceWatermarkStatus :: NielsenNonLinearWatermarkSettings -> Lude.Maybe NielsenSourceWatermarkStatusType) (\s a -> s {sourceWatermarkStatus = a} :: NielsenNonLinearWatermarkSettings)
{-# DEPRECATED nnlwsSourceWatermarkStatus "Use generic-lens or generic-optics with 'sourceWatermarkStatus' instead." #-}

instance Lude.FromJSON NielsenNonLinearWatermarkSettings where
  parseJSON =
    Lude.withObject
      "NielsenNonLinearWatermarkSettings"
      ( \x ->
          NielsenNonLinearWatermarkSettings'
            Lude.<$> (x Lude..:? "episodeId")
            Lude.<*> (x Lude..:? "activeWatermarkProcess")
            Lude.<*> (x Lude..:? "sourceId")
            Lude.<*> (x Lude..:? "cbetSourceId")
            Lude.<*> (x Lude..:? "ticServerUrl")
            Lude.<*> (x Lude..:? "metadataDestination")
            Lude.<*> (x Lude..:? "assetName")
            Lude.<*> (x Lude..:? "adiFilename")
            Lude.<*> (x Lude..:? "assetId")
            Lude.<*> (x Lude..:? "uniqueTicPerAudioTrack")
            Lude.<*> (x Lude..:? "sourceWatermarkStatus")
      )

instance Lude.ToJSON NielsenNonLinearWatermarkSettings where
  toJSON NielsenNonLinearWatermarkSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("episodeId" Lude..=) Lude.<$> episodeId,
            ("activeWatermarkProcess" Lude..=) Lude.<$> activeWatermarkProcess,
            ("sourceId" Lude..=) Lude.<$> sourceId,
            ("cbetSourceId" Lude..=) Lude.<$> cbetSourceId,
            ("ticServerUrl" Lude..=) Lude.<$> ticServerURL,
            ("metadataDestination" Lude..=) Lude.<$> metadataDestination,
            ("assetName" Lude..=) Lude.<$> assetName,
            ("adiFilename" Lude..=) Lude.<$> adiFilename,
            ("assetId" Lude..=) Lude.<$> assetId,
            ("uniqueTicPerAudioTrack" Lude..=) Lude.<$> uniqueTicPerAudioTrack,
            ("sourceWatermarkStatus" Lude..=) Lude.<$> sourceWatermarkStatus
          ]
      )
