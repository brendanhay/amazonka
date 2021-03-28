{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
  ( NielsenNonLinearWatermarkSettings (..)
  -- * Smart constructor
  , mkNielsenNonLinearWatermarkSettings
  -- * Lenses
  , nnlwsActiveWatermarkProcess
  , nnlwsAdiFilename
  , nnlwsAssetId
  , nnlwsAssetName
  , nnlwsCbetSourceId
  , nnlwsEpisodeId
  , nnlwsMetadataDestination
  , nnlwsSourceId
  , nnlwsSourceWatermarkStatus
  , nnlwsTicServerUrl
  , nnlwsUniqueTicPerAudioTrack
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.NielsenActiveWatermarkProcessType as Types
import qualified Network.AWS.MediaConvert.Types.NielsenSourceWatermarkStatusType as Types
import qualified Network.AWS.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType as Types
import qualified Network.AWS.Prelude as Core

-- | Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
--
-- /See:/ 'mkNielsenNonLinearWatermarkSettings' smart constructor.
data NielsenNonLinearWatermarkSettings = NielsenNonLinearWatermarkSettings'
  { activeWatermarkProcess :: Core.Maybe Types.NielsenActiveWatermarkProcessType
    -- ^ Choose the type of Nielsen watermarks that you want in your outputs. When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value for the setting SID (sourceId). When you choose CBET (CBET), you must provide a value for the setting CSID (cbetSourceId). When you choose NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values for both of these settings.
  , adiFilename :: Core.Maybe Core.Text
    -- ^ Optional. Use this setting when you want the service to include an ADI file in the Nielsen  metadata .zip file. To provide an ADI file, store it in Amazon S3 and provide a URL to it  here. The URL should be in the following format: S3://bucket/path/ADI-file. For more information about the metadata .zip file, see the setting Metadata destination (metadataDestination).
  , assetId :: Core.Maybe Core.Text
    -- ^ Use the asset ID that you provide to Nielsen to uniquely identify this asset. Required for all Nielsen non-linear watermarking.
  , assetName :: Core.Maybe Core.Text
    -- ^ Use the asset name that you provide to Nielsen for this asset. Required for all Nielsen non-linear watermarking.
  , cbetSourceId :: Core.Maybe Core.Text
    -- ^ Use the CSID that Nielsen provides to you. This CBET source ID should be unique to your Nielsen account but common to all of your output assets that have CBET watermarking. Required when you choose a value for the setting Watermark types (ActiveWatermarkProcess) that includes CBET.
  , episodeId :: Core.Maybe Core.Text
    -- ^ Optional. If this asset uses an episode ID with Nielsen, provide it here.
  , metadataDestination :: Core.Maybe Core.Text
    -- ^ Specify the Amazon S3 location where you want MediaConvert to save your Nielsen non-linear metadata .zip file. This Amazon S3 bucket must be in the same Region as the one where you do your MediaConvert transcoding. If you want to include an ADI file in this .zip file, use the setting ADI file (adiFilename) to specify it. MediaConvert delivers the Nielsen metadata .zip files only to your metadata destination Amazon S3 bucket. It doesn't deliver the .zip files to Nielsen. You are responsible for delivering the metadata .zip files to Nielsen.
  , sourceId :: Core.Maybe Core.Natural
    -- ^ Use the SID that Nielsen provides to you. This source ID should be unique to your Nielsen account but common to all of your output assets. Required for all Nielsen non-linear watermarking. This ID should be unique to your Nielsen account but common to all of your output assets. Required for all Nielsen non-linear watermarking.
  , sourceWatermarkStatus :: Core.Maybe Types.NielsenSourceWatermarkStatusType
    -- ^ Required. Specify whether your source content already contains Nielsen non-linear watermarks. When you set this value to Watermarked (WATERMARKED), the service fails the job. Nielsen requires that you add non-linear watermarking to only clean content that doesn't already  have non-linear Nielsen watermarks.
  , ticServerUrl :: Core.Maybe Core.Text
    -- ^ Specify the endpoint for the TIC server that you have deployed and configured in the AWS Cloud. Required for all Nielsen non-linear watermarking. MediaConvert can't connect directly to a TIC server. Instead, you must use API Gateway to provide a RESTful interface between MediaConvert and a TIC server that you deploy in your AWS account. For more information on deploying a TIC server in your AWS account and the required API Gateway, contact Nielsen support.
  , uniqueTicPerAudioTrack :: Core.Maybe Types.NielsenUniqueTicPerAudioTrackType
    -- ^ To create assets that have the same TIC values in each audio track, keep the default value Share TICs (SAME_TICS_PER_TRACK). To create assets that have unique TIC values for each audio track, choose Use unique TICs (RESERVE_UNIQUE_TICS_PER_TRACK).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NielsenNonLinearWatermarkSettings' value with any optional fields omitted.
mkNielsenNonLinearWatermarkSettings
    :: NielsenNonLinearWatermarkSettings
mkNielsenNonLinearWatermarkSettings
  = NielsenNonLinearWatermarkSettings'{activeWatermarkProcess =
                                         Core.Nothing,
                                       adiFilename = Core.Nothing, assetId = Core.Nothing,
                                       assetName = Core.Nothing, cbetSourceId = Core.Nothing,
                                       episodeId = Core.Nothing, metadataDestination = Core.Nothing,
                                       sourceId = Core.Nothing,
                                       sourceWatermarkStatus = Core.Nothing,
                                       ticServerUrl = Core.Nothing,
                                       uniqueTicPerAudioTrack = Core.Nothing}

-- | Choose the type of Nielsen watermarks that you want in your outputs. When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value for the setting SID (sourceId). When you choose CBET (CBET), you must provide a value for the setting CSID (cbetSourceId). When you choose NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values for both of these settings.
--
-- /Note:/ Consider using 'activeWatermarkProcess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsActiveWatermarkProcess :: Lens.Lens' NielsenNonLinearWatermarkSettings (Core.Maybe Types.NielsenActiveWatermarkProcessType)
nnlwsActiveWatermarkProcess = Lens.field @"activeWatermarkProcess"
{-# INLINEABLE nnlwsActiveWatermarkProcess #-}
{-# DEPRECATED activeWatermarkProcess "Use generic-lens or generic-optics with 'activeWatermarkProcess' instead"  #-}

-- | Optional. Use this setting when you want the service to include an ADI file in the Nielsen  metadata .zip file. To provide an ADI file, store it in Amazon S3 and provide a URL to it  here. The URL should be in the following format: S3://bucket/path/ADI-file. For more information about the metadata .zip file, see the setting Metadata destination (metadataDestination).
--
-- /Note:/ Consider using 'adiFilename' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsAdiFilename :: Lens.Lens' NielsenNonLinearWatermarkSettings (Core.Maybe Core.Text)
nnlwsAdiFilename = Lens.field @"adiFilename"
{-# INLINEABLE nnlwsAdiFilename #-}
{-# DEPRECATED adiFilename "Use generic-lens or generic-optics with 'adiFilename' instead"  #-}

-- | Use the asset ID that you provide to Nielsen to uniquely identify this asset. Required for all Nielsen non-linear watermarking.
--
-- /Note:/ Consider using 'assetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsAssetId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Core.Maybe Core.Text)
nnlwsAssetId = Lens.field @"assetId"
{-# INLINEABLE nnlwsAssetId #-}
{-# DEPRECATED assetId "Use generic-lens or generic-optics with 'assetId' instead"  #-}

-- | Use the asset name that you provide to Nielsen for this asset. Required for all Nielsen non-linear watermarking.
--
-- /Note:/ Consider using 'assetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsAssetName :: Lens.Lens' NielsenNonLinearWatermarkSettings (Core.Maybe Core.Text)
nnlwsAssetName = Lens.field @"assetName"
{-# INLINEABLE nnlwsAssetName #-}
{-# DEPRECATED assetName "Use generic-lens or generic-optics with 'assetName' instead"  #-}

-- | Use the CSID that Nielsen provides to you. This CBET source ID should be unique to your Nielsen account but common to all of your output assets that have CBET watermarking. Required when you choose a value for the setting Watermark types (ActiveWatermarkProcess) that includes CBET.
--
-- /Note:/ Consider using 'cbetSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsCbetSourceId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Core.Maybe Core.Text)
nnlwsCbetSourceId = Lens.field @"cbetSourceId"
{-# INLINEABLE nnlwsCbetSourceId #-}
{-# DEPRECATED cbetSourceId "Use generic-lens or generic-optics with 'cbetSourceId' instead"  #-}

-- | Optional. If this asset uses an episode ID with Nielsen, provide it here.
--
-- /Note:/ Consider using 'episodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsEpisodeId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Core.Maybe Core.Text)
nnlwsEpisodeId = Lens.field @"episodeId"
{-# INLINEABLE nnlwsEpisodeId #-}
{-# DEPRECATED episodeId "Use generic-lens or generic-optics with 'episodeId' instead"  #-}

-- | Specify the Amazon S3 location where you want MediaConvert to save your Nielsen non-linear metadata .zip file. This Amazon S3 bucket must be in the same Region as the one where you do your MediaConvert transcoding. If you want to include an ADI file in this .zip file, use the setting ADI file (adiFilename) to specify it. MediaConvert delivers the Nielsen metadata .zip files only to your metadata destination Amazon S3 bucket. It doesn't deliver the .zip files to Nielsen. You are responsible for delivering the metadata .zip files to Nielsen.
--
-- /Note:/ Consider using 'metadataDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsMetadataDestination :: Lens.Lens' NielsenNonLinearWatermarkSettings (Core.Maybe Core.Text)
nnlwsMetadataDestination = Lens.field @"metadataDestination"
{-# INLINEABLE nnlwsMetadataDestination #-}
{-# DEPRECATED metadataDestination "Use generic-lens or generic-optics with 'metadataDestination' instead"  #-}

-- | Use the SID that Nielsen provides to you. This source ID should be unique to your Nielsen account but common to all of your output assets. Required for all Nielsen non-linear watermarking. This ID should be unique to your Nielsen account but common to all of your output assets. Required for all Nielsen non-linear watermarking.
--
-- /Note:/ Consider using 'sourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsSourceId :: Lens.Lens' NielsenNonLinearWatermarkSettings (Core.Maybe Core.Natural)
nnlwsSourceId = Lens.field @"sourceId"
{-# INLINEABLE nnlwsSourceId #-}
{-# DEPRECATED sourceId "Use generic-lens or generic-optics with 'sourceId' instead"  #-}

-- | Required. Specify whether your source content already contains Nielsen non-linear watermarks. When you set this value to Watermarked (WATERMARKED), the service fails the job. Nielsen requires that you add non-linear watermarking to only clean content that doesn't already  have non-linear Nielsen watermarks.
--
-- /Note:/ Consider using 'sourceWatermarkStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsSourceWatermarkStatus :: Lens.Lens' NielsenNonLinearWatermarkSettings (Core.Maybe Types.NielsenSourceWatermarkStatusType)
nnlwsSourceWatermarkStatus = Lens.field @"sourceWatermarkStatus"
{-# INLINEABLE nnlwsSourceWatermarkStatus #-}
{-# DEPRECATED sourceWatermarkStatus "Use generic-lens or generic-optics with 'sourceWatermarkStatus' instead"  #-}

-- | Specify the endpoint for the TIC server that you have deployed and configured in the AWS Cloud. Required for all Nielsen non-linear watermarking. MediaConvert can't connect directly to a TIC server. Instead, you must use API Gateway to provide a RESTful interface between MediaConvert and a TIC server that you deploy in your AWS account. For more information on deploying a TIC server in your AWS account and the required API Gateway, contact Nielsen support.
--
-- /Note:/ Consider using 'ticServerUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsTicServerUrl :: Lens.Lens' NielsenNonLinearWatermarkSettings (Core.Maybe Core.Text)
nnlwsTicServerUrl = Lens.field @"ticServerUrl"
{-# INLINEABLE nnlwsTicServerUrl #-}
{-# DEPRECATED ticServerUrl "Use generic-lens or generic-optics with 'ticServerUrl' instead"  #-}

-- | To create assets that have the same TIC values in each audio track, keep the default value Share TICs (SAME_TICS_PER_TRACK). To create assets that have unique TIC values for each audio track, choose Use unique TICs (RESERVE_UNIQUE_TICS_PER_TRACK).
--
-- /Note:/ Consider using 'uniqueTicPerAudioTrack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nnlwsUniqueTicPerAudioTrack :: Lens.Lens' NielsenNonLinearWatermarkSettings (Core.Maybe Types.NielsenUniqueTicPerAudioTrackType)
nnlwsUniqueTicPerAudioTrack = Lens.field @"uniqueTicPerAudioTrack"
{-# INLINEABLE nnlwsUniqueTicPerAudioTrack #-}
{-# DEPRECATED uniqueTicPerAudioTrack "Use generic-lens or generic-optics with 'uniqueTicPerAudioTrack' instead"  #-}

instance Core.FromJSON NielsenNonLinearWatermarkSettings where
        toJSON NielsenNonLinearWatermarkSettings{..}
          = Core.object
              (Core.catMaybes
                 [("activeWatermarkProcess" Core..=) Core.<$>
                    activeWatermarkProcess,
                  ("adiFilename" Core..=) Core.<$> adiFilename,
                  ("assetId" Core..=) Core.<$> assetId,
                  ("assetName" Core..=) Core.<$> assetName,
                  ("cbetSourceId" Core..=) Core.<$> cbetSourceId,
                  ("episodeId" Core..=) Core.<$> episodeId,
                  ("metadataDestination" Core..=) Core.<$> metadataDestination,
                  ("sourceId" Core..=) Core.<$> sourceId,
                  ("sourceWatermarkStatus" Core..=) Core.<$> sourceWatermarkStatus,
                  ("ticServerUrl" Core..=) Core.<$> ticServerUrl,
                  ("uniqueTicPerAudioTrack" Core..=) Core.<$>
                    uniqueTicPerAudioTrack])

instance Core.FromJSON NielsenNonLinearWatermarkSettings where
        parseJSON
          = Core.withObject "NielsenNonLinearWatermarkSettings" Core.$
              \ x ->
                NielsenNonLinearWatermarkSettings' Core.<$>
                  (x Core..:? "activeWatermarkProcess") Core.<*>
                    x Core..:? "adiFilename"
                    Core.<*> x Core..:? "assetId"
                    Core.<*> x Core..:? "assetName"
                    Core.<*> x Core..:? "cbetSourceId"
                    Core.<*> x Core..:? "episodeId"
                    Core.<*> x Core..:? "metadataDestination"
                    Core.<*> x Core..:? "sourceId"
                    Core.<*> x Core..:? "sourceWatermarkStatus"
                    Core.<*> x Core..:? "ticServerUrl"
                    Core.<*> x Core..:? "uniqueTicPerAudioTrack"
