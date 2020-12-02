{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.NielsenActiveWatermarkProcessType
import Network.AWS.MediaConvert.Types.NielsenSourceWatermarkStatusType
import Network.AWS.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
import Network.AWS.Prelude

-- | Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
--
-- /See:/ 'nielsenNonLinearWatermarkSettings' smart constructor.
data NielsenNonLinearWatermarkSettings = NielsenNonLinearWatermarkSettings'
  { _nnlwsEpisodeId ::
      !(Maybe Text),
    _nnlwsActiveWatermarkProcess ::
      !( Maybe
           NielsenActiveWatermarkProcessType
       ),
    _nnlwsSourceId ::
      !(Maybe Nat),
    _nnlwsCbetSourceId ::
      !(Maybe Text),
    _nnlwsTicServerURL ::
      !(Maybe Text),
    _nnlwsMetadataDestination ::
      !(Maybe Text),
    _nnlwsAssetName ::
      !(Maybe Text),
    _nnlwsAdiFilename ::
      !(Maybe Text),
    _nnlwsAssetId ::
      !(Maybe Text),
    _nnlwsUniqueTicPerAudioTrack ::
      !( Maybe
           NielsenUniqueTicPerAudioTrackType
       ),
    _nnlwsSourceWatermarkStatus ::
      !( Maybe
           NielsenSourceWatermarkStatusType
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NielsenNonLinearWatermarkSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nnlwsEpisodeId' - Optional. If this asset uses an episode ID with Nielsen, provide it here.
--
-- * 'nnlwsActiveWatermarkProcess' - Choose the type of Nielsen watermarks that you want in your outputs. When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value for the setting SID (sourceId). When you choose CBET (CBET), you must provide a value for the setting CSID (cbetSourceId). When you choose NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values for both of these settings.
--
-- * 'nnlwsSourceId' - Use the SID that Nielsen provides to you. This source ID should be unique to your Nielsen account but common to all of your output assets. Required for all Nielsen non-linear watermarking. This ID should be unique to your Nielsen account but common to all of your output assets. Required for all Nielsen non-linear watermarking.
--
-- * 'nnlwsCbetSourceId' - Use the CSID that Nielsen provides to you. This CBET source ID should be unique to your Nielsen account but common to all of your output assets that have CBET watermarking. Required when you choose a value for the setting Watermark types (ActiveWatermarkProcess) that includes CBET.
--
-- * 'nnlwsTicServerURL' - Specify the endpoint for the TIC server that you have deployed and configured in the AWS Cloud. Required for all Nielsen non-linear watermarking. MediaConvert can't connect directly to a TIC server. Instead, you must use API Gateway to provide a RESTful interface between MediaConvert and a TIC server that you deploy in your AWS account. For more information on deploying a TIC server in your AWS account and the required API Gateway, contact Nielsen support.
--
-- * 'nnlwsMetadataDestination' - Specify the Amazon S3 location where you want MediaConvert to save your Nielsen non-linear metadata .zip file. This Amazon S3 bucket must be in the same Region as the one where you do your MediaConvert transcoding. If you want to include an ADI file in this .zip file, use the setting ADI file (adiFilename) to specify it. MediaConvert delivers the Nielsen metadata .zip files only to your metadata destination Amazon S3 bucket. It doesn't deliver the .zip files to Nielsen. You are responsible for delivering the metadata .zip files to Nielsen.
--
-- * 'nnlwsAssetName' - Use the asset name that you provide to Nielsen for this asset. Required for all Nielsen non-linear watermarking.
--
-- * 'nnlwsAdiFilename' - Optional. Use this setting when you want the service to include an ADI file in the Nielsen  metadata .zip file. To provide an ADI file, store it in Amazon S3 and provide a URL to it  here. The URL should be in the following format: S3://bucket/path/ADI-file. For more information about the metadata .zip file, see the setting Metadata destination (metadataDestination).
--
-- * 'nnlwsAssetId' - Use the asset ID that you provide to Nielsen to uniquely identify this asset. Required for all Nielsen non-linear watermarking.
--
-- * 'nnlwsUniqueTicPerAudioTrack' - To create assets that have the same TIC values in each audio track, keep the default value Share TICs (SAME_TICS_PER_TRACK). To create assets that have unique TIC values for each audio track, choose Use unique TICs (RESERVE_UNIQUE_TICS_PER_TRACK).
--
-- * 'nnlwsSourceWatermarkStatus' - Required. Specify whether your source content already contains Nielsen non-linear watermarks. When you set this value to Watermarked (WATERMARKED), the service fails the job. Nielsen requires that you add non-linear watermarking to only clean content that doesn't already  have non-linear Nielsen watermarks.
nielsenNonLinearWatermarkSettings ::
  NielsenNonLinearWatermarkSettings
nielsenNonLinearWatermarkSettings =
  NielsenNonLinearWatermarkSettings'
    { _nnlwsEpisodeId = Nothing,
      _nnlwsActiveWatermarkProcess = Nothing,
      _nnlwsSourceId = Nothing,
      _nnlwsCbetSourceId = Nothing,
      _nnlwsTicServerURL = Nothing,
      _nnlwsMetadataDestination = Nothing,
      _nnlwsAssetName = Nothing,
      _nnlwsAdiFilename = Nothing,
      _nnlwsAssetId = Nothing,
      _nnlwsUniqueTicPerAudioTrack = Nothing,
      _nnlwsSourceWatermarkStatus = Nothing
    }

-- | Optional. If this asset uses an episode ID with Nielsen, provide it here.
nnlwsEpisodeId :: Lens' NielsenNonLinearWatermarkSettings (Maybe Text)
nnlwsEpisodeId = lens _nnlwsEpisodeId (\s a -> s {_nnlwsEpisodeId = a})

-- | Choose the type of Nielsen watermarks that you want in your outputs. When you choose NAES 2 and NW (NAES2_AND_NW), you must provide a value for the setting SID (sourceId). When you choose CBET (CBET), you must provide a value for the setting CSID (cbetSourceId). When you choose NAES 2, NW, and CBET (NAES2_AND_NW_AND_CBET), you must provide values for both of these settings.
nnlwsActiveWatermarkProcess :: Lens' NielsenNonLinearWatermarkSettings (Maybe NielsenActiveWatermarkProcessType)
nnlwsActiveWatermarkProcess = lens _nnlwsActiveWatermarkProcess (\s a -> s {_nnlwsActiveWatermarkProcess = a})

-- | Use the SID that Nielsen provides to you. This source ID should be unique to your Nielsen account but common to all of your output assets. Required for all Nielsen non-linear watermarking. This ID should be unique to your Nielsen account but common to all of your output assets. Required for all Nielsen non-linear watermarking.
nnlwsSourceId :: Lens' NielsenNonLinearWatermarkSettings (Maybe Natural)
nnlwsSourceId = lens _nnlwsSourceId (\s a -> s {_nnlwsSourceId = a}) . mapping _Nat

-- | Use the CSID that Nielsen provides to you. This CBET source ID should be unique to your Nielsen account but common to all of your output assets that have CBET watermarking. Required when you choose a value for the setting Watermark types (ActiveWatermarkProcess) that includes CBET.
nnlwsCbetSourceId :: Lens' NielsenNonLinearWatermarkSettings (Maybe Text)
nnlwsCbetSourceId = lens _nnlwsCbetSourceId (\s a -> s {_nnlwsCbetSourceId = a})

-- | Specify the endpoint for the TIC server that you have deployed and configured in the AWS Cloud. Required for all Nielsen non-linear watermarking. MediaConvert can't connect directly to a TIC server. Instead, you must use API Gateway to provide a RESTful interface between MediaConvert and a TIC server that you deploy in your AWS account. For more information on deploying a TIC server in your AWS account and the required API Gateway, contact Nielsen support.
nnlwsTicServerURL :: Lens' NielsenNonLinearWatermarkSettings (Maybe Text)
nnlwsTicServerURL = lens _nnlwsTicServerURL (\s a -> s {_nnlwsTicServerURL = a})

-- | Specify the Amazon S3 location where you want MediaConvert to save your Nielsen non-linear metadata .zip file. This Amazon S3 bucket must be in the same Region as the one where you do your MediaConvert transcoding. If you want to include an ADI file in this .zip file, use the setting ADI file (adiFilename) to specify it. MediaConvert delivers the Nielsen metadata .zip files only to your metadata destination Amazon S3 bucket. It doesn't deliver the .zip files to Nielsen. You are responsible for delivering the metadata .zip files to Nielsen.
nnlwsMetadataDestination :: Lens' NielsenNonLinearWatermarkSettings (Maybe Text)
nnlwsMetadataDestination = lens _nnlwsMetadataDestination (\s a -> s {_nnlwsMetadataDestination = a})

-- | Use the asset name that you provide to Nielsen for this asset. Required for all Nielsen non-linear watermarking.
nnlwsAssetName :: Lens' NielsenNonLinearWatermarkSettings (Maybe Text)
nnlwsAssetName = lens _nnlwsAssetName (\s a -> s {_nnlwsAssetName = a})

-- | Optional. Use this setting when you want the service to include an ADI file in the Nielsen  metadata .zip file. To provide an ADI file, store it in Amazon S3 and provide a URL to it  here. The URL should be in the following format: S3://bucket/path/ADI-file. For more information about the metadata .zip file, see the setting Metadata destination (metadataDestination).
nnlwsAdiFilename :: Lens' NielsenNonLinearWatermarkSettings (Maybe Text)
nnlwsAdiFilename = lens _nnlwsAdiFilename (\s a -> s {_nnlwsAdiFilename = a})

-- | Use the asset ID that you provide to Nielsen to uniquely identify this asset. Required for all Nielsen non-linear watermarking.
nnlwsAssetId :: Lens' NielsenNonLinearWatermarkSettings (Maybe Text)
nnlwsAssetId = lens _nnlwsAssetId (\s a -> s {_nnlwsAssetId = a})

-- | To create assets that have the same TIC values in each audio track, keep the default value Share TICs (SAME_TICS_PER_TRACK). To create assets that have unique TIC values for each audio track, choose Use unique TICs (RESERVE_UNIQUE_TICS_PER_TRACK).
nnlwsUniqueTicPerAudioTrack :: Lens' NielsenNonLinearWatermarkSettings (Maybe NielsenUniqueTicPerAudioTrackType)
nnlwsUniqueTicPerAudioTrack = lens _nnlwsUniqueTicPerAudioTrack (\s a -> s {_nnlwsUniqueTicPerAudioTrack = a})

-- | Required. Specify whether your source content already contains Nielsen non-linear watermarks. When you set this value to Watermarked (WATERMARKED), the service fails the job. Nielsen requires that you add non-linear watermarking to only clean content that doesn't already  have non-linear Nielsen watermarks.
nnlwsSourceWatermarkStatus :: Lens' NielsenNonLinearWatermarkSettings (Maybe NielsenSourceWatermarkStatusType)
nnlwsSourceWatermarkStatus = lens _nnlwsSourceWatermarkStatus (\s a -> s {_nnlwsSourceWatermarkStatus = a})

instance FromJSON NielsenNonLinearWatermarkSettings where
  parseJSON =
    withObject
      "NielsenNonLinearWatermarkSettings"
      ( \x ->
          NielsenNonLinearWatermarkSettings'
            <$> (x .:? "episodeId")
            <*> (x .:? "activeWatermarkProcess")
            <*> (x .:? "sourceId")
            <*> (x .:? "cbetSourceId")
            <*> (x .:? "ticServerUrl")
            <*> (x .:? "metadataDestination")
            <*> (x .:? "assetName")
            <*> (x .:? "adiFilename")
            <*> (x .:? "assetId")
            <*> (x .:? "uniqueTicPerAudioTrack")
            <*> (x .:? "sourceWatermarkStatus")
      )

instance Hashable NielsenNonLinearWatermarkSettings

instance NFData NielsenNonLinearWatermarkSettings

instance ToJSON NielsenNonLinearWatermarkSettings where
  toJSON NielsenNonLinearWatermarkSettings' {..} =
    object
      ( catMaybes
          [ ("episodeId" .=) <$> _nnlwsEpisodeId,
            ("activeWatermarkProcess" .=) <$> _nnlwsActiveWatermarkProcess,
            ("sourceId" .=) <$> _nnlwsSourceId,
            ("cbetSourceId" .=) <$> _nnlwsCbetSourceId,
            ("ticServerUrl" .=) <$> _nnlwsTicServerURL,
            ("metadataDestination" .=) <$> _nnlwsMetadataDestination,
            ("assetName" .=) <$> _nnlwsAssetName,
            ("adiFilename" .=) <$> _nnlwsAdiFilename,
            ("assetId" .=) <$> _nnlwsAssetId,
            ("uniqueTicPerAudioTrack" .=) <$> _nnlwsUniqueTicPerAudioTrack,
            ("sourceWatermarkStatus" .=) <$> _nnlwsSourceWatermarkStatus
          ]
      )
