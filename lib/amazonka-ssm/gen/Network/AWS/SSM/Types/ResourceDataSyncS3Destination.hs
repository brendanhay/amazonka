{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncS3Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncS3Destination where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
import Network.AWS.SSM.Types.ResourceDataSyncS3Format

-- | Information about the target S3 bucket for the Resource Data Sync.
--
--
--
-- /See:/ 'resourceDataSyncS3Destination' smart constructor.
data ResourceDataSyncS3Destination = ResourceDataSyncS3Destination'
  { _rdssdPrefix ::
      !(Maybe Text),
    _rdssdDestinationDataSharing ::
      !( Maybe
           ResourceDataSyncDestinationDataSharing
       ),
    _rdssdAWSKMSKeyARN ::
      !(Maybe Text),
    _rdssdBucketName :: !Text,
    _rdssdSyncFormat ::
      !ResourceDataSyncS3Format,
    _rdssdRegion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceDataSyncS3Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdssdPrefix' - An Amazon S3 prefix for the bucket.
--
-- * 'rdssdDestinationDataSharing' - Enables destination data sharing. By default, this field is @null@ .
--
-- * 'rdssdAWSKMSKeyARN' - The ARN of an encryption key for a destination in Amazon S3. Must belong to the same Region as the destination S3 bucket.
--
-- * 'rdssdBucketName' - The name of the S3 bucket where the aggregated data is stored.
--
-- * 'rdssdSyncFormat' - A supported sync format. The following format is currently supported: JsonSerDe
--
-- * 'rdssdRegion' - The AWS Region with the S3 bucket targeted by the Resource Data Sync.
resourceDataSyncS3Destination ::
  -- | 'rdssdBucketName'
  Text ->
  -- | 'rdssdSyncFormat'
  ResourceDataSyncS3Format ->
  -- | 'rdssdRegion'
  Text ->
  ResourceDataSyncS3Destination
resourceDataSyncS3Destination pBucketName_ pSyncFormat_ pRegion_ =
  ResourceDataSyncS3Destination'
    { _rdssdPrefix = Nothing,
      _rdssdDestinationDataSharing = Nothing,
      _rdssdAWSKMSKeyARN = Nothing,
      _rdssdBucketName = pBucketName_,
      _rdssdSyncFormat = pSyncFormat_,
      _rdssdRegion = pRegion_
    }

-- | An Amazon S3 prefix for the bucket.
rdssdPrefix :: Lens' ResourceDataSyncS3Destination (Maybe Text)
rdssdPrefix = lens _rdssdPrefix (\s a -> s {_rdssdPrefix = a})

-- | Enables destination data sharing. By default, this field is @null@ .
rdssdDestinationDataSharing :: Lens' ResourceDataSyncS3Destination (Maybe ResourceDataSyncDestinationDataSharing)
rdssdDestinationDataSharing = lens _rdssdDestinationDataSharing (\s a -> s {_rdssdDestinationDataSharing = a})

-- | The ARN of an encryption key for a destination in Amazon S3. Must belong to the same Region as the destination S3 bucket.
rdssdAWSKMSKeyARN :: Lens' ResourceDataSyncS3Destination (Maybe Text)
rdssdAWSKMSKeyARN = lens _rdssdAWSKMSKeyARN (\s a -> s {_rdssdAWSKMSKeyARN = a})

-- | The name of the S3 bucket where the aggregated data is stored.
rdssdBucketName :: Lens' ResourceDataSyncS3Destination Text
rdssdBucketName = lens _rdssdBucketName (\s a -> s {_rdssdBucketName = a})

-- | A supported sync format. The following format is currently supported: JsonSerDe
rdssdSyncFormat :: Lens' ResourceDataSyncS3Destination ResourceDataSyncS3Format
rdssdSyncFormat = lens _rdssdSyncFormat (\s a -> s {_rdssdSyncFormat = a})

-- | The AWS Region with the S3 bucket targeted by the Resource Data Sync.
rdssdRegion :: Lens' ResourceDataSyncS3Destination Text
rdssdRegion = lens _rdssdRegion (\s a -> s {_rdssdRegion = a})

instance FromJSON ResourceDataSyncS3Destination where
  parseJSON =
    withObject
      "ResourceDataSyncS3Destination"
      ( \x ->
          ResourceDataSyncS3Destination'
            <$> (x .:? "Prefix")
            <*> (x .:? "DestinationDataSharing")
            <*> (x .:? "AWSKMSKeyARN")
            <*> (x .: "BucketName")
            <*> (x .: "SyncFormat")
            <*> (x .: "Region")
      )

instance Hashable ResourceDataSyncS3Destination

instance NFData ResourceDataSyncS3Destination

instance ToJSON ResourceDataSyncS3Destination where
  toJSON ResourceDataSyncS3Destination' {..} =
    object
      ( catMaybes
          [ ("Prefix" .=) <$> _rdssdPrefix,
            ("DestinationDataSharing" .=) <$> _rdssdDestinationDataSharing,
            ("AWSKMSKeyARN" .=) <$> _rdssdAWSKMSKeyARN,
            Just ("BucketName" .= _rdssdBucketName),
            Just ("SyncFormat" .= _rdssdSyncFormat),
            Just ("Region" .= _rdssdRegion)
          ]
      )
