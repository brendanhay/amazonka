{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackDetail where

import Network.AWS.Config.Types.ConformancePackInputParameter
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns details of a conformance pack. A conformance pack is a collection of AWS Config rules and remediation actions that can be easily deployed in an account and a region.
--
--
--
-- /See:/ 'conformancePackDetail' smart constructor.
data ConformancePackDetail = ConformancePackDetail'
  { _cpdDeliveryS3Bucket ::
      !(Maybe Text),
    _cpdDeliveryS3KeyPrefix :: !(Maybe Text),
    _cpdCreatedBy :: !(Maybe Text),
    _cpdLastUpdateRequestedTime :: !(Maybe POSIX),
    _cpdConformancePackInputParameters ::
      !(Maybe [ConformancePackInputParameter]),
    _cpdConformancePackName :: !Text,
    _cpdConformancePackARN :: !Text,
    _cpdConformancePackId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConformancePackDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpdDeliveryS3Bucket' - Conformance pack template that is used to create a pack. The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*".
--
-- * 'cpdDeliveryS3KeyPrefix' - The prefix for the Amazon S3 bucket.
--
-- * 'cpdCreatedBy' - AWS service that created the conformance pack.
--
-- * 'cpdLastUpdateRequestedTime' - Last time when conformation pack update was requested.
--
-- * 'cpdConformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
--
-- * 'cpdConformancePackName' - Name of the conformance pack.
--
-- * 'cpdConformancePackARN' - Amazon Resource Name (ARN) of the conformance pack.
--
-- * 'cpdConformancePackId' - ID of the conformance pack.
conformancePackDetail ::
  -- | 'cpdConformancePackName'
  Text ->
  -- | 'cpdConformancePackARN'
  Text ->
  -- | 'cpdConformancePackId'
  Text ->
  ConformancePackDetail
conformancePackDetail
  pConformancePackName_
  pConformancePackARN_
  pConformancePackId_ =
    ConformancePackDetail'
      { _cpdDeliveryS3Bucket = Nothing,
        _cpdDeliveryS3KeyPrefix = Nothing,
        _cpdCreatedBy = Nothing,
        _cpdLastUpdateRequestedTime = Nothing,
        _cpdConformancePackInputParameters = Nothing,
        _cpdConformancePackName = pConformancePackName_,
        _cpdConformancePackARN = pConformancePackARN_,
        _cpdConformancePackId = pConformancePackId_
      }

-- | Conformance pack template that is used to create a pack. The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*".
cpdDeliveryS3Bucket :: Lens' ConformancePackDetail (Maybe Text)
cpdDeliveryS3Bucket = lens _cpdDeliveryS3Bucket (\s a -> s {_cpdDeliveryS3Bucket = a})

-- | The prefix for the Amazon S3 bucket.
cpdDeliveryS3KeyPrefix :: Lens' ConformancePackDetail (Maybe Text)
cpdDeliveryS3KeyPrefix = lens _cpdDeliveryS3KeyPrefix (\s a -> s {_cpdDeliveryS3KeyPrefix = a})

-- | AWS service that created the conformance pack.
cpdCreatedBy :: Lens' ConformancePackDetail (Maybe Text)
cpdCreatedBy = lens _cpdCreatedBy (\s a -> s {_cpdCreatedBy = a})

-- | Last time when conformation pack update was requested.
cpdLastUpdateRequestedTime :: Lens' ConformancePackDetail (Maybe UTCTime)
cpdLastUpdateRequestedTime = lens _cpdLastUpdateRequestedTime (\s a -> s {_cpdLastUpdateRequestedTime = a}) . mapping _Time

-- | A list of @ConformancePackInputParameter@ objects.
cpdConformancePackInputParameters :: Lens' ConformancePackDetail [ConformancePackInputParameter]
cpdConformancePackInputParameters = lens _cpdConformancePackInputParameters (\s a -> s {_cpdConformancePackInputParameters = a}) . _Default . _Coerce

-- | Name of the conformance pack.
cpdConformancePackName :: Lens' ConformancePackDetail Text
cpdConformancePackName = lens _cpdConformancePackName (\s a -> s {_cpdConformancePackName = a})

-- | Amazon Resource Name (ARN) of the conformance pack.
cpdConformancePackARN :: Lens' ConformancePackDetail Text
cpdConformancePackARN = lens _cpdConformancePackARN (\s a -> s {_cpdConformancePackARN = a})

-- | ID of the conformance pack.
cpdConformancePackId :: Lens' ConformancePackDetail Text
cpdConformancePackId = lens _cpdConformancePackId (\s a -> s {_cpdConformancePackId = a})

instance FromJSON ConformancePackDetail where
  parseJSON =
    withObject
      "ConformancePackDetail"
      ( \x ->
          ConformancePackDetail'
            <$> (x .:? "DeliveryS3Bucket")
            <*> (x .:? "DeliveryS3KeyPrefix")
            <*> (x .:? "CreatedBy")
            <*> (x .:? "LastUpdateRequestedTime")
            <*> (x .:? "ConformancePackInputParameters" .!= mempty)
            <*> (x .: "ConformancePackName")
            <*> (x .: "ConformancePackArn")
            <*> (x .: "ConformancePackId")
      )

instance Hashable ConformancePackDetail

instance NFData ConformancePackDetail
