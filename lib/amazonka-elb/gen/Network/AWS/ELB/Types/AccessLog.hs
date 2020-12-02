{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.AccessLog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.AccessLog where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the @AccessLog@ attribute.
--
--
--
-- /See:/ 'accessLog' smart constructor.
data AccessLog = AccessLog'
  { _alEmitInterval :: !(Maybe Int),
    _alS3BucketPrefix :: !(Maybe Text),
    _alS3BucketName :: !(Maybe Text),
    _alEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessLog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alEmitInterval' - The interval for publishing the access logs. You can specify an interval of either 5 minutes or 60 minutes. Default: 60 minutes
--
-- * 'alS3BucketPrefix' - The logical hierarchy you created for your Amazon S3 bucket, for example @my-bucket-prefix/prod@ . If the prefix is not provided, the log is placed at the root level of the bucket.
--
-- * 'alS3BucketName' - The name of the Amazon S3 bucket where the access logs are stored.
--
-- * 'alEnabled' - Specifies whether access logs are enabled for the load balancer.
accessLog ::
  -- | 'alEnabled'
  Bool ->
  AccessLog
accessLog pEnabled_ =
  AccessLog'
    { _alEmitInterval = Nothing,
      _alS3BucketPrefix = Nothing,
      _alS3BucketName = Nothing,
      _alEnabled = pEnabled_
    }

-- | The interval for publishing the access logs. You can specify an interval of either 5 minutes or 60 minutes. Default: 60 minutes
alEmitInterval :: Lens' AccessLog (Maybe Int)
alEmitInterval = lens _alEmitInterval (\s a -> s {_alEmitInterval = a})

-- | The logical hierarchy you created for your Amazon S3 bucket, for example @my-bucket-prefix/prod@ . If the prefix is not provided, the log is placed at the root level of the bucket.
alS3BucketPrefix :: Lens' AccessLog (Maybe Text)
alS3BucketPrefix = lens _alS3BucketPrefix (\s a -> s {_alS3BucketPrefix = a})

-- | The name of the Amazon S3 bucket where the access logs are stored.
alS3BucketName :: Lens' AccessLog (Maybe Text)
alS3BucketName = lens _alS3BucketName (\s a -> s {_alS3BucketName = a})

-- | Specifies whether access logs are enabled for the load balancer.
alEnabled :: Lens' AccessLog Bool
alEnabled = lens _alEnabled (\s a -> s {_alEnabled = a})

instance FromXML AccessLog where
  parseXML x =
    AccessLog'
      <$> (x .@? "EmitInterval")
      <*> (x .@? "S3BucketPrefix")
      <*> (x .@? "S3BucketName")
      <*> (x .@ "Enabled")

instance Hashable AccessLog

instance NFData AccessLog

instance ToQuery AccessLog where
  toQuery AccessLog' {..} =
    mconcat
      [ "EmitInterval" =: _alEmitInterval,
        "S3BucketPrefix" =: _alS3BucketPrefix,
        "S3BucketName" =: _alS3BucketName,
        "Enabled" =: _alEnabled
      ]
