{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.QueryLoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.QueryLoggingConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

-- | A complex type that contains information about a configuration for DNS query logging.
--
--
--
-- /See:/ 'queryLoggingConfig' smart constructor.
data QueryLoggingConfig = QueryLoggingConfig'
  { _qlcId :: !Text,
    _qlcHostedZoneId :: !ResourceId,
    _qlcCloudWatchLogsLogGroupARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryLoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qlcId' - The ID for a configuration for DNS query logging.
--
-- * 'qlcHostedZoneId' - The ID of the hosted zone that CloudWatch Logs is logging queries for.
--
-- * 'qlcCloudWatchLogsLogGroupARN' - The Amazon Resource Name (ARN) of the CloudWatch Logs log group that Amazon Route 53 is publishing logs to.
queryLoggingConfig ::
  -- | 'qlcId'
  Text ->
  -- | 'qlcHostedZoneId'
  ResourceId ->
  -- | 'qlcCloudWatchLogsLogGroupARN'
  Text ->
  QueryLoggingConfig
queryLoggingConfig pId_ pHostedZoneId_ pCloudWatchLogsLogGroupARN_ =
  QueryLoggingConfig'
    { _qlcId = pId_,
      _qlcHostedZoneId = pHostedZoneId_,
      _qlcCloudWatchLogsLogGroupARN = pCloudWatchLogsLogGroupARN_
    }

-- | The ID for a configuration for DNS query logging.
qlcId :: Lens' QueryLoggingConfig Text
qlcId = lens _qlcId (\s a -> s {_qlcId = a})

-- | The ID of the hosted zone that CloudWatch Logs is logging queries for.
qlcHostedZoneId :: Lens' QueryLoggingConfig ResourceId
qlcHostedZoneId = lens _qlcHostedZoneId (\s a -> s {_qlcHostedZoneId = a})

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group that Amazon Route 53 is publishing logs to.
qlcCloudWatchLogsLogGroupARN :: Lens' QueryLoggingConfig Text
qlcCloudWatchLogsLogGroupARN = lens _qlcCloudWatchLogsLogGroupARN (\s a -> s {_qlcCloudWatchLogsLogGroupARN = a})

instance FromXML QueryLoggingConfig where
  parseXML x =
    QueryLoggingConfig'
      <$> (x .@ "Id")
      <*> (x .@ "HostedZoneId")
      <*> (x .@ "CloudWatchLogsLogGroupArn")

instance Hashable QueryLoggingConfig

instance NFData QueryLoggingConfig
