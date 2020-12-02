{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.RefreshSchemasStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.RefreshSchemasStatus where

import Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that describes status of a schema at an endpoint specified by the @DescribeRefreshSchemaStatus@ operation.
--
--
--
-- /See:/ 'refreshSchemasStatus' smart constructor.
data RefreshSchemasStatus = RefreshSchemasStatus'
  { _rssStatus ::
      !(Maybe RefreshSchemasStatusTypeValue),
    _rssLastRefreshDate :: !(Maybe POSIX),
    _rssReplicationInstanceARN :: !(Maybe Text),
    _rssEndpointARN :: !(Maybe Text),
    _rssLastFailureMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RefreshSchemasStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rssStatus' - The status of the schema.
--
-- * 'rssLastRefreshDate' - The date the schema was last refreshed.
--
-- * 'rssReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
--
-- * 'rssEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'rssLastFailureMessage' - The last failure message for the schema.
refreshSchemasStatus ::
  RefreshSchemasStatus
refreshSchemasStatus =
  RefreshSchemasStatus'
    { _rssStatus = Nothing,
      _rssLastRefreshDate = Nothing,
      _rssReplicationInstanceARN = Nothing,
      _rssEndpointARN = Nothing,
      _rssLastFailureMessage = Nothing
    }

-- | The status of the schema.
rssStatus :: Lens' RefreshSchemasStatus (Maybe RefreshSchemasStatusTypeValue)
rssStatus = lens _rssStatus (\s a -> s {_rssStatus = a})

-- | The date the schema was last refreshed.
rssLastRefreshDate :: Lens' RefreshSchemasStatus (Maybe UTCTime)
rssLastRefreshDate = lens _rssLastRefreshDate (\s a -> s {_rssLastRefreshDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the replication instance.
rssReplicationInstanceARN :: Lens' RefreshSchemasStatus (Maybe Text)
rssReplicationInstanceARN = lens _rssReplicationInstanceARN (\s a -> s {_rssReplicationInstanceARN = a})

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
rssEndpointARN :: Lens' RefreshSchemasStatus (Maybe Text)
rssEndpointARN = lens _rssEndpointARN (\s a -> s {_rssEndpointARN = a})

-- | The last failure message for the schema.
rssLastFailureMessage :: Lens' RefreshSchemasStatus (Maybe Text)
rssLastFailureMessage = lens _rssLastFailureMessage (\s a -> s {_rssLastFailureMessage = a})

instance FromJSON RefreshSchemasStatus where
  parseJSON =
    withObject
      "RefreshSchemasStatus"
      ( \x ->
          RefreshSchemasStatus'
            <$> (x .:? "Status")
            <*> (x .:? "LastRefreshDate")
            <*> (x .:? "ReplicationInstanceArn")
            <*> (x .:? "EndpointArn")
            <*> (x .:? "LastFailureMessage")
      )

instance Hashable RefreshSchemasStatus

instance NFData RefreshSchemasStatus
