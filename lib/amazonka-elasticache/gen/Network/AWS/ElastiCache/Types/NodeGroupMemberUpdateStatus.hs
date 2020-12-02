{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus where

import Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy
import Network.AWS.ElastiCache.Types.NodeUpdateStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status of the service update on the node group member
--
--
--
-- /See:/ 'nodeGroupMemberUpdateStatus' smart constructor.
data NodeGroupMemberUpdateStatus = NodeGroupMemberUpdateStatus'
  { _ngmusNodeUpdateEndDate ::
      !(Maybe ISO8601),
    _ngmusNodeUpdateInitiatedBy ::
      !(Maybe NodeUpdateInitiatedBy),
    _ngmusNodeUpdateStatusModifiedDate ::
      !(Maybe ISO8601),
    _ngmusCacheClusterId ::
      !(Maybe Text),
    _ngmusCacheNodeId :: !(Maybe Text),
    _ngmusNodeUpdateInitiatedDate ::
      !(Maybe ISO8601),
    _ngmusNodeUpdateStartDate ::
      !(Maybe ISO8601),
    _ngmusNodeUpdateStatus ::
      !(Maybe NodeUpdateStatus),
    _ngmusNodeDeletionDate ::
      !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeGroupMemberUpdateStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngmusNodeUpdateEndDate' - The end date of the update for a node
--
-- * 'ngmusNodeUpdateInitiatedBy' - Reflects whether the update was initiated by the customer or automatically applied
--
-- * 'ngmusNodeUpdateStatusModifiedDate' - The date when the NodeUpdateStatus was last modified
--
-- * 'ngmusCacheClusterId' - The cache cluster ID
--
-- * 'ngmusCacheNodeId' - The node ID of the cache cluster
--
-- * 'ngmusNodeUpdateInitiatedDate' - The date when the update is triggered
--
-- * 'ngmusNodeUpdateStartDate' - The start date of the update for a node
--
-- * 'ngmusNodeUpdateStatus' - The update status of the node
--
-- * 'ngmusNodeDeletionDate' - The deletion date of the node
nodeGroupMemberUpdateStatus ::
  NodeGroupMemberUpdateStatus
nodeGroupMemberUpdateStatus =
  NodeGroupMemberUpdateStatus'
    { _ngmusNodeUpdateEndDate = Nothing,
      _ngmusNodeUpdateInitiatedBy = Nothing,
      _ngmusNodeUpdateStatusModifiedDate = Nothing,
      _ngmusCacheClusterId = Nothing,
      _ngmusCacheNodeId = Nothing,
      _ngmusNodeUpdateInitiatedDate = Nothing,
      _ngmusNodeUpdateStartDate = Nothing,
      _ngmusNodeUpdateStatus = Nothing,
      _ngmusNodeDeletionDate = Nothing
    }

-- | The end date of the update for a node
ngmusNodeUpdateEndDate :: Lens' NodeGroupMemberUpdateStatus (Maybe UTCTime)
ngmusNodeUpdateEndDate = lens _ngmusNodeUpdateEndDate (\s a -> s {_ngmusNodeUpdateEndDate = a}) . mapping _Time

-- | Reflects whether the update was initiated by the customer or automatically applied
ngmusNodeUpdateInitiatedBy :: Lens' NodeGroupMemberUpdateStatus (Maybe NodeUpdateInitiatedBy)
ngmusNodeUpdateInitiatedBy = lens _ngmusNodeUpdateInitiatedBy (\s a -> s {_ngmusNodeUpdateInitiatedBy = a})

-- | The date when the NodeUpdateStatus was last modified
ngmusNodeUpdateStatusModifiedDate :: Lens' NodeGroupMemberUpdateStatus (Maybe UTCTime)
ngmusNodeUpdateStatusModifiedDate = lens _ngmusNodeUpdateStatusModifiedDate (\s a -> s {_ngmusNodeUpdateStatusModifiedDate = a}) . mapping _Time

-- | The cache cluster ID
ngmusCacheClusterId :: Lens' NodeGroupMemberUpdateStatus (Maybe Text)
ngmusCacheClusterId = lens _ngmusCacheClusterId (\s a -> s {_ngmusCacheClusterId = a})

-- | The node ID of the cache cluster
ngmusCacheNodeId :: Lens' NodeGroupMemberUpdateStatus (Maybe Text)
ngmusCacheNodeId = lens _ngmusCacheNodeId (\s a -> s {_ngmusCacheNodeId = a})

-- | The date when the update is triggered
ngmusNodeUpdateInitiatedDate :: Lens' NodeGroupMemberUpdateStatus (Maybe UTCTime)
ngmusNodeUpdateInitiatedDate = lens _ngmusNodeUpdateInitiatedDate (\s a -> s {_ngmusNodeUpdateInitiatedDate = a}) . mapping _Time

-- | The start date of the update for a node
ngmusNodeUpdateStartDate :: Lens' NodeGroupMemberUpdateStatus (Maybe UTCTime)
ngmusNodeUpdateStartDate = lens _ngmusNodeUpdateStartDate (\s a -> s {_ngmusNodeUpdateStartDate = a}) . mapping _Time

-- | The update status of the node
ngmusNodeUpdateStatus :: Lens' NodeGroupMemberUpdateStatus (Maybe NodeUpdateStatus)
ngmusNodeUpdateStatus = lens _ngmusNodeUpdateStatus (\s a -> s {_ngmusNodeUpdateStatus = a})

-- | The deletion date of the node
ngmusNodeDeletionDate :: Lens' NodeGroupMemberUpdateStatus (Maybe UTCTime)
ngmusNodeDeletionDate = lens _ngmusNodeDeletionDate (\s a -> s {_ngmusNodeDeletionDate = a}) . mapping _Time

instance FromXML NodeGroupMemberUpdateStatus where
  parseXML x =
    NodeGroupMemberUpdateStatus'
      <$> (x .@? "NodeUpdateEndDate")
      <*> (x .@? "NodeUpdateInitiatedBy")
      <*> (x .@? "NodeUpdateStatusModifiedDate")
      <*> (x .@? "CacheClusterId")
      <*> (x .@? "CacheNodeId")
      <*> (x .@? "NodeUpdateInitiatedDate")
      <*> (x .@? "NodeUpdateStartDate")
      <*> (x .@? "NodeUpdateStatus")
      <*> (x .@? "NodeDeletionDate")

instance Hashable NodeGroupMemberUpdateStatus

instance NFData NodeGroupMemberUpdateStatus
