{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.TestFailover
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the input of a @TestFailover@ operation which test automatic failover on a specified node group (called shard in the console) in a replication group (called cluster in the console).
--
--
-- __Note the following__
--
--     * A customer can use this operation to test automatic failover on up to 5 shards (called node groups in the ElastiCache API and AWS CLI) in any rolling 24-hour period.
--
--     * If calling this operation on shards in different clusters (called replication groups in the API and CLI), the calls can be made concurrently.
--
--
--
--     * If calling this operation multiple times on different shards in the same Redis (cluster mode enabled) replication group, the first node replacement must complete before a subsequent call can be made.
--
--     * To determine whether the node replacement is complete you can check Events using the Amazon ElastiCache console, the AWS CLI, or the ElastiCache API. Look for the following automatic failover related events, listed here in order of occurrance:
--
--     * Replication group message: @Test Failover API called for node group <node-group-id>@
--
--     * Cache cluster message: @Failover from master node <primary-node-id> to replica node <node-id> completed@
--
--     * Replication group message: @Failover from master node <primary-node-id> to replica node <node-id> completed@
--
--     * Cache cluster message: @Recovering cache nodes <node-id>@
--
--     * Cache cluster message: @Finished recovery for cache nodes <node-id>@
--
--
--
-- For more information see:
--
--     * <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/ECEvents.Viewing.html Viewing ElastiCache Events> in the /ElastiCache User Guide/
--
--     * <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeEvents.html DescribeEvents> in the ElastiCache API Reference
--
--
--
--
--
-- Also see, <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/AutoFailover.html#auto-failover-test Testing Multi-AZ with Automatic Failover> in the /ElastiCache User Guide/ .
--
module Network.AWS.ElastiCache.TestFailover
    (
    -- * Creating a Request
      testFailover
    , TestFailover
    -- * Request Lenses
    , tfReplicationGroupId
    , tfNodeGroupId

    -- * Destructuring the Response
    , testFailoverResponse
    , TestFailoverResponse
    -- * Response Lenses
    , tfrsReplicationGroup
    , tfrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'testFailover' smart constructor.
data TestFailover = TestFailover'
  { _tfReplicationGroupId :: !Text
  , _tfNodeGroupId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestFailover' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfReplicationGroupId' - The name of the replication group (console: cluster) whose automatic failover is being tested by this operation.
--
-- * 'tfNodeGroupId' - The name of the node group (called shard in the console) in this replication group on which automatic failover is to be tested. You may test automatic failover on up to 5 node groups in any rolling 24-hour period.
testFailover
    :: Text -- ^ 'tfReplicationGroupId'
    -> Text -- ^ 'tfNodeGroupId'
    -> TestFailover
testFailover pReplicationGroupId_ pNodeGroupId_ =
  TestFailover'
    { _tfReplicationGroupId = pReplicationGroupId_
    , _tfNodeGroupId = pNodeGroupId_
    }


-- | The name of the replication group (console: cluster) whose automatic failover is being tested by this operation.
tfReplicationGroupId :: Lens' TestFailover Text
tfReplicationGroupId = lens _tfReplicationGroupId (\ s a -> s{_tfReplicationGroupId = a})

-- | The name of the node group (called shard in the console) in this replication group on which automatic failover is to be tested. You may test automatic failover on up to 5 node groups in any rolling 24-hour period.
tfNodeGroupId :: Lens' TestFailover Text
tfNodeGroupId = lens _tfNodeGroupId (\ s a -> s{_tfNodeGroupId = a})

instance AWSRequest TestFailover where
        type Rs TestFailover = TestFailoverResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "TestFailoverResult"
              (\ s h x ->
                 TestFailoverResponse' <$>
                   (x .@? "ReplicationGroup") <*> (pure (fromEnum s)))

instance Hashable TestFailover where

instance NFData TestFailover where

instance ToHeaders TestFailover where
        toHeaders = const mempty

instance ToPath TestFailover where
        toPath = const "/"

instance ToQuery TestFailover where
        toQuery TestFailover'{..}
          = mconcat
              ["Action" =: ("TestFailover" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "ReplicationGroupId" =: _tfReplicationGroupId,
               "NodeGroupId" =: _tfNodeGroupId]

-- | /See:/ 'testFailoverResponse' smart constructor.
data TestFailoverResponse = TestFailoverResponse'
  { _tfrsReplicationGroup :: !(Maybe ReplicationGroup)
  , _tfrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestFailoverResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfrsReplicationGroup' - Undocumented member.
--
-- * 'tfrsResponseStatus' - -- | The response status code.
testFailoverResponse
    :: Int -- ^ 'tfrsResponseStatus'
    -> TestFailoverResponse
testFailoverResponse pResponseStatus_ =
  TestFailoverResponse'
    {_tfrsReplicationGroup = Nothing, _tfrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
tfrsReplicationGroup :: Lens' TestFailoverResponse (Maybe ReplicationGroup)
tfrsReplicationGroup = lens _tfrsReplicationGroup (\ s a -> s{_tfrsReplicationGroup = a})

-- | -- | The response status code.
tfrsResponseStatus :: Lens' TestFailoverResponse Int
tfrsResponseStatus = lens _tfrsResponseStatus (\ s a -> s{_tfrsResponseStatus = a})

instance NFData TestFailoverResponse where
