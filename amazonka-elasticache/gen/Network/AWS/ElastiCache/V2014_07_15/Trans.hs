{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | ElastiCache is a web service that makes it easy to deploy, operate, and
-- scale an in-memory cache in the cloud. The service improves the performance
-- of web applications by allowing you to retrieve information from fast,
-- managed, in-memory caches, instead of relying entirely on slower disk-based
-- databases. Amazon ElastiCache automatically detects and replaces failed
-- nodes, reducing the overhead associated with self-managed infrastructures
-- and provides a resilient system that mitigates the risk of overloaded
-- databases, which slow website and application load times. Through
-- integration with Amazon CloudWatch, Amazon ElastiCache provides enhanced
-- visibility into key performance metrics associated with your Memcached or
-- Redis nodes.
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.ElastiCache.V2014_07_15.Trans
    (
    -- * AuthorizeCacheSecurityGroupIngress
      authorizeCacheSecurityGroupIngress
    -- * CopySnapshot
    , copySnapshot
    -- * CreateCacheCluster
    , createCacheCluster
    -- * CreateCacheParameterGroup
    , createCacheParameterGroup
    -- * CreateCacheSecurityGroup
    , createCacheSecurityGroup
    -- * CreateCacheSubnetGroup
    , createCacheSubnetGroup
    -- * CreateReplicationGroup
    , createReplicationGroup
    -- * CreateSnapshot
    , createSnapshot
    -- * DeleteCacheCluster
    , deleteCacheCluster
    -- * DeleteCacheParameterGroup
    , deleteCacheParameterGroup
    -- * DeleteCacheSecurityGroup
    , deleteCacheSecurityGroup
    -- * DeleteCacheSubnetGroup
    , deleteCacheSubnetGroup
    -- * DeleteReplicationGroup
    , deleteReplicationGroup
    -- * DeleteSnapshot
    , deleteSnapshot
    -- * DescribeCacheClusters
    , describeCacheClusters
    -- * DescribeCacheEngineVersions
    , describeCacheEngineVersions
    -- * DescribeCacheParameterGroups
    , describeCacheParameterGroups
    -- * DescribeCacheParameters
    , describeCacheParameters
    -- * DescribeCacheSecurityGroups
    , describeCacheSecurityGroups
    -- * DescribeCacheSubnetGroups
    , describeCacheSubnetGroups
    -- * DescribeEngineDefaultParameters
    , describeEngineDefaultParameters
    -- * DescribeEvents
    , describeEvents
    -- * DescribeReplicationGroups
    , describeReplicationGroups
    -- * DescribeReservedCacheNodes
    , describeReservedCacheNodes
    -- * DescribeReservedCacheNodesOfferings
    , describeReservedCacheNodesOfferings
    -- * DescribeSnapshots
    , describeSnapshots
    -- * ModifyCacheCluster
    , modifyCacheCluster
    -- * ModifyCacheParameterGroup
    , modifyCacheParameterGroup
    -- * ModifyCacheSubnetGroup
    , modifyCacheSubnetGroup
    -- * ModifyReplicationGroup
    , modifyReplicationGroup
    -- * PurchaseReservedCacheNodesOffering
    , purchaseReservedCacheNodesOffering
    -- * RebootCacheCluster
    , rebootCacheCluster
    -- * ResetCacheParameterGroup
    , resetCacheParameterGroup
    -- * RevokeCacheSecurityGroupIngress
    , revokeCacheSecurityGroupIngress

    -- * Re-exported
    , module Control.Monad.Trans.AWS
    , module Network.AWS.ElastiCache.V2014_07_15
    ) where

import Control.Monad.Trans.AWS
import Network.AWS.Prelude
import Network.AWS.ElastiCache.V2014_07_15

-- | The AuthorizeCacheSecurityGroupIngress operation allows network ingress to
-- a cache security group. Applications using ElastiCache must be running on
-- Amazon EC2, and Amazon EC2 security groups are used as the authorization
-- mechanism. You cannot authorize ingress from an Amazon EC2 security group
-- in one region to an ElastiCache cluster in another region.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=AuthorizeCacheSecurityGroupIngress &EC2SecurityGroupName=default
-- &CacheSecurityGroupName=mygroup &EC2SecurityGroupOwnerId=1234-5678-1234
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= authorizing default
-- 565419523791 mygroup 123456781234 My security group
-- 817fa999-3647-11e0-ae57-f96cfe56749c.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.AuthorizeCacheSecurityGroupIngress'
authorizeCacheSecurityGroupIngress :: ( MonadCatch m
                                      , MonadResource m
                                      , MonadError Error m
                                      , MonadReader Env m
                                      , AWSRequest a
                                      )
                                   => Text -- ^ 'acsgiCacheSecurityGroupName'
                                   -> Text -- ^ 'acsgiEC2SecurityGroupName'
                                   -> Text -- ^ 'acsgiEC2SecurityGroupOwnerId'
                                   -> State AuthorizeCacheSecurityGroupIngress a
                                   -> m AuthorizeCacheSecurityGroupIngressResponse
authorizeCacheSecurityGroupIngress p1 p2 p3 s =
    send $ (mkAuthorizeCacheSecurityGroupIngress p1 p2 p3) &~ s

-- | The CopySnapshot operation makes a copy of an existing snapshot.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.CopySnapshot'
copySnapshot :: ( MonadCatch m
                , MonadResource m
                , MonadError Error m
                , MonadReader Env m
                , AWSRequest a
                )
             => Text -- ^ 'csSourceSnapshotName'
             -> Text -- ^ 'csTargetSnapshotName'
             -> State CopySnapshot a
             -> m CopySnapshotResponse
copySnapshot p1 p2 s =
    send $ (mkCopySnapshot p1 p2) &~ s

-- | The CreateCacheCluster operation creates a new cache cluster. All nodes in
-- the cache cluster run the same protocol-compliant cache engine software -
-- either Memcached or Redis. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=CreateCacheCluster &CacheClusterId=myMemcachedCluster
-- &CacheNodeType=cache.m1.small &CacheSecurityGroupNames.member.1=default
-- &Engine=memcached &NumCacheNodes=3
-- &PreferredAvailabilityZones.member.1=us-east-1a
-- &PreferredAvailabilityZones.member.2=us-east-1b
-- &PreferredAvailabilityZones.member.3=us-east-1e
-- &SignatureMethod=HmacSHA256&SignatureVersion=4 &Version=2014-03-24
-- &X-Amz-Algorithm=AWS4-HMAC-SHA256
-- &X-Amz-Credential=[your-access-key-id]/20140721/us-east-1/elasticache/aws4_request
-- &X-Amz-Date=20140724T170651Z
-- &X-Amz-SignedHeaders=content-type;host;user-agent;x-amz-content-sha256;x-amz-date
-- &X-Amz-Signature=[signature-value] creating 3 memcached 1.4.5 true
-- sun:08:00-sun:09:00 cache.m1.large default active mycache
-- aaf2e796-363f-11e0-a564-8f11342c56b0 ]]> -->.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.CreateCacheCluster'
createCacheCluster :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'cccCacheClusterId'
                   -> State CreateCacheCluster a
                   -> m CreateCacheClusterResponse
createCacheCluster p1 s =
    send $ (mkCreateCacheCluster p1) &~ s

-- | The CreateCacheParameterGroup operation creates a new cache parameter
-- group. A cache parameter group is a collection of parameters that you apply
-- to all of the nodes in a cache cluster.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=CreateCacheParameterGroup
-- &Description=My%20first%20cache%20parameter%20group
-- &CacheParameterGroupFamily=memcached1.4
-- &CacheParameterGroupName=mycacheparametergroup1 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= mycacheparametergroup3 memcached1.4 My first cache
-- parameter group 05699541-b7f9-11e0-9326-b7275b9d4a6c.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.CreateCacheParameterGroup'
createCacheParameterGroup :: ( MonadCatch m
                             , MonadResource m
                             , MonadError Error m
                             , MonadReader Env m
                             , AWSRequest a
                             )
                          => Text -- ^ 'ccpgCacheParameterGroupName'
                          -> Text -- ^ 'ccpgCacheParameterGroupFamily'
                          -> Text -- ^ 'ccpgDescription'
                          -> State CreateCacheParameterGroup a
                          -> m CreateCacheParameterGroupResponse
createCacheParameterGroup p1 p2 p3 s =
    send $ (mkCreateCacheParameterGroup p1 p2 p3) &~ s

-- | The CreateCacheSecurityGroup operation creates a new cache security group.
-- Use a cache security group to control access to one or more cache clusters.
-- Cache security groups are only used when you are creating a cluster outside
-- of an Amazon Virtual Private Cloud (VPC). If you are creating a cluster
-- inside of a VPC, use a cache subnet group instead. For more information,
-- see CreateCacheSubnetGroup. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=CreateCacheSecurityGroup
-- &CacheSecurityGroupName=mycachesecuritygroup
-- &Description=My%20cache%20security%20group &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= mycachesecuritygroup 123456789012 My cache security
-- group 2b1c8035-b7fa-11e0-9326-b7275b9d4a6c.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.CreateCacheSecurityGroup'
createCacheSecurityGroup :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'ccsgCacheSecurityGroupName'
                         -> Text -- ^ 'ccsgDescription'
                         -> State CreateCacheSecurityGroup a
                         -> m CreateCacheSecurityGroupResponse
createCacheSecurityGroup p1 p2 s =
    send $ (mkCreateCacheSecurityGroup p1 p2) &~ s

-- | The CreateCacheSubnetGroup operation creates a new cache subnet group. Use
-- this parameter only when you are creating a cluster in an Amazon Virtual
-- Private Cloud (VPC). https://elasticache.amazonaws.com/
-- ?Action=CreateCacheSubnetGroup &CacheSubnetGroupName=myCachesubnetgroup
-- &CacheSubnetGroupDescription=My%20new%20CacheSubnetGroup
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= 990524496922 My new
-- CacheSubnetGroup myCachesubnetgroup Active subnet-7c5b4115 us-east-1c
-- Active subnet-7b5b4112 us-east-1b Active subnet-3ea6bd57 us-east-1d
-- ed662948-a57b-11df-9e38-7ffab86c801f.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.CreateCacheSubnetGroup'
createCacheSubnetGroup :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'ccsg1CacheSubnetGroupName'
                       -> Text -- ^ 'ccsg1CacheSubnetGroupDescription'
                       -> [Text] -- ^ 'ccsg1SubnetIds'
                       -> State CreateCacheSubnetGroup a
                       -> m CreateCacheSubnetGroupResponse
createCacheSubnetGroup p1 p2 p3 s =
    send $ (mkCreateCacheSubnetGroup p1 p2 p3) &~ s

-- | The CreateReplicationGroup operation creates a replication group. A
-- replication group is a collection of cache clusters, where one of the
-- clusters is a read/write primary and the other clusters are read-only
-- replicas. Writes to the primary are automatically propagated to the
-- replicas. When you create a replication group, you must specify an existing
-- cache cluster that is in the primary role. When the replication group has
-- been successfully created, you can add one or more read replica replicas to
-- it, up to a total of five read replicas.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=CreateReplicationGroup
-- ?ReplicationGroupDescription=My%20replication%20group
-- &ReplicationGroupId=my-repgroup &PrimaryClusterId=my-redis-primary
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= my-redis-primary
-- my-redis-primary my-repgroup creating My replication group
-- f3b7b32d-b9d2-11e3-8a16-7978bb24ffdf.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.CreateReplicationGroup'
createReplicationGroup :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'crgReplicationGroupId'
                       -> Text -- ^ 'crgPrimaryClusterId'
                       -> Text -- ^ 'crgReplicationGroupDescription'
                       -> State CreateReplicationGroup a
                       -> m CreateReplicationGroupResponse
createReplicationGroup p1 p2 p3 s =
    send $ (mkCreateReplicationGroup p1 p2 p3) &~ s

-- | The CreateSnapshot operation creates a copy of an entire cache cluster at a
-- specific moment in time. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=CreateSnapshot &CacheClusterId=my-redis-primary
-- &SnapshotName=my-manual-snapshot &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- my-redis-primary 6379 cache.m1.small default.redis2.8 redis us-east-1d
-- 2014-04-01T18:46:57.972Z 2.8.6 manual true wed:09:00-wed:10:00
-- my-manual-snapshot 5 2014-04-01T18:46:57.972Z 0001 creating 1 07:30-08:30
-- faf5a232-b9ce-11e3-8a16-7978bb24ffdf.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.CreateSnapshot'
createSnapshot :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'cs1CacheClusterId'
               -> Text -- ^ 'cs1SnapshotName'
               -> State CreateSnapshot a
               -> m CreateSnapshotResponse
createSnapshot p1 p2 s =
    send $ (mkCreateSnapshot p1 p2) &~ s

-- | The DeleteCacheCluster operation deletes a previously provisioned cache
-- cluster. DeleteCacheCluster deletes all associated cache nodes, node
-- endpoints and the cache cluster itself. When you receive a successful
-- response from this operation, Amazon ElastiCache immediately begins
-- deleting the cache cluster; you cannot cancel or revert this operation.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=DeleteCacheCluster
-- &CacheClusterId=simcoprod43&Version=2014-03-24 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= in-sync default.memcached1.4 simcoprod43 deleting 11211
-- simcoprod43.m2st2p.cfg.cache.amazonaws.com cache.m1.large memcached
-- us-east-1b 2014-03-27T02:18:26.497Z 1.4.5 true mon:05:00-mon:05:30 default
-- active 3 ab84aa7e-b7fa-11e0-9b0b-a9261be2b354.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DeleteCacheCluster'
deleteCacheCluster :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'dccCacheClusterId'
                   -> State DeleteCacheCluster a
                   -> m DeleteCacheClusterResponse
deleteCacheCluster p1 s =
    send $ (mkDeleteCacheCluster p1) &~ s

-- | The DeleteCacheParameterGroup operation deletes the specified cache
-- parameter group. You cannot delete a cache parameter group if it is
-- associated with any cache clusters.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DeleteCacheParameterGroup &CacheParameterGroupName=myparametergroup
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential=
-- d0a417cb-575b-11e0-8869-cd22b4f9d96f.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DeleteCacheParameterGroup'
deleteCacheParameterGroup :: ( MonadCatch m
                             , MonadResource m
                             , MonadError Error m
                             , MonadReader Env m
                             , AWSRequest a
                             )
                          => Text -- ^ 'dcpgCacheParameterGroupName'
                          -> State DeleteCacheParameterGroup a
                          -> m DeleteCacheParameterGroupResponse
deleteCacheParameterGroup p1 s =
    send $ (mkDeleteCacheParameterGroup p1) &~ s

-- | The DeleteCacheSecurityGroup operation deletes a cache security group. You
-- cannot delete a cache security group if it is associated with any cache
-- clusters. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DeleteCacheSecurityGroup
-- &CacheSecurityGroupName=mycachesecuritygroup3 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= c130cfb7-3650-11e0-ae57-f96cfe56749c.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DeleteCacheSecurityGroup'
deleteCacheSecurityGroup :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'dcsgCacheSecurityGroupName'
                         -> State DeleteCacheSecurityGroup a
                         -> m DeleteCacheSecurityGroupResponse
deleteCacheSecurityGroup p1 s =
    send $ (mkDeleteCacheSecurityGroup p1) &~ s

-- | The DeleteCacheSubnetGroup operation deletes a cache subnet group. You
-- cannot delete a cache subnet group if it is associated with any cache
-- clusters. https://elasticache.amazonaws.com/ ?Action=DeleteCacheSubnetGroup
-- &CacheSubnetGroupName=mysubnetgroup &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- 5d013245-4172-11df-8520-e7e1e602a915.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DeleteCacheSubnetGroup'
deleteCacheSubnetGroup :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'dcsg1CacheSubnetGroupName'
                       -> State DeleteCacheSubnetGroup a
                       -> m DeleteCacheSubnetGroupResponse
deleteCacheSubnetGroup p1 s =
    send $ (mkDeleteCacheSubnetGroup p1) &~ s

-- | The DeleteReplicationGroup operation deletes an existing replication group.
-- By default, this operation deletes the entire replication group, including
-- the primary cache cluster and all of the read replicas. You can optionally
-- delete only the read replicas, while retaining the primary cache cluster.
-- When you receive a successful response from this operation, Amazon
-- ElastiCache immediately begins deleting the selected resources; you cannot
-- cancel or revert this operation.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=DeleteReplicationGroup
-- &RetainPrimaryCluster=false &FinalSnapshotIdentifier=my-final-snapshot
-- &ReplicationGroupId=my-repgroup &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- my-redis-primary my-repgroup deleting My replication group
-- 93eb37db-b9d7-11e3-8a16-7978bb24ffdf.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DeleteReplicationGroup'
deleteReplicationGroup :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'drgReplicationGroupId'
                       -> State DeleteReplicationGroup a
                       -> m DeleteReplicationGroupResponse
deleteReplicationGroup p1 s =
    send $ (mkDeleteReplicationGroup p1) &~ s

-- | The DeleteSnapshot operation deletes an existing snapshot. When you receive
-- a successful response from this operation, ElastiCache immediately begins
-- deleting the snapshot; you cannot cancel or revert this operation.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=DeleteSnapshot
-- &SnapshotName=my-manual-snapshot &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- my-redis-primary 6379 cache.m1.small default.redis2.8 redis us-east-1d
-- 2014-04-01T18:46:57.972Z 2.8.6 manual true wed:09:00-wed:10:00
-- my-manual-snapshot 5 2014-04-01T18:54:12Z 2014-04-01T18:46:57.972Z 0001 3
-- MB deleting 1 07:30-08:30 694d7017-b9d2-11e3-8a16-7978bb24ffdf.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DeleteSnapshot'
deleteSnapshot :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'dsSnapshotName'
               -> State DeleteSnapshot a
               -> m DeleteSnapshotResponse
deleteSnapshot p1 s =
    send $ (mkDeleteSnapshot p1) &~ s

-- | The DescribeCacheClusters operation returns information about all
-- provisioned cache clusters if no cache cluster identifier is specified, or
-- about a specific cache cluster if a cache cluster identifier is supplied.
-- By default, abbreviated information about the cache clusters(s) will be
-- returned. You can use the optional ShowDetails flag to retrieve detailed
-- information about the cache nodes associated with the cache clusters. These
-- details include the DNS address and port for the cache node endpoint. If
-- the cluster is in the CREATING state, only cluster level information will
-- be displayed until all of the nodes are successfully provisioned. If the
-- cluster is in the DELETING state, only cluster level information will be
-- displayed. If cache nodes are currently being added to the cache cluster,
-- node endpoint information and creation time for the additional nodes will
-- not be displayed until they are completely provisioned. When the cache
-- cluster state is available, the cluster is ready for use. If cache nodes
-- are currently being removed from the cache cluster, no endpoint information
-- for the removed nodes is displayed.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=DescribeCacheClusters
-- &MaxRecords=100 &ShowCacheNodeInfo=false &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= in-sync default.memcached1.4 simcoprod42 available 11211
-- simcoprod42.m2st2p.cfg.cache.amazonaws.com
-- https://console.aws.amazon.com/elasticache/home#client-download:
-- cache.m1.large memcached us-east-1d 2014-03-26T01:21:46.607Z 1.4.5 true
-- fri:08:30-fri:09:00 default active active
-- arn:aws:sns:us-east-1:123456789012:ElastiCacheNotifications 6
-- f270d58f-b7fb-11e0-9326-b7275b9d4a6c.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DescribeCacheClusters'
describeCacheClusters :: ( MonadCatch m
                         , MonadResource m
                         , MonadError Error m
                         , MonadReader Env (ResumableSource m)
                         , AWSPager a
                         )
                      => State DescribeCacheClusters a
                      -> ResumableSource m DescribeCacheClustersResponse
describeCacheClusters s =
    paginate (mkDescribeCacheClusters &~ s)

-- | The DescribeCacheEngineVersions operation returns a list of the available
-- cache engines and their versions.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheEngineVersions &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= memcached1.4 memcached memcached version 1.4.14
-- memcached 1.4.14 memcached1.4 memcached memcached version 1.4.5 memcached
-- 1.4.5 a6ac9ad2-f8a4-11e1-a4d1-a345e5370093.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DescribeCacheEngineVersions'
describeCacheEngineVersions :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env (ResumableSource m)
                               , AWSPager a
                               )
                            => State DescribeCacheEngineVersions a
                            -> ResumableSource m DescribeCacheEngineVersionsResponse
describeCacheEngineVersions s =
    paginate (mkDescribeCacheEngineVersions &~ s)

-- | The DescribeCacheParameterGroups operation returns a list of cache
-- parameter group descriptions. If a cache parameter group name is specified,
-- the list will contain only the descriptions for that group.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheParameterGroups &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= default.memcached1.4 memcached1.4 Default parameter
-- group for memcached1.4 mycacheparametergroup memcached1.4 My cache
-- parameter group mycacheparametergroup1 memcached1.4 My first cache
-- parameter group mycacheparametergroup3 memcached1.4 My first cache
-- parameter group 7193fbb8-b7fc-11e0-9b0b-a9261be2b354.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DescribeCacheParameterGroups'
describeCacheParameterGroups :: ( MonadCatch m
                                , MonadResource m
                                , MonadError Error m
                                , MonadReader Env (ResumableSource m)
                                , AWSPager a
                                )
                             => State DescribeCacheParameterGroups a
                             -> ResumableSource m DescribeCacheParameterGroupsResponse
describeCacheParameterGroups s =
    paginate (mkDescribeCacheParameterGroups &~ s)

-- | The DescribeCacheParameters operation returns the detailed parameter list
-- for a particular cache parameter group. Some of the output has been omitted
-- for brevity. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheParameters
-- &CacheParameterGroupName=default.memcached1.4 &MaxRecords=100
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= 1-07-15/"> cache.c1.xlarge
-- 6000 (...output omitted...) integer system false The maximum configurable
-- amount of memory to use to store items, in megabytes. 1-100000
-- max_cache_memory 1.4.5 (...output omitted...) 1024 integer system false The
-- backlog queue limit. 1-10000 backlog_queue_limit 1.4.5 (...output
-- omitted...) 0c507368-b7fe-11e0-9326-b7275b9d4a6c.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DescribeCacheParameters'
describeCacheParameters :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env (ResumableSource m)
                           , AWSPager a
                           )
                        => Text -- ^ 'dcpCacheParameterGroupName'
                        -> State DescribeCacheParameters a
                        -> ResumableSource m DescribeCacheParametersResponse
describeCacheParameters p1 s =
    paginate $ (mkDescribeCacheParameters p1) &~ s

-- | The DescribeCacheSecurityGroups operation returns a list of cache security
-- group descriptions. If a cache security group name is specified, the list
-- will contain only the description of that group.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheSecurityGroups &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= default 123456789012 default mycachesecuritygroup
-- 123456789012 My Security Group a95360ae-b7fc-11e0-9326-b7275b9d4a6c.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DescribeCacheSecurityGroups'
describeCacheSecurityGroups :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env (ResumableSource m)
                               , AWSPager a
                               )
                            => State DescribeCacheSecurityGroups a
                            -> ResumableSource m DescribeCacheSecurityGroupsResponse
describeCacheSecurityGroups s =
    paginate (mkDescribeCacheSecurityGroups &~ s)

-- | The DescribeCacheSubnetGroups operation returns a list of cache subnet
-- group descriptions. If a subnet group name is specified, the list will
-- contain only the description of that group. Some of the output has been
-- omitted for brevity. https://elasticache.amazonaws.com/
-- ?Action=DescribeCacheSubnetGroups &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- 990524496922 description subnet_grp1 Active subnet-7c5b4115 us-east-1c
-- Active subnet-7b5b4112 us-east-1b Active subnet-3ea6bd57 us-east-1d
-- (...output omitted...) 31d0faee-229b-11e1-81f1-df3a2a803dad.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DescribeCacheSubnetGroups'
describeCacheSubnetGroups :: ( MonadCatch m
                             , MonadResource m
                             , MonadError Error m
                             , MonadReader Env (ResumableSource m)
                             , AWSPager a
                             )
                          => State DescribeCacheSubnetGroups a
                          -> ResumableSource m DescribeCacheSubnetGroupsResponse
describeCacheSubnetGroups s =
    paginate (mkDescribeCacheSubnetGroups &~ s)

-- | The DescribeEngineDefaultParameters operation returns the default engine
-- and system parameter information for the specified cache engine. Some of
-- the output has been omitted for brevity.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeEngineDefaultParameters
-- &CacheParameterGroupFamily=memcached1.4 &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= memcached1.4 1024 integer system false The backlog queue
-- limit. 1-10000 backlog_queue_limit 1.4.5 (...output omitted...)
-- cache.c1.xlarge 6000 (...output omitted...) integer system false The
-- maximum configurable amount of memory to use to store items, in megabytes.
-- 1-100000 max_cache_memory 1.4.5 (...output omitted...)
-- 061282fe-b7fd-11e0-9326-b7275b9d4a6c.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DescribeEngineDefaultParameters'
describeEngineDefaultParameters :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadError Error m
                                   , MonadReader Env (ResumableSource m)
                                   , AWSPager a
                                   )
                                => Text -- ^ 'dedpCacheParameterGroupFamily'
                                -> State DescribeEngineDefaultParameters a
                                -> ResumableSource m DescribeEngineDefaultParametersResponse
describeEngineDefaultParameters p1 s =
    paginate $ (mkDescribeEngineDefaultParameters p1) &~ s

-- | The DescribeEvents operation returns events related to cache clusters,
-- cache security groups, and cache parameter groups. You can obtain events
-- specific to a particular cache cluster, cache security group, or cache
-- parameter group by providing the name as a parameter. By default, only the
-- events occurring within the last hour are returned; however, you can
-- retrieve up to 14 days' worth of events if necessary. Some of the output
-- has been omitted for brevity. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeEvents &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= Cache cluster created cache-cluster
-- 2014-04-01T18:22:18.202Z my-redis-primary (...output omitted...)
-- e21c81b4-b9cd-11e3-8a16-7978bb24ffdf.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DescribeEvents'
describeEvents :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env (ResumableSource m)
                  , AWSPager a
                  )
               => State DescribeEvents a
               -> ResumableSource m DescribeEventsResponse
describeEvents s =
    paginate (mkDescribeEvents &~ s)

-- | The DescribeReplicationGroups operation returns information about a
-- particular replication group. If no identifier is specified,
-- DescribeReplicationGroups returns information about all replication groups.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeReplicationGroups &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= my-redis-primary my-redis-primary 0001 6379
-- my-repgroup.q68zge.ng.0001.use1devo.elmo-dev.amazonaws.com available
-- my-redis-primary 6379
-- my-redis-primary.q68zge.0001.use1devo.elmo-dev.amazonaws.com us-east-1d
-- 0001 primary my-repgroup available My replication group
-- 144745b0-b9d3-11e3-8a16-7978bb24ffdf.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DescribeReplicationGroups'
describeReplicationGroups :: ( MonadCatch m
                             , MonadResource m
                             , MonadError Error m
                             , MonadReader Env (ResumableSource m)
                             , AWSPager a
                             )
                          => State DescribeReplicationGroups a
                          -> ResumableSource m DescribeReplicationGroupsResponse
describeReplicationGroups s =
    paginate (mkDescribeReplicationGroups &~ s)

-- | The DescribeReservedCacheNodes operation returns information about reserved
-- cache nodes for this account, or about a specified reserved cache node.
-- https://elasticache.amazonaws.com/ ?Action=DescribeReservedCacheNodes
-- &ReservedCacheNodeId=customerSpecifiedID &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= Medium Utilization memcached
-- 649fd0c8-cf6d-47a0-bfa6-060f8e75e95f payment-failed myreservationid 1
-- 2010-12-15T00:25:14.131Z 31536000 227.5 0.046 cache.m1.small
-- c695119b-2961-11e1-bd06-6fe008f046c3.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DescribeReservedCacheNodes'
describeReservedCacheNodes :: ( MonadCatch m
                              , MonadResource m
                              , MonadError Error m
                              , MonadReader Env (ResumableSource m)
                              , AWSPager a
                              )
                           => State DescribeReservedCacheNodes a
                           -> ResumableSource m DescribeReservedCacheNodesResponse
describeReservedCacheNodes s =
    paginate (mkDescribeReservedCacheNodes &~ s)

-- | The DescribeReservedCacheNodesOfferings operation lists available reserved
-- cache node offerings. https://elasticache.amazonaws.com/
-- ?Action=DescribeReservedCacheNodesOfferings
-- &ReservedCacheNodesOfferingId=438012d3-4052-4cc7-b2e3-8d3372e0e706
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= 31536000 Heavy Utilization
-- Hourly 0.123 162.0 memcached 0.0 SampleOfferingId cache.m1.small
-- 521b420a-2961-11e1-bd06-6fe008f046c3.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DescribeReservedCacheNodesOfferings'
describeReservedCacheNodesOfferings :: ( MonadCatch m
                                       , MonadResource m
                                       , MonadError Error m
                                       , MonadReader Env (ResumableSource m)
                                       , AWSPager a
                                       )
                                    => State DescribeReservedCacheNodesOfferings a
                                    -> ResumableSource m DescribeReservedCacheNodesOfferingsResponse
describeReservedCacheNodesOfferings s =
    paginate (mkDescribeReservedCacheNodesOfferings &~ s)

-- | The DescribeSnapshots operation returns information about cache cluster
-- snapshots. By default, DescribeSnapshots lists all of your snapshots; it
-- can optionally describe a single snapshot, or just the snapshots associated
-- with a particular cache cluster.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=DescribeSnapshots
-- &MaxRecords=50 &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- my-redis-primary 6379 cache.m1.small default.redis2.8 redis us-east-1d
-- 2014-04-01T18:46:57.972Z 2.8.6 manual true wed:09:00-wed:10:00
-- my-manual-snapshot 5 2014-04-01T18:54:12Z 2014-04-01T18:46:57.972Z 0001 3
-- MB creating 1 07:30-08:30 51b0b25e-b9cf-11e3-8a16-7978bb24ffdf.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.DescribeSnapshots'
describeSnapshots :: ( MonadCatch m
                     , MonadResource m
                     , MonadError Error m
                     , MonadReader Env m
                     , AWSRequest a
                     )
                  => State DescribeSnapshots a
                  -> m DescribeSnapshotsResponse
describeSnapshots s =
    send (mkDescribeSnapshots &~ s)

-- | The ModifyCacheCluster operation modifies the settings for a cache cluster.
-- You can use this operation to change one or more cluster configuration
-- parameters by specifying the parameters and the new values.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=ModifyCacheCluster
-- &NumCacheNodes=5 &CacheClusterId=simcoprod01 &ApplyImmediately=true
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= in-sync default.memcached1.4
-- simcoprod01 available 11211 simcoprod01.m2st2p.cfg.cache.amazonaws.com
-- cache.m1.large memcached 5 us-east-1b 2014-03-26T23:45:20.937Z 1.4.5 true
-- fri:04:30-fri:05:00 default active 3 d5786c6d-b7fe-11e0-9326-b7275b9d4a6c.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.ModifyCacheCluster'
modifyCacheCluster :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'mccCacheClusterId'
                   -> State ModifyCacheCluster a
                   -> m ModifyCacheClusterResponse
modifyCacheCluster p1 s =
    send $ (mkModifyCacheCluster p1) &~ s

-- | The ModifyCacheParameterGroup operation modifies the parameters of a cache
-- parameter group. You can modify up to 20 parameters in a single request by
-- submitting a list parameter name and value pairs.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=ModifyCacheParameterGroup
-- ?ParameterNameValues.member.1.ParameterName=chunk_size_growth_factor
-- &ParameterNameValues.member.1.ParameterValue=1.02
-- &CacheParameterGroupName=mycacheparametergroup &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= mycacheparametergroup
-- fcedeef2-b7ff-11e0-9326-b7275b9d4a6c.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.ModifyCacheParameterGroup'
modifyCacheParameterGroup :: ( MonadCatch m
                             , MonadResource m
                             , MonadError Error m
                             , MonadReader Env m
                             , AWSRequest a
                             )
                          => Text -- ^ 'mcpgCacheParameterGroupName'
                          -> [ParameterNameValue] -- ^ 'mcpgParameterNameValues'
                          -> State ModifyCacheParameterGroup a
                          -> m ModifyCacheParameterGroupResponse
modifyCacheParameterGroup p1 p2 s =
    send $ (mkModifyCacheParameterGroup p1 p2) &~ s

-- | The ModifyCacheSubnetGroup operation modifies an existing cache subnet
-- group. https://elasticache.amazonaws.com/ ?Action=ModifyCacheSubnetGroup
-- &CacheSubnetGroupName=myCachesubnetgroup
-- &CacheSubnetGroupDescription=My%20modified%20CacheSubnetGroup
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= 990524496922 My modified
-- CacheSubnetGroup myCachesubnetgroup Active subnet-7c5b4115 us-east-1c
-- Active subnet-7b5b4112 us-east-1b Active subnet-3ea6bd57 us-east-1d
-- ed662948-a57b-11df-9e38-7ffab86c801f.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.ModifyCacheSubnetGroup'
modifyCacheSubnetGroup :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'mcsgCacheSubnetGroupName'
                       -> State ModifyCacheSubnetGroup a
                       -> m ModifyCacheSubnetGroupResponse
modifyCacheSubnetGroup p1 s =
    send $ (mkModifyCacheSubnetGroup p1) &~ s

-- | The ModifyReplicationGroup operation modifies the settings for a
-- replication group. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=ModifyReplicationGroup &ApplyImmediately=false
-- &ReplicationGroupId=my-repgroup &PrimaryClusterId=my-replica-1
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= my-redis-primary
-- my-redis-primary my-replica-1 0001 6379
-- my-repgroup.q68zge.ng.0001.use1devo.elmo-dev.amazonaws.com available
-- my-redis-primary 6379
-- my-redis-primary.q68zge.0001.use1devo.elmo-dev.amazonaws.com us-east-1d
-- 0001 primary my-replica-1 6379
-- my-replica-1.q68zge.0001.use1devo.elmo-dev.amazonaws.com us-east-1e 0001
-- replica my-repgroup available my-replica-1 My replication group
-- 6fd0aad6-b9d7-11e3-8a16-7978bb24ffdf.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.ModifyReplicationGroup'
modifyReplicationGroup :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'mrgReplicationGroupId'
                       -> State ModifyReplicationGroup a
                       -> m ModifyReplicationGroupResponse
modifyReplicationGroup p1 s =
    send $ (mkModifyReplicationGroup p1) &~ s

-- | The PurchaseReservedCacheNodesOffering operation allows you to purchase a
-- reserved cache node offering. https://elasticache.amazonaws.com/
-- ?Action=PurchaseReservedCacheNodesOffering
-- &ReservedCacheNodeId=myreservationID
-- &ReservedCacheNodesOfferingId=438012d3-4052-4cc7-b2e3-8d3372e0e706
-- &CacheNodeCount=1 &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- Medium Utilization memcached 438012d3-4052-4cc7-b2e3-8d3372e0e706
-- payment-pending myreservationID 10 2014-03-18T23:24:56.577Z 31536000 123.0
-- 0.123 cache.m1.small 7f099901-29cf-11e1-bd06-6fe008f046c3.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.PurchaseReservedCacheNodesOffering'
purchaseReservedCacheNodesOffering :: ( MonadCatch m
                                      , MonadResource m
                                      , MonadError Error m
                                      , MonadReader Env m
                                      , AWSRequest a
                                      )
                                   => Text -- ^ 'prcnoReservedCacheNodesOfferingId'
                                   -> State PurchaseReservedCacheNodesOffering a
                                   -> m PurchaseReservedCacheNodesOfferingResponse
purchaseReservedCacheNodesOffering p1 s =
    send $ (mkPurchaseReservedCacheNodesOffering p1) &~ s

-- | The RebootCacheCluster operation reboots some, or all, of the cache nodes
-- within a provisioned cache cluster. This API will apply any modified cache
-- parameter groups to the cache cluster. The reboot action takes place as
-- soon as possible, and results in a momentary outage to the cache cluster.
-- During the reboot, the cache cluster status is set to REBOOTING. The reboot
-- causes the contents of the cache (for each cache node being rebooted) to be
-- lost. When the reboot is complete, a cache cluster event is created.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=RebootCacheCluster
-- &CacheClusterId=mycache &CacheNodeIdsToReboot.member.1=0001
-- &CacheNodeIdsToReboot.member.2=0002 &CacheNodeIdsToReboot.member.3=0003
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= rebooting cache cluster
-- nodes default.memcached1.4 in-sync mycache 11211
-- mycache.q68zge.cfg.use1devo.elmo-dev.amazonaws.com cache.m1.small memcached
-- us-east-1b 2014-04-01T19:04:12.812Z 1.4.17 true wed:09:00-wed:10:00
-- https://console.aws.amazon.com/elasticache/home#client-download: default
-- active 3 cf7e6fc4-b9d1-11e3-8a16-7978bb24ffdf.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.RebootCacheCluster'
rebootCacheCluster :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'rccCacheClusterId'
                   -> [Text] -- ^ 'rccCacheNodeIdsToReboot'
                   -> State RebootCacheCluster a
                   -> m RebootCacheClusterResponse
rebootCacheCluster p1 p2 s =
    send $ (mkRebootCacheCluster p1 p2) &~ s

-- | The ResetCacheParameterGroup operation modifies the parameters of a cache
-- parameter group to the engine or system default value. You can reset
-- specific parameters by submitting a list of parameter names. To reset the
-- entire cache parameter group, specify the ResetAllParameters and
-- CacheParameterGroupName parameters.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=ResetCacheParameterGroup &ResetAllParameters=true
-- &CacheParameterGroupName=mycacheparametergroup1 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= mycacheparametergroup1
-- cb7cc855-b9d2-11e3-8a16-7978bb24ffdf.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.ResetCacheParameterGroup'
resetCacheParameterGroup :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'rcpgCacheParameterGroupName'
                         -> [ParameterNameValue] -- ^ 'rcpgParameterNameValues'
                         -> State ResetCacheParameterGroup a
                         -> m ResetCacheParameterGroupResponse
resetCacheParameterGroup p1 p3 s =
    send $ (mkResetCacheParameterGroup p1 p3) &~ s

-- | The RevokeCacheSecurityGroupIngress operation revokes ingress from a cache
-- security group. Use this operation to disallow access from an Amazon EC2
-- security group that had been previously authorized.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=RevokeCacheSecurityGroupIngress &EC2SecurityGroupName=default
-- &CacheSecurityGroupName=mygroup &EC2SecurityGroupOwnerId=1234-5678-1234
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= revoking default
-- 123456781234 mygroup 123456789012 My security group
-- 02ae3699-3650-11e0-a564-8f11342c56b0.
--
-- See: 'Network.AWS.ElastiCache.V2014_07_15.RevokeCacheSecurityGroupIngress'
revokeCacheSecurityGroupIngress :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadError Error m
                                   , MonadReader Env m
                                   , AWSRequest a
                                   )
                                => Text -- ^ 'rcsgiCacheSecurityGroupName'
                                -> Text -- ^ 'rcsgiEC2SecurityGroupName'
                                -> Text -- ^ 'rcsgiEC2SecurityGroupOwnerId'
                                -> State RevokeCacheSecurityGroupIngress a
                                -> m RevokeCacheSecurityGroupIngressResponse
revokeCacheSecurityGroupIngress p1 p2 p3 s =
    send $ (mkRevokeCacheSecurityGroupIngress p1 p2 p3) &~ s
