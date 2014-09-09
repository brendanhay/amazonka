{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Redshift is a fast, fully managed, petabyte-scale data warehouse
-- service that makes it simple and cost-effective to efficiently analyze all
-- your data using your existing business intelligence tools. You can start
-- small for just $0.25 per hour with no commitments or upfront costs and
-- scale to a petabyte or more for $1,000 per terabyte per year, less than a
-- tenth of most other data warehousing solutions.
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.Redshift.V2012_12_01.Trans
    (
    -- * AuthorizeClusterSecurityGroupIngress
      authorizeClusterSecurityGroupIngress
    -- * AuthorizeSnapshotAccess
    , authorizeSnapshotAccess
    -- * CopyClusterSnapshot
    , copyClusterSnapshot
    -- * CreateCluster
    , createCluster
    -- * CreateClusterParameterGroup
    , createClusterParameterGroup
    -- * CreateClusterSecurityGroup
    , createClusterSecurityGroup
    -- * CreateClusterSnapshot
    , createClusterSnapshot
    -- * CreateClusterSubnetGroup
    , createClusterSubnetGroup
    -- * CreateEventSubscription
    , createEventSubscription
    -- * CreateHsmClientCertificate
    , createHsmClientCertificate
    -- * CreateHsmConfiguration
    , createHsmConfiguration
    -- * DeleteCluster
    , deleteCluster
    -- * DeleteClusterParameterGroup
    , deleteClusterParameterGroup
    -- * DeleteClusterSecurityGroup
    , deleteClusterSecurityGroup
    -- * DeleteClusterSnapshot
    , deleteClusterSnapshot
    -- * DeleteClusterSubnetGroup
    , deleteClusterSubnetGroup
    -- * DeleteEventSubscription
    , deleteEventSubscription
    -- * DeleteHsmClientCertificate
    , deleteHsmClientCertificate
    -- * DeleteHsmConfiguration
    , deleteHsmConfiguration
    -- * DescribeClusterParameterGroups
    , describeClusterParameterGroups
    -- * DescribeClusterParameters
    , describeClusterParameters
    -- * DescribeClusterSecurityGroups
    , describeClusterSecurityGroups
    -- * DescribeClusterSnapshots
    , describeClusterSnapshots
    -- * DescribeClusterSubnetGroups
    , describeClusterSubnetGroups
    -- * DescribeClusterVersions
    , describeClusterVersions
    -- * DescribeClusters
    , describeClusters
    -- * DescribeDefaultClusterParameters
    , describeDefaultClusterParameters
    -- * DescribeEventCategories
    , describeEventCategories
    -- * DescribeEventSubscriptions
    , describeEventSubscriptions
    -- * DescribeEvents
    , describeEvents
    -- * DescribeHsmClientCertificates
    , describeHsmClientCertificates
    -- * DescribeHsmConfigurations
    , describeHsmConfigurations
    -- * DescribeLoggingStatus
    , describeLoggingStatus
    -- * DescribeOrderableClusterOptions
    , describeOrderableClusterOptions
    -- * DescribeReservedNodeOfferings
    , describeReservedNodeOfferings
    -- * DescribeReservedNodes
    , describeReservedNodes
    -- * DescribeResize
    , describeResize
    -- * DisableLogging
    , disableLogging
    -- * DisableSnapshotCopy
    , disableSnapshotCopy
    -- * EnableLogging
    , enableLogging
    -- * EnableSnapshotCopy
    , enableSnapshotCopy
    -- * ModifyCluster
    , modifyCluster
    -- * ModifyClusterParameterGroup
    , modifyClusterParameterGroup
    -- * ModifyClusterSubnetGroup
    , modifyClusterSubnetGroup
    -- * ModifyEventSubscription
    , modifyEventSubscription
    -- * ModifySnapshotCopyRetentionPeriod
    , modifySnapshotCopyRetentionPeriod
    -- * PurchaseReservedNodeOffering
    , purchaseReservedNodeOffering
    -- * RebootCluster
    , rebootCluster
    -- * ResetClusterParameterGroup
    , resetClusterParameterGroup
    -- * RestoreFromClusterSnapshot
    , restoreFromClusterSnapshot
    -- * RevokeClusterSecurityGroupIngress
    , revokeClusterSecurityGroupIngress
    -- * RevokeSnapshotAccess
    , revokeSnapshotAccess
    -- * RotateEncryptionKey
    , rotateEncryptionKey

    -- * Re-exported
    , module Control.Monad.Trans.AWS
    , module Network.AWS.Redshift.V2012_12_01
    ) where

import Control.Monad.Trans.AWS
import Network.AWS.Prelude
import Network.AWS.Redshift.V2012_12_01

-- | Adds an inbound (ingress) rule to an Amazon Redshift security group.
-- Depending on whether the application accessing your cluster is running on
-- the Internet or an EC2 instance, you can authorize inbound access to either
-- a Classless Interdomain Routing (CIDR) IP address range or an EC2 security
-- group. EC2SecurityGroupName and EC2SecurityGroupOwnerId --> You can add as
-- many as 20 ingress rules to an Amazon Redshift security group. The EC2
-- security group must be defined in the AWS region where the cluster resides.
-- For an overview of CIDR blocks, see the Wikipedia article on Classless
-- Inter-Domain Routing. You must also associate the security group with a
-- cluster so that clients running on these IP addresses or the EC2 instance
-- are authorized to connect to the cluster. For information about managing
-- security groups, go to Working with Security Groups in the Amazon Redshift
-- Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=AuthorizeClusterSecurityGroupIngress &CIDRIP=192.168.40.3/32
-- &ClusterSecurityGroupName=securitygroup1 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T020649Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 192.168.40.3/32
-- authorized my security group securitygroup1
-- 8c7cd4c8-6501-11e2-a8da-655adc216806.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.AuthorizeClusterSecurityGroupIngress'
authorizeClusterSecurityGroupIngress :: ( MonadCatch m
                                        , MonadResource m
                                        , MonadError Error m
                                        , MonadReader Env m
                                        , AWSRequest a
                                        )
                                     => Text -- ^ 'acsgiClusterSecurityGroupName'
                                     -> State AuthorizeClusterSecurityGroupIngress a
                                     -> m AuthorizeClusterSecurityGroupIngressResponse
authorizeClusterSecurityGroupIngress p1 s =
    send $ (mkAuthorizeClusterSecurityGroupIngress p1) &~ s

-- | Authorizes the specified AWS customer account to restore the specified
-- snapshot. For more information about working with snapshots, go to Amazon
-- Redshift Snapshots in the Amazon Redshift Management Guide.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.AuthorizeSnapshotAccess'
authorizeSnapshotAccess :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'asaSnapshotIdentifier'
                        -> Text -- ^ 'asaAccountWithRestoreAccess'
                        -> State AuthorizeSnapshotAccess a
                        -> m AuthorizeSnapshotAccessResponse
authorizeSnapshotAccess p1 p3 s =
    send $ (mkAuthorizeSnapshotAccess p1 p3) &~ s

-- | Copies the specified automated cluster snapshot to a new manual cluster
-- snapshot. The source must be an automated snapshot and it must be in the
-- available state. When you delete a cluster, Amazon Redshift deletes any
-- automated snapshots of the cluster. Also, when the retention period of the
-- snapshot expires, Amazon Redshift automatically deletes it. If you want to
-- keep an automated snapshot for a longer period, you can make a manual copy
-- of the snapshot. Manual snapshots are retained until you delete them. For
-- more information about working with snapshots, go to Amazon Redshift
-- Snapshots in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=CopyClusterSnapshot
-- &SourceSnapshotIdentifier=cm:examplecluster-2013-01-22-19-27-58
-- &TargetSnapshotIdentifier=my-snapshot-456 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T014618Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 5439 my-snapshot-456
-- available manual 1.0 2013-01-22T19:27:58.931Z 2 dev
-- 2013-01-22T19:23:59.368Z us-east-1c dw1.xlarge examplecluster adminuser
-- aebb56f5-64fe-11e2-88c5-53eb05787dfb.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.CopyClusterSnapshot'
copyClusterSnapshot :: ( MonadCatch m
                       , MonadResource m
                       , MonadError Error m
                       , MonadReader Env m
                       , AWSRequest a
                       )
                    => Text -- ^ 'ccsSourceSnapshotIdentifier'
                    -> Text -- ^ 'ccsTargetSnapshotIdentifier'
                    -> State CopyClusterSnapshot a
                    -> m CopyClusterSnapshotResponse
copyClusterSnapshot p1 p3 s =
    send $ (mkCopyClusterSnapshot p1 p3) &~ s

-- | Creates a new cluster. To create the cluster in virtual private cloud
-- (VPC), you must provide cluster subnet group name. If you don't provide a
-- cluster subnet group name or the cluster security group parameter, Amazon
-- Redshift creates a non-VPC cluster, it associates the default cluster
-- security group with the cluster. For more information about managing
-- clusters, go to Amazon Redshift Clusters in the Amazon Redshift Management
-- Guide . Create a non-VPC cluster. https://redshift.us-east-1.amazonaws.com/
-- ?Action=CreateCluster &ClusterIdentifier=examplecluster
-- &MasterUsername=masteruser &MasterUserPassword=12345678Aa &NumberOfNodes=2
-- &NodeType=dw1.xlarge &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T000028Z
-- &x-amz-signedheaders=content-type;host;x-amz-date **** 1.0 creating 2 1
-- true false dev sun:10:30-sun:11:00 in-sync default.redshift-1.0 active
-- default dw1.xlarge examplecluster true masteruser
-- e69b1294-64ef-11e2-b07c-f7fbdd006c67 Create cluster in virtual private
-- cloud (VPC). This example request specifies a ClusterSubnetGroup in the
-- request. https://redshift.us-east-1.amazonaws.com/ ?Action=CreateCluster
-- &ClusterIdentifier=exampleclusterinvpc &MasterUsername=master
-- &MasterUserPassword=1234abcdA &NodeType=dw1.xlarge &NumberOfNodes=2
-- &ClusterSubnetGroupName=mysubnetgroup1 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T000028Z
-- &x-amz-signedheaders=content-type;host;x-amz-date **** mysubnetgroup1 1.0
-- creating 2 1 false false dev sat:08:30-sat:09:00 in-sync
-- default.redshift-1.0 vpc-796a5913 dw1.xlarge exampleclusterinvpc true
-- master fa337bb4-6a4d-11e2-a12a-cb8076a904bd.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.CreateCluster'
createCluster :: ( MonadCatch m
                 , MonadResource m
                 , MonadError Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'ccClusterIdentifier'
              -> Text -- ^ 'ccNodeType'
              -> Text -- ^ 'ccMasterUsername'
              -> Text -- ^ 'ccMasterUserPassword'
              -> State CreateCluster a
              -> m CreateClusterResponse
createCluster p2 p4 p5 p6 s =
    send $ (mkCreateCluster p2 p4 p5 p6) &~ s

-- | Creates an Amazon Redshift parameter group. Creating parameter groups is
-- independent of creating clusters. You can associate a cluster with a
-- parameter group when you create the cluster. You can also associate an
-- existing cluster with a parameter group after the cluster is created by
-- using ModifyCluster. Parameters in the parameter group define specific
-- behavior that applies to the databases you create on the cluster. For more
-- information about managing parameter groups, go to Amazon Redshift
-- Parameter Groups in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=CreateClusterParameterGroup &Description=description my parameter
-- group &ParameterGroupFamily=redshift-1.0
-- &ParameterGroupName=parametergroup1 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T002544Z
-- &x-amz-signedheaders=content-type;host;x-amz-date redshift-1.0 description
-- my parameter group parametergroup1 6d6df847-64f3-11e2-bea9-49e0ce183f07.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.CreateClusterParameterGroup'
createClusterParameterGroup :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => Text -- ^ 'ccpgParameterGroupName'
                            -> Text -- ^ 'ccpgParameterGroupFamily'
                            -> Text -- ^ 'ccpgDescription'
                            -> State CreateClusterParameterGroup a
                            -> m CreateClusterParameterGroupResponse
createClusterParameterGroup p1 p2 p3 s =
    send $ (mkCreateClusterParameterGroup p1 p2 p3) &~ s

-- | Creates a new Amazon Redshift security group. You use security groups to
-- control access to non-VPC clusters. For information about managing security
-- groups, go to Amazon Redshift Cluster Security Groups in the Amazon
-- Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=CreateClusterSecurityGroup &ClusterSecurityGroupName=securitygroup1
-- &Description=my security group &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T005817Z
-- &x-amz-signedheaders=content-type;host;x-amz-date my security group
-- securitygroup1 f9ee270f-64f7-11e2-a8da-655adc216806.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.CreateClusterSecurityGroup'
createClusterSecurityGroup :: ( MonadCatch m
                              , MonadResource m
                              , MonadError Error m
                              , MonadReader Env m
                              , AWSRequest a
                              )
                           => Text -- ^ 'ccsgClusterSecurityGroupName'
                           -> Text -- ^ 'ccsgDescription'
                           -> State CreateClusterSecurityGroup a
                           -> m CreateClusterSecurityGroupResponse
createClusterSecurityGroup p1 p2 s =
    send $ (mkCreateClusterSecurityGroup p1 p2) &~ s

-- | Creates a manual snapshot of the specified cluster. The cluster must be in
-- the available state. For more information about working with snapshots, go
-- to Amazon Redshift Snapshots in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=CreateClusterSnapshot
-- &ClusterIdentifier=examplecluster &SnapshotIdentifier=snapshot-1234
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T010824Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 5439 my-snapshot-123
-- creating manual 1.0 2013-01-23T01:08:29.142Z 2 dev 2013-01-22T19:23:59.368Z
-- us-east-1c dw1.xlarge examplecluster adminuser
-- 65baef14-64f9-11e2-bea9-49e0ce183f07.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.CreateClusterSnapshot'
createClusterSnapshot :: ( MonadCatch m
                         , MonadResource m
                         , MonadError Error m
                         , MonadReader Env m
                         , AWSRequest a
                         )
                      => Text -- ^ 'ccs1SnapshotIdentifier'
                      -> Text -- ^ 'ccs1ClusterIdentifier'
                      -> State CreateClusterSnapshot a
                      -> m CreateClusterSnapshotResponse
createClusterSnapshot p1 p2 s =
    send $ (mkCreateClusterSnapshot p1 p2) &~ s

-- | Creates a new Amazon Redshift subnet group. You must provide a list of one
-- or more subnets in your existing Amazon Virtual Private Cloud (Amazon VPC)
-- when creating Amazon Redshift subnet group. For information about subnet
-- groups, go to Amazon Redshift Cluster Subnet Groups in the Amazon Redshift
-- Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=CreateClusterSubnetGroup &ClusterSubnetGroupName=mysubnetgroup1
-- &Description=My subnet group 1 &SubnetIds.member.1=subnet-756a591f
-- &SubnetIds.member.1=subnet-716a591b &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130129/us-east-1/redshift/aws4_request
-- &x-amz-date=20130129T192820Z
-- &x-amz-signedheaders=content-type;host;x-amz-date vpc-796a5913 My subnet
-- group 1 mysubnetgroup1 Complete Active subnet-756a591f us-east-1c
-- 0a60660f-6a4a-11e2-aad2-71d00c36728e.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.CreateClusterSubnetGroup'
createClusterSubnetGroup :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'ccsg1ClusterSubnetGroupName'
                         -> Text -- ^ 'ccsg1Description'
                         -> [Text] -- ^ 'ccsg1SubnetIds'
                         -> State CreateClusterSubnetGroup a
                         -> m CreateClusterSubnetGroupResponse
createClusterSubnetGroup p1 p2 p3 s =
    send $ (mkCreateClusterSubnetGroup p1 p2 p3) &~ s

-- | Creates an Amazon Redshift event notification subscription. This action
-- requires an ARN (Amazon Resource Name) of an Amazon SNS topic created by
-- either the Amazon Redshift console, the Amazon SNS console, or the Amazon
-- SNS API. To obtain an ARN with Amazon SNS, you must create a topic in
-- Amazon SNS and subscribe to the topic. The ARN is displayed in the SNS
-- console. You can specify the source type, and lists of Amazon Redshift
-- source IDs, event categories, and event severities. Notifications will be
-- sent for all events you want that match those criteria. For example, you
-- can specify source type = cluster, source ID = my-cluster-1 and mycluster2,
-- event categories = Availability, Backup, and severity = ERROR. The
-- subscription will only send notifications for those ERROR events in the
-- Availability and Backup categories for the specified clusters. If you
-- specify both the source type and source IDs, such as source type = cluster
-- and source identifier = my-cluster-1, notifications will be sent for all
-- the cluster events for my-cluster-1. If you specify a source type but do
-- not specify a source identifier, you will receive notice of the events for
-- the objects of that type in your AWS account. If you do not specify either
-- the SourceType nor the SourceIdentifier, you will be notified of events
-- generated from all Amazon Redshift sources belonging to your AWS account.
-- You must specify a source type if you specify a source ID.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.CreateEventSubscription'
createEventSubscription :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'cesSubscriptionName'
                        -> Text -- ^ 'cesSnsTopicArn'
                        -> State CreateEventSubscription a
                        -> m CreateEventSubscriptionResponse
createEventSubscription p1 p2 s =
    send $ (mkCreateEventSubscription p1 p2) &~ s

-- | Creates an HSM client certificate that an Amazon Redshift cluster will use
-- to connect to the client's HSM in order to store and retrieve the keys used
-- to encrypt the cluster databases. The command returns a public key, which
-- you must store in the HSM. In addition to creating the HSM certificate, you
-- must create an Amazon Redshift HSM configuration that provides a cluster
-- the information needed to store and use encryption keys in the HSM. For
-- more information, go to Hardware Security Modules in the Amazon Redshift
-- Management Guide.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.CreateHsmClientCertificate'
createHsmClientCertificate :: ( MonadCatch m
                              , MonadResource m
                              , MonadError Error m
                              , MonadReader Env m
                              , AWSRequest a
                              )
                           => Text -- ^ 'chccHsmClientCertificateIdentifier'
                           -> State CreateHsmClientCertificate a
                           -> m CreateHsmClientCertificateResponse
createHsmClientCertificate p1 s =
    send $ (mkCreateHsmClientCertificate p1) &~ s

-- | Creates an HSM configuration that contains the information required by an
-- Amazon Redshift cluster to store and use database encryption keys in a
-- Hardware Security Module (HSM). After creating the HSM configuration, you
-- can specify it as a parameter when creating a cluster. The cluster will
-- then store its encryption keys in the HSM. In addition to creating an HSM
-- configuration, you must also create an HSM client certificate. For more
-- information, go to Hardware Security Modules in the Amazon Redshift
-- Management Guide.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.CreateHsmConfiguration'
createHsmConfiguration :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'chcHsmConfigurationIdentifier'
                       -> Text -- ^ 'chcDescription'
                       -> Text -- ^ 'chcHsmIpAddress'
                       -> Text -- ^ 'chcHsmPartitionName'
                       -> Text -- ^ 'chcHsmPartitionPassword'
                       -> Text -- ^ 'chcHsmServerPublicCertificate'
                       -> State CreateHsmConfiguration a
                       -> m CreateHsmConfigurationResponse
createHsmConfiguration p1 p2 p3 p4 p5 p6 s =
    send $ (mkCreateHsmConfiguration p1 p2 p3 p4 p5 p6) &~ s

-- | Deletes a previously provisioned cluster. A successful response from the
-- web service indicates that the request was received correctly. If a final
-- cluster snapshot is requested the status of the cluster will be
-- "final-snapshot" while the snapshot is being taken, then it's "deleting"
-- once Amazon Redshift begins deleting the cluster. Use DescribeClusters to
-- monitor the status of the deletion. The delete operation cannot be canceled
-- or reverted once submitted. For more information about managing clusters,
-- go to Amazon Redshift Clusters in the Amazon Redshift Management Guide .
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DeleteCluster
-- &ClusterIdentifier=examplecluster2 &SkipFinalClusterSnapshot=true
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T022400Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 1.0 5439
-- examplecluster2.cobbanlpscsn.us-east-1.redshift.amazonaws.com deleting 2 1
-- true true dev sun:10:30-sun:11:00 in-sync default.redshift-1.0
-- 2013-01-23T00:11:32.804Z active default us-east-1a dw1.xlarge
-- examplecluster2 true masteruser f2e6b87e-6503-11e2-b343-393adc3f0a21.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DeleteCluster'
deleteCluster :: ( MonadCatch m
                 , MonadResource m
                 , MonadError Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'dcClusterIdentifier'
              -> State DeleteCluster a
              -> m DeleteClusterResponse
deleteCluster p1 s =
    send $ (mkDeleteCluster p1) &~ s

-- | Deletes a specified Amazon Redshift parameter group. You cannot delete a
-- parameter group if it is associated with a cluster.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=DeleteClusterParameterGroup &ParameterGroupName=parametergroup1
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T015410Z
-- &x-amz-signedheaders=content-type;host;x-amz-date
-- 29674ca0-40da-11e2-b679-dba6cf515770.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DeleteClusterParameterGroup'
deleteClusterParameterGroup :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => Text -- ^ 'dcpgParameterGroupName'
                            -> State DeleteClusterParameterGroup a
                            -> m DeleteClusterParameterGroupResponse
deleteClusterParameterGroup p1 s =
    send $ (mkDeleteClusterParameterGroup p1) &~ s

-- | Deletes an Amazon Redshift security group. You cannot delete a security
-- group that is associated with any clusters. You cannot delete the default
-- security group. For information about managing security groups, go to
-- Amazon Redshift Cluster Security Groups in the Amazon Redshift Management
-- Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DeleteClusterSecurityGroup &ClusterSecurityGroupName=securitygroup1
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T015926Z
-- &x-amz-signedheaders=content-type;host;x-amz-date
-- e54e05dc-40da-11e2-955f-313c36e9e01d.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DeleteClusterSecurityGroup'
deleteClusterSecurityGroup :: ( MonadCatch m
                              , MonadResource m
                              , MonadError Error m
                              , MonadReader Env m
                              , AWSRequest a
                              )
                           => Text -- ^ 'dcsgClusterSecurityGroupName'
                           -> State DeleteClusterSecurityGroup a
                           -> m DeleteClusterSecurityGroupResponse
deleteClusterSecurityGroup p1 s =
    send $ (mkDeleteClusterSecurityGroup p1) &~ s

-- | Deletes the specified manual snapshot. The snapshot must be in the
-- available state, with no other users authorized to access the snapshot.
-- Unlike automated snapshots, manual snapshots are retained even after you
-- delete your cluster. Amazon Redshift does not delete your manual snapshots.
-- You must delete manual snapshot explicitly to avoid getting charged. If
-- other accounts are authorized to access the snapshot, you must revoke all
-- of the authorizations before you can delete the snapshot.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DeleteClusterSnapshot
-- &SnapshotIdentifier=snapshot-1234 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T005225Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 2012-12-07T23:31:02.372Z
-- 5439 snapshot-1234 deleted 2012-12-06T23:09:01.475Z manual 1.0 us-east-1a
-- examplecluster masteruser dw1.xlarge mydb 3
-- 88a31de4-40d1-11e2-8a25-eb010998df4e.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DeleteClusterSnapshot'
deleteClusterSnapshot :: ( MonadCatch m
                         , MonadResource m
                         , MonadError Error m
                         , MonadReader Env m
                         , AWSRequest a
                         )
                      => Text -- ^ 'dcsSnapshotIdentifier'
                      -> State DeleteClusterSnapshot a
                      -> m DeleteClusterSnapshotResponse
deleteClusterSnapshot p1 s =
    send $ (mkDeleteClusterSnapshot p1) &~ s

-- | Deletes the specified cluster subnet group.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DeleteClusterSubnetGroup
-- &ClusterSubnetGroupName=my-subnet-group-2 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130130/us-east-1/redshift/aws4_request
-- &x-amz-date=20130130T154635Z
-- &x-amz-signedheaders=content-type;host;x-amz-date
-- 3a63806b-6af4-11e2-b27b-4d850b1c672d.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DeleteClusterSubnetGroup'
deleteClusterSubnetGroup :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'dcsg1ClusterSubnetGroupName'
                         -> State DeleteClusterSubnetGroup a
                         -> m DeleteClusterSubnetGroupResponse
deleteClusterSubnetGroup p1 s =
    send $ (mkDeleteClusterSubnetGroup p1) &~ s

-- | Deletes an Amazon Redshift event notification subscription.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DeleteEventSubscription'
deleteEventSubscription :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'desSubscriptionName'
                        -> State DeleteEventSubscription a
                        -> m DeleteEventSubscriptionResponse
deleteEventSubscription p1 s =
    send $ (mkDeleteEventSubscription p1) &~ s

-- | Deletes the specified HSM client certificate.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DeleteHsmClientCertificate'
deleteHsmClientCertificate :: ( MonadCatch m
                              , MonadResource m
                              , MonadError Error m
                              , MonadReader Env m
                              , AWSRequest a
                              )
                           => Text -- ^ 'dhccHsmClientCertificateIdentifier'
                           -> State DeleteHsmClientCertificate a
                           -> m DeleteHsmClientCertificateResponse
deleteHsmClientCertificate p1 s =
    send $ (mkDeleteHsmClientCertificate p1) &~ s

-- | Deletes the specified Amazon Redshift HSM configuration.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DeleteHsmConfiguration'
deleteHsmConfiguration :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'dhcHsmConfigurationIdentifier'
                       -> State DeleteHsmConfiguration a
                       -> m DeleteHsmConfigurationResponse
deleteHsmConfiguration p1 s =
    send $ (mkDeleteHsmConfiguration p1) &~ s

-- | Returns a list of Amazon Redshift parameter groups, including parameter
-- groups you created and the default parameter group. For each parameter
-- group, the response includes the parameter group name, description, and
-- parameter group family name. You can optionally specify a name to retrieve
-- the description of a specific parameter group. For more information about
-- managing parameter groups, go to Amazon Redshift Parameter Groups in the
-- Amazon Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterParameterGroups &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T004002Z
-- &x-amz-signedheaders=content-type;host;x-amz-date redshift-1.0 Default
-- parameter group for redshift-1.0 default.redshift-1.0 redshift-1.0
-- description my parameter group parametergroup1
-- 6d28788b-64f5-11e2-b343-393adc3f0a21.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeClusterParameterGroups'
describeClusterParameterGroups :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError Error m
                                  , MonadReader Env (ResumableSource m)
                                  , AWSPager a
                                  )
                               => State DescribeClusterParameterGroups a
                               -> ResumableSource m DescribeClusterParameterGroupsResponse
describeClusterParameterGroups s =
    paginate (mkDescribeClusterParameterGroups &~ s)

-- | Returns a detailed list of parameters contained within the specified Amazon
-- Redshift parameter group. For each parameter the response includes
-- information such as parameter name, description, data type, value, whether
-- the parameter value is modifiable, and so on. You can specify source filter
-- to retrieve parameters of only specific type. For example, to retrieve
-- parameters that were modified by a user action such as from
-- ModifyClusterParameterGroup, you can specify source equal to user. For more
-- information about managing parameter groups, go to Amazon Redshift
-- Parameter Groups in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeClusterParameters
-- &ParameterGroupName=parametergroup1 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T010408Z
-- &x-amz-signedheaders=content-type;host;x-amz-date ISO, MDY string
-- engine-default true Sets the display format for date and time values.
-- datestyle 0 integer engine-default true Sets the number of digits displayed
-- for floating-point values -15-2 extra_float_digits default string
-- engine-default true This parameter applies a user-defined label to a group
-- of queries that are run during the same session.. query_group false boolean
-- engine-default true require ssl for all databaseconnections true,false
-- require_ssl $user, public string engine-default true Sets the schema search
-- order for names that are not schema-qualified. search_path 0 integer
-- engine-default true Aborts any statement that takes over the specified
-- number of milliseconds. statement_timeout
-- [{&quot;query_concurrency&quot;:5}] string engine-default true wlm json
-- configuration wlm_json_configuration 2ba35df4-40d3-11e2-82cf-0b45b05c0221.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeClusterParameters'
describeClusterParameters :: ( MonadCatch m
                             , MonadResource m
                             , MonadError Error m
                             , MonadReader Env (ResumableSource m)
                             , AWSPager a
                             )
                          => Text -- ^ 'dcpParameterGroupName'
                          -> State DescribeClusterParameters a
                          -> ResumableSource m DescribeClusterParametersResponse
describeClusterParameters p1 s =
    paginate $ (mkDescribeClusterParameters p1) &~ s

-- | Returns information about Amazon Redshift security groups. If the name of a
-- security group is specified, the response will contain only information
-- about only that security group. For information about managing security
-- groups, go to Amazon Redshift Cluster Security Groups in the Amazon
-- Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterSecurityGroups &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T010237Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 0.0.0.0/0 authorized
-- default default my security group securitygroup1
-- 947a8305-64f8-11e2-bec0-17624ad140dd.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeClusterSecurityGroups'
describeClusterSecurityGroups :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError Error m
                                 , MonadReader Env (ResumableSource m)
                                 , AWSPager a
                                 )
                              => State DescribeClusterSecurityGroups a
                              -> ResumableSource m DescribeClusterSecurityGroupsResponse
describeClusterSecurityGroups s =
    paginate (mkDescribeClusterSecurityGroups &~ s)

-- | Returns one or more snapshot objects, which contain metadata about your
-- cluster snapshots. By default, this operation returns information about all
-- snapshots of all clusters that are owned by you AWS customer account. No
-- information is returned for snapshots owned by inactive AWS customer
-- accounts. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterSnapshots &ClusterIdentifier=examplecluster
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T011512Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 5439
-- cm:examplecluster-2013-01-22-19-27-58 available automated 1.0
-- 2013-01-22T19:27:58.931Z 2 dev 2013-01-22T19:23:59.368Z us-east-1c
-- dw1.xlarge examplecluster adminuser 5439 my-snapshot-123 available manual
-- 1.0 2013-01-23T01:09:03.149Z 2 dev 2013-01-22T19:23:59.368Z us-east-1c
-- dw1.xlarge examplecluster adminuser 56a9daf4-64fa-11e2-a8da-655adc216806.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeClusterSnapshots'
describeClusterSnapshots :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env (ResumableSource m)
                            , AWSPager a
                            )
                         => State DescribeClusterSnapshots a
                         -> ResumableSource m DescribeClusterSnapshotsResponse
describeClusterSnapshots s =
    paginate (mkDescribeClusterSnapshots &~ s)

-- | Returns one or more cluster subnet group objects, which contain metadata
-- about your cluster subnet groups. By default, this operation returns
-- information about all cluster subnet groups that are defined in you AWS
-- account. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterSubnetGroups &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130130/us-east-1/redshift/aws4_request
-- &x-amz-date=20130130T153938Z
-- &x-amz-signedheaders=content-type;host;x-amz-date vpc-5d917a30 my subnet
-- group my-subnet-group Complete Active subnet-71c5091c us-east-1a Active
-- subnet-78de1215 us-east-1a 42024b68-6af3-11e2-a726-6368a468fa67.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeClusterSubnetGroups'
describeClusterSubnetGroups :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env (ResumableSource m)
                               , AWSPager a
                               )
                            => State DescribeClusterSubnetGroups a
                            -> ResumableSource m DescribeClusterSubnetGroupsResponse
describeClusterSubnetGroups s =
    paginate (mkDescribeClusterSubnetGroups &~ s)

-- | Returns descriptions of the available Amazon Redshift cluster versions. You
-- can call this operation even before creating any clusters to learn more
-- about the Amazon Redshift versions. For more information about managing
-- clusters, go to Amazon Redshift Clusters in the Amazon Redshift Management
-- Guide https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterVersions &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T230708Z
-- &x-amz-signedheaders=content-type;host;x-amz-date redshift-1.0 Initial
-- release of redshift 1.0 d39cd5e5-40c2-11e2-8a25-eb010998df4e.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeClusterVersions'
describeClusterVersions :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env (ResumableSource m)
                           , AWSPager a
                           )
                        => State DescribeClusterVersions a
                        -> ResumableSource m DescribeClusterVersionsResponse
describeClusterVersions s =
    paginate (mkDescribeClusterVersions &~ s)

-- | Returns properties of provisioned clusters including general cluster
-- properties, cluster database properties, maintenance and backup properties,
-- and security and access properties. This operation supports pagination. For
-- more information about managing clusters, go to Amazon Redshift Clusters in
-- the Amazon Redshift Management Guide . Describing All Clusters The
-- following example shows a request that describes all clusters.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeClusters
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T000452Z
-- &x-amz-signedheaders=content-type;host;x-amz-date **** 1.0 creating 2 1
-- true false dev sun:10:30-sun:11:00 in-sync default.redshift-1.0 active
-- default us-east-1a dw1.xlarge examplecluster true masteruser
-- 837d45d6-64f0-11e2-b07c-f7fbdd006c67.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeClusters'
describeClusters :: ( MonadCatch m
                    , MonadResource m
                    , MonadError Error m
                    , MonadReader Env (ResumableSource m)
                    , AWSPager a
                    )
                 => State DescribeClusters a
                 -> ResumableSource m DescribeClustersResponse
describeClusters s =
    paginate (mkDescribeClusters &~ s)

-- | Returns a list of parameter settings for the specified parameter group
-- family. For more information about managing parameter groups, go to Amazon
-- Redshift Parameter Groups in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeDefaultClusterParameters &ParameterGroupFamily=redshift-1.0
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T231708Z
-- &x-amz-signedheaders=content-type;host;x-amz-date redshift-1.0 ISO, MDY
-- string engine-default true Sets the display format for date and time
-- values. datestyle 0 integer engine-default true Sets the number of digits
-- displayed for floating-point values -15-2 extra_float_digits default string
-- engine-default true This parameter applies a user-defined label to a group
-- of queries that are run during the same session.. query_group false boolean
-- engine-default true require ssl for all databaseconnections true,false
-- require_ssl $user, public string engine-default true Sets the schema search
-- order for names that are not schema-qualified. search_path 0 integer
-- engine-default true Aborts any statement that takes over the specified
-- number of milliseconds. statement_timeout
-- [{&quot;query_concurrency&quot;:5}] string engine-default true wlm json
-- configuration wlm_json_configuration 396df00b-40c4-11e2-82cf-0b45b05c0221.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeDefaultClusterParameters'
describeDefaultClusterParameters :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadError Error m
                                    , MonadReader Env (ResumableSource m)
                                    , AWSPager a
                                    )
                                 => Text -- ^ 'ddcpParameterGroupFamily'
                                 -> State DescribeDefaultClusterParameters a
                                 -> ResumableSource m DescribeDefaultClusterParametersResponse
describeDefaultClusterParameters p1 s =
    paginate $ (mkDescribeDefaultClusterParameters p1) &~ s

-- | Displays a list of event categories for all event source types, or for a
-- specified source type. For a list of the event categories and source types,
-- go to Amazon Redshift Event Notifications.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeEventCategories'
describeEventCategories :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => State DescribeEventCategories a
                        -> m DescribeEventCategoriesResponse
describeEventCategories s =
    send (mkDescribeEventCategories &~ s)

-- | Lists descriptions of all the Amazon Redshift event notifications
-- subscription for a customer account. If you specify a subscription name,
-- lists the description for that subscription.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeEventSubscriptions'
describeEventSubscriptions :: ( MonadCatch m
                              , MonadResource m
                              , MonadError Error m
                              , MonadReader Env (ResumableSource m)
                              , AWSPager a
                              )
                           => State DescribeEventSubscriptions a
                           -> ResumableSource m DescribeEventSubscriptionsResponse
describeEventSubscriptions s =
    paginate (mkDescribeEventSubscriptions &~ s)

-- | Returns events related to clusters, security groups, snapshots, and
-- parameter groups for the past 14 days. Events specific to a particular
-- cluster, security group, snapshot or parameter group can be obtained by
-- providing the name as a parameter. By default, the past hour of events are
-- returned. https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeEvents
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T232427Z
-- &x-amz-signedheaders=content-type;host;x-amz-date Cluster security group
-- securitygroup1 has been updated. Changes need to be applied to all clusters
-- using this cluster security group. cluster-security-group
-- 2012-12-07T23:05:02.660Z securitygroup1
-- 3eeb9efe-40c5-11e2-816a-1bba29fad1f5.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeEvents'
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

-- | Returns information about the specified HSM client certificate. If no
-- certificate ID is specified, returns information about all the HSM
-- certificates owned by your AWS customer account.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeHsmClientCertificates'
describeHsmClientCertificates :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError Error m
                                 , MonadReader Env (ResumableSource m)
                                 , AWSPager a
                                 )
                              => State DescribeHsmClientCertificates a
                              -> ResumableSource m DescribeHsmClientCertificatesResponse
describeHsmClientCertificates s =
    paginate (mkDescribeHsmClientCertificates &~ s)

-- | Returns information about the specified Amazon Redshift HSM configuration.
-- If no configuration ID is specified, returns information about all the HSM
-- configurations owned by your AWS customer account.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeHsmConfigurations'
describeHsmConfigurations :: ( MonadCatch m
                             , MonadResource m
                             , MonadError Error m
                             , MonadReader Env (ResumableSource m)
                             , AWSPager a
                             )
                          => State DescribeHsmConfigurations a
                          -> ResumableSource m DescribeHsmConfigurationsResponse
describeHsmConfigurations s =
    paginate (mkDescribeHsmConfigurations &~ s)

-- | Describes whether information, such as queries and connection attempts, is
-- being logged for the specified Amazon Redshift cluster.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeLoggingStatus'
describeLoggingStatus :: ( MonadCatch m
                         , MonadResource m
                         , MonadError Error m
                         , MonadReader Env m
                         , AWSRequest a
                         )
                      => Text -- ^ 'dlsClusterIdentifier'
                      -> State DescribeLoggingStatus a
                      -> m DescribeLoggingStatusResponse
describeLoggingStatus p1 s =
    send $ (mkDescribeLoggingStatus p1) &~ s

-- | Returns a list of orderable cluster options. Before you create a new
-- cluster you can use this operation to find what options are available, such
-- as the EC2 Availability Zones (AZ) in the specific AWS region that you can
-- specify, and the node types you can request. The node types differ by
-- available storage, memory, CPU and price. With the cost involved you might
-- want to obtain a list of cluster options in the specific region and specify
-- values when creating a cluster. For more information about managing
-- clusters, go to Amazon Redshift Clusters in the Amazon Redshift Management
-- Guide https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeOrderableClusterOptions &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T225314Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 1.0 multi-node
-- dw1.8xlarge us-east-1a us-east-1c us-east-1d 1.0 multi-node dw1.xlarge
-- us-east-1a us-east-1c us-east-1d 1.0 single-node dw1.xlarge us-east-1a
-- us-east-1c us-east-1d e37414cc-40c0-11e2-b6a0-df98b1a86860.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeOrderableClusterOptions'
describeOrderableClusterOptions :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadError Error m
                                   , MonadReader Env (ResumableSource m)
                                   , AWSPager a
                                   )
                                => State DescribeOrderableClusterOptions a
                                -> ResumableSource m DescribeOrderableClusterOptionsResponse
describeOrderableClusterOptions s =
    paginate (mkDescribeOrderableClusterOptions &~ s)

-- | Returns a list of the available reserved node offerings by Amazon Redshift
-- with their descriptions including the node type, the fixed and recurring
-- costs of reserving the node and duration the node will be reserved for you.
-- These descriptions help you determine which reserve node offering you want
-- to purchase. You then use the unique offering ID in you call to
-- PurchaseReservedNodeOffering to reserve one or more nodes for your Amazon
-- Redshift cluster. For more information about managing parameter groups, go
-- to Purchasing Reserved Nodes in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeReservedNodeOfferings &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130117/us-east-1/redshift/aws4_request
-- &x-amz-date=20130117T232351Z
-- &x-amz-signedheaders=content-type;host;x-amz-date Heavy Utilization
-- 94608000 Hourly 0.21 12452.0 3a98bf7d-979a-49cc-b568-18f24315baf0 0.0
-- dw1.8xlarge Heavy Utilization 31536000 Hourly 0.09 1815.0
-- d586503b-289f-408b-955b-9c95005d6908 0.0 dw1.xlarge
-- f4a07e06-60fc-11e2-95d9-658e9466d117.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeReservedNodeOfferings'
describeReservedNodeOfferings :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError Error m
                                 , MonadReader Env (ResumableSource m)
                                 , AWSPager a
                                 )
                              => State DescribeReservedNodeOfferings a
                              -> ResumableSource m DescribeReservedNodeOfferingsResponse
describeReservedNodeOfferings s =
    paginate (mkDescribeReservedNodeOfferings &~ s)

-- | Returns the descriptions of the reserved nodes.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeReservedNodes
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130125/us-east-1/redshift/aws4_request
-- &x-amz-date=20130125T202355Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 2013-01-22T18:46:48.600Z
-- Medium Utilization 31536000 800.0 0.158 payment-pending dw1.xlarge 1
-- 4357912c-9266-469d-beb0-0f1b775e1bc9 2013-01-22T20:09:16.630Z Heavy
-- Utilization 94608000 Hourly 0.21 12452.0 0.0 payment-pending dw1.8xlarge 2
-- 93bbbca2-e88c-4b8b-a600-b64eaabf18a3 2013-01-23T21:49:32.517Z Medium
-- Utilization 31536000 800.0 0.158 payment-pending dw1.xlarge 1
-- bbcd9749-f2ea-4d01-9b1b-b576f618eb4e 24dc90c8-672d-11e2-b2e1-8f41f0379151.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeReservedNodes'
describeReservedNodes :: ( MonadCatch m
                         , MonadResource m
                         , MonadError Error m
                         , MonadReader Env (ResumableSource m)
                         , AWSPager a
                         )
                      => State DescribeReservedNodes a
                      -> ResumableSource m DescribeReservedNodesResponse
describeReservedNodes s =
    paginate (mkDescribeReservedNodes &~ s)

-- | Returns information about the last resize operation for the specified
-- cluster. If no resize operation has ever been initiated for the specified
-- cluster, a HTTP 404 error is returned. If a resize operation was initiated
-- and completed, the status of the resize remains as SUCCEEDED until the next
-- resize. A resize operation can be requested using ModifyCluster and
-- specifying a different number or type of nodes for the cluster.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeResize
-- &ClusterIdentifier=examplecluster &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T232427Z
-- &x-amz-signedheaders=content-type;host;x-amz-date multi-node SUCCEEDED
-- 6.5263 66922 0 users venue sales listing event date category 10254
-- dw1.xlarge 2 a6d59c61-a162-11e2-b2bc-fb54c9d11e09.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DescribeResize'
describeResize :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'drClusterIdentifier'
               -> State DescribeResize a
               -> m DescribeResizeResponse
describeResize p1 s =
    send $ (mkDescribeResize p1) &~ s

-- | Stops logging information, such as queries and connection attempts, for the
-- specified Amazon Redshift cluster.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DisableLogging'
disableLogging :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'dlClusterIdentifier'
               -> State DisableLogging a
               -> m DisableLoggingResponse
disableLogging p1 s =
    send $ (mkDisableLogging p1) &~ s

-- | Disables the automatic copying of snapshots from one region to another
-- region for a specified cluster.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.DisableSnapshotCopy'
disableSnapshotCopy :: ( MonadCatch m
                       , MonadResource m
                       , MonadError Error m
                       , MonadReader Env m
                       , AWSRequest a
                       )
                    => Text -- ^ 'dscClusterIdentifier'
                    -> State DisableSnapshotCopy a
                    -> m DisableSnapshotCopyResponse
disableSnapshotCopy p1 s =
    send $ (mkDisableSnapshotCopy p1) &~ s

-- | Starts logging information, such as queries and connection attempts, for
-- the specified Amazon Redshift cluster.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.EnableLogging'
enableLogging :: ( MonadCatch m
                 , MonadResource m
                 , MonadError Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'elClusterIdentifier'
              -> Text -- ^ 'elBucketName'
              -> State EnableLogging a
              -> m EnableLoggingResponse
enableLogging p1 p2 s =
    send $ (mkEnableLogging p1 p2) &~ s

-- | Enables the automatic copy of snapshots from one region to another region
-- for a specified cluster.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.EnableSnapshotCopy'
enableSnapshotCopy :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'escClusterIdentifier'
                   -> Text -- ^ 'escDestinationRegion'
                   -> State EnableSnapshotCopy a
                   -> m EnableSnapshotCopyResponse
enableSnapshotCopy p1 p2 s =
    send $ (mkEnableSnapshotCopy p1 p2) &~ s

-- | Modifies the settings for a cluster. For example, you can add another
-- security or parameter group, update the preferred maintenance window, or
-- change the master user password. Resetting a cluster password or modifying
-- the security groups associated with a cluster do not need a reboot.
-- However, modifying a parameter group requires a reboot for parameters to
-- take effect. For more information about managing clusters, go to Amazon
-- Redshift Clusters in the Amazon Redshift Management Guide You can also
-- change node type and the number of nodes to scale up or down the cluster.
-- When resizing a cluster, you must specify both the number of nodes and the
-- node type even if one of the parameters does not change. If you specify the
-- same number of nodes and node type that are already configured for the
-- cluster, an error is returned. https://redshift.us-east-1.amazonaws.com/
-- ?Action=ModifyCluster &AllowVersionUpgrade=true
-- &AutomatedSnapshotRetentionPeriod=2 &ClusterIdentifier=examplecluster
-- &ClusterParameterGroupName=parametergroup1
-- &PreferredMaintenanceWindow=wed:07:30-wed:08:00 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T022911Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 1.0 5439
-- examplecluster.coqoarplqhsn.us-east-1.redshift.amazonaws.com available 2 2
-- true false dev wed:07:30-wed:08:00 applying parametergroup1
-- 2013-01-22T19:23:59.368Z active default us-east-1c dw1.xlarge
-- examplecluster true adminuser acbc43d5-6504-11e2-bea9-49e0ce183f07.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.ModifyCluster'
modifyCluster :: ( MonadCatch m
                 , MonadResource m
                 , MonadError Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'mcClusterIdentifier'
              -> State ModifyCluster a
              -> m ModifyClusterResponse
modifyCluster p1 s =
    send $ (mkModifyCluster p1) &~ s

-- | Modifies the parameters of a parameter group. For more information about
-- managing parameter groups, go to Amazon Redshift Parameter Groups in the
-- Amazon Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=ModifyClusterParameterGroup &ParameterGroupName=parametergroup1
-- &Parameters.member.1.ParameterName=extra_float_digits
-- &Parameters.member.1.ParameterValue=2
-- &Parameters.member.2.ParameterName=wlm_json_configuration
-- &Parameters.member.2.ParameterValue=[{"user_group":["example_user_group1"],"query_group":["example_query_group1"],"query_concurrency":7},{"query_concurrency":5}]
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T022525Z
-- &x-amz-signedheaders=content-type;host;x-amz-date Your parameter group has
-- been updated but changes won't get applied until you reboot the associated
-- Clusters. parametergroup1 86e64043-40de-11e2-8a25-eb010998df4e.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.ModifyClusterParameterGroup'
modifyClusterParameterGroup :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => Text -- ^ 'mcpgParameterGroupName'
                            -> [Parameter] -- ^ 'mcpgParameters'
                            -> State ModifyClusterParameterGroup a
                            -> m ModifyClusterParameterGroupResponse
modifyClusterParameterGroup p1 p2 s =
    send $ (mkModifyClusterParameterGroup p1 p2) &~ s

-- | Modifies a cluster subnet group to include the specified list of VPC
-- subnets. The operation replaces the existing list of subnets with the new
-- list of subnets.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.ModifyClusterSubnetGroup'
modifyClusterSubnetGroup :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'mcsgClusterSubnetGroupName'
                         -> [Text] -- ^ 'mcsgSubnetIds'
                         -> State ModifyClusterSubnetGroup a
                         -> m ModifyClusterSubnetGroupResponse
modifyClusterSubnetGroup p1 p3 s =
    send $ (mkModifyClusterSubnetGroup p1 p3) &~ s

-- | Modifies an existing Amazon Redshift event notification subscription.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.ModifyEventSubscription'
modifyEventSubscription :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'mesSubscriptionName'
                        -> State ModifyEventSubscription a
                        -> m ModifyEventSubscriptionResponse
modifyEventSubscription p1 s =
    send $ (mkModifyEventSubscription p1) &~ s

-- | Modifies the number of days to retain automated snapshots in the
-- destination region after they are copied from the source region.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.ModifySnapshotCopyRetentionPeriod'
modifySnapshotCopyRetentionPeriod :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError Error m
                                     , MonadReader Env m
                                     , AWSRequest a
                                     )
                                  => Text -- ^ 'mscrpClusterIdentifier'
                                  -> Integer -- ^ 'mscrpRetentionPeriod'
                                  -> State ModifySnapshotCopyRetentionPeriod a
                                  -> m ModifySnapshotCopyRetentionPeriodResponse
modifySnapshotCopyRetentionPeriod p1 p2 s =
    send $ (mkModifySnapshotCopyRetentionPeriod p1 p2) &~ s

-- | Allows you to purchase reserved nodes. Amazon Redshift offers a predefined
-- set of reserved node offerings. You can purchase one of the offerings. You
-- can call the DescribeReservedNodeOfferings API to obtain the available
-- reserved node offerings. You can call this API by providing a specific
-- reserved node offering and the number of nodes you want to reserve. For
-- more information about managing parameter groups, go to Purchasing Reserved
-- Nodes in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=PurchaseReservedNodeOffering
-- &ReservedNodeOfferingId=3a98bf7d-979a-49cc-b568-18f24315baf0 &NodeCount=2
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130117/us-east-1/redshift/aws4_request
-- &x-amz-date=20130117T232351Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 2013-01-18T21:42:44.402Z
-- Heavy Utilization 94608000 Hourly 0.21 12452.0 0.0 payment-pending
-- dw1.8xlarge 2 1ba8e2e3-dacf-48d9-841f-cc675182a8a6
-- fcb117cc-61b7-11e2-b6e9-87e586e4ca38.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.PurchaseReservedNodeOffering'
purchaseReservedNodeOffering :: ( MonadCatch m
                                , MonadResource m
                                , MonadError Error m
                                , MonadReader Env m
                                , AWSRequest a
                                )
                             => Text -- ^ 'prnoReservedNodeOfferingId'
                             -> State PurchaseReservedNodeOffering a
                             -> m PurchaseReservedNodeOfferingResponse
purchaseReservedNodeOffering p1 s =
    send $ (mkPurchaseReservedNodeOffering p1) &~ s

-- | Reboots a cluster. This action is taken as soon as possible. It results in
-- a momentary outage to the cluster, during which the cluster status is set
-- to rebooting. A cluster event is created when the reboot is completed. Any
-- pending cluster modifications (see ModifyCluster) are applied at this
-- reboot. For more information about managing clusters, go to Amazon Redshift
-- Clusters in the Amazon Redshift Management Guide
-- https://redshift.us-east-1.amazonaws.com/ ?Action=RebootCluster
-- &ClusterIdentifier=examplecluster &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T021951Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 1.0 5439
-- examplecluster.cobaosmlqshn.us-east-1.redshift.amazonaws.com rebooting 2 1
-- true false dev sun:06:30-sun:07:00 in-sync default.redshift-1.0
-- 2013-01-22T19:23:59.368Z active default us-east-1c dw1.xlarge
-- examplecluster true adminuser 5edee79e-6503-11e2-9e70-918437dd236d.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.RebootCluster'
rebootCluster :: ( MonadCatch m
                 , MonadResource m
                 , MonadError Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'rc1ClusterIdentifier'
              -> State RebootCluster a
              -> m RebootClusterResponse
rebootCluster p1 s =
    send $ (mkRebootCluster p1) &~ s

-- | Sets one or more parameters of the specified parameter group to their
-- default values and sets the source values of the parameters to
-- "engine-default". To reset the entire parameter group specify the
-- ResetAllParameters parameter. For parameter changes to take effect you must
-- reboot any associated clusters. https://redshift.us-east-1.amazonaws.com/
-- ?Action=ResetClusterParameterGroup &ParameterGroupName=parametergroup1
-- &Parameters.member.1.ParameterName=extra_float_digits &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T020847Z
-- &x-amz-signedheaders=content-type;host;x-amz-date Your parameter group has
-- been updated but changes won't get applied until you reboot the associated
-- Clusters. parametergroup1 625d23c1-40dc-11e2-8a25-eb010998df4e.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.ResetClusterParameterGroup'
resetClusterParameterGroup :: ( MonadCatch m
                              , MonadResource m
                              , MonadError Error m
                              , MonadReader Env m
                              , AWSRequest a
                              )
                           => Text -- ^ 'rcpgParameterGroupName'
                           -> State ResetClusterParameterGroup a
                           -> m ResetClusterParameterGroupResponse
resetClusterParameterGroup p1 s =
    send $ (mkResetClusterParameterGroup p1) &~ s

-- | Creates a new cluster from a snapshot. Amazon Redshift creates the
-- resulting cluster with the same configuration as the original cluster from
-- which the snapshot was created, except that the new cluster is created with
-- the default cluster security and parameter group. After Amazon Redshift
-- creates the cluster you can use the ModifyCluster API to associate a
-- different security group and different parameter group with the restored
-- cluster. If you restore a cluster into a VPC, you must provide a cluster
-- subnet group where you want the cluster restored. For more information
-- about working with snapshots, go to Amazon Redshift Snapshots in the Amazon
-- Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=RestoreFromClusterSnapshot
-- &ClusterIdentifier=examplecluster-restored
-- &SnapshotIdentifier=cm:examplecluster-2013-01-22-19-27-58
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T023350Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 1.0 creating 2 1 true
-- false dev sun:06:30-sun:07:00 in-sync default.redshift-1.0 active default
-- dw1.xlarge examplecluster-restored true adminuser
-- 52a9aee8-6505-11e2-bec0-17624ad140dd.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.RestoreFromClusterSnapshot'
restoreFromClusterSnapshot :: ( MonadCatch m
                              , MonadResource m
                              , MonadError Error m
                              , MonadReader Env m
                              , AWSRequest a
                              )
                           => Text -- ^ 'rfcsClusterIdentifier'
                           -> Text -- ^ 'rfcsSnapshotIdentifier'
                           -> State RestoreFromClusterSnapshot a
                           -> m RestoreFromClusterSnapshotResponse
restoreFromClusterSnapshot p1 p2 s =
    send $ (mkRestoreFromClusterSnapshot p1 p2) &~ s

-- | Revokes an ingress rule in an Amazon Redshift security group for a
-- previously authorized IP range or Amazon EC2 security group. To add an
-- ingress rule, see AuthorizeClusterSecurityGroupIngress. For information
-- about managing security groups, go to Amazon Redshift Cluster Security
-- Groups in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=RevokeClusterSecurityGroupIngress
-- &ClusterSecurityGroupName=securitygroup1 &CIDRIP=192.168.40.3/32
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T021606Z
-- &x-amz-signedheaders=content-type;host;x-amz-date my security group
-- securitygroup1 d8eff363-6502-11e2-a8da-655adc216806.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.RevokeClusterSecurityGroupIngress'
revokeClusterSecurityGroupIngress :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError Error m
                                     , MonadReader Env m
                                     , AWSRequest a
                                     )
                                  => Text -- ^ 'rcsgiClusterSecurityGroupName'
                                  -> State RevokeClusterSecurityGroupIngress a
                                  -> m RevokeClusterSecurityGroupIngressResponse
revokeClusterSecurityGroupIngress p1 s =
    send $ (mkRevokeClusterSecurityGroupIngress p1) &~ s

-- | Removes the ability of the specified AWS customer account to restore the
-- specified snapshot. If the account is currently restoring the snapshot, the
-- restore will run to completion. For more information about working with
-- snapshots, go to Amazon Redshift Snapshots in the Amazon Redshift
-- Management Guide.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.RevokeSnapshotAccess'
revokeSnapshotAccess :: ( MonadCatch m
                        , MonadResource m
                        , MonadError Error m
                        , MonadReader Env m
                        , AWSRequest a
                        )
                     => Text -- ^ 'rsaSnapshotIdentifier'
                     -> Text -- ^ 'rsaAccountWithRestoreAccess'
                     -> State RevokeSnapshotAccess a
                     -> m RevokeSnapshotAccessResponse
revokeSnapshotAccess p1 p3 s =
    send $ (mkRevokeSnapshotAccess p1 p3) &~ s

-- | Rotates the encryption keys for a cluster.
--
-- See: 'Network.AWS.Redshift.V2012_12_01.RotateEncryptionKey'
rotateEncryptionKey :: ( MonadCatch m
                       , MonadResource m
                       , MonadError Error m
                       , MonadReader Env m
                       , AWSRequest a
                       )
                    => Text -- ^ 'rekClusterIdentifier'
                    -> State RotateEncryptionKey a
                    -> m RotateEncryptionKeyResponse
rotateEncryptionKey p1 s =
    send $ (mkRotateEncryptionKey p1) &~ s
