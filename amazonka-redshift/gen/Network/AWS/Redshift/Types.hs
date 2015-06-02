{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.Redshift.Types
    (
    -- * Service
      Redshift
    -- ** Error
    , RESTError
    -- ** XML
    , ns

    -- * Snapshot
    , Snapshot
    , snapshot
    , sAccountsWithRestoreAccess
    , sActualIncrementalBackupSizeInMegaBytes
    , sAvailabilityZone
    , sBackupProgressInMegaBytes
    , sClusterCreateTime
    , sClusterIdentifier
    , sClusterVersion
    , sCurrentBackupRateInMegaBytesPerSecond
    , sDBName
    , sElapsedTimeInSeconds
    , sEncrypted
    , sEncryptedWithHSM
    , sEstimatedSecondsToCompletion
    , sKmsKeyId
    , sMasterUsername
    , sNodeType
    , sNumberOfNodes
    , sOwnerAccount
    , sPort
    , sSnapshotCreateTime
    , sSnapshotIdentifier
    , sSnapshotType
    , sSourceRegion
    , sStatus
    , sTags
    , sTotalBackupSizeInMegaBytes
    , sVpcId

    -- * ClusterParameterGroup
    , ClusterParameterGroup
    , clusterParameterGroup
    , cpgDescription
    , cpgParameterGroupFamily
    , cpgParameterGroupName
    , cpgTags

    -- * RestoreStatus
    , RestoreStatus
    , restoreStatus
    , rsCurrentRestoreRateInMegaBytesPerSecond
    , rsElapsedTimeInSeconds
    , rsEstimatedTimeToCompletionInSeconds
    , rsProgressInMegaBytes
    , rsSnapshotSizeInMegaBytes
    , rsStatus

    -- * Event
    , Event
    , event
    , eDate
    , eEventCategories
    , eEventId
    , eMessage
    , eSeverity
    , eSourceIdentifier
    , eSourceType

    -- * ClusterSnapshotCopyStatus
    , ClusterSnapshotCopyStatus
    , clusterSnapshotCopyStatus
    , cscsDestinationRegion
    , cscsRetentionPeriod

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * HsmClientCertificate
    , HsmClientCertificate
    , hsmClientCertificate
    , hccHsmClientCertificateIdentifier
    , hccHsmClientCertificatePublicKey
    , hccTags

    -- * Cluster
    , Cluster
    , cluster
    , cAllowVersionUpgrade
    , cAutomatedSnapshotRetentionPeriod
    , cAvailabilityZone
    , cClusterCreateTime
    , cClusterIdentifier
    , cClusterNodes
    , cClusterParameterGroups
    , cClusterPublicKey
    , cClusterRevisionNumber
    , cClusterSecurityGroups
    , cClusterSnapshotCopyStatus
    , cClusterStatus
    , cClusterSubnetGroupName
    , cClusterVersion
    , cDBName
    , cElasticIpStatus
    , cEncrypted
    , cEndpoint
    , cHsmStatus
    , cKmsKeyId
    , cMasterUsername
    , cModifyStatus
    , cNodeType
    , cNumberOfNodes
    , cPendingModifiedValues
    , cPreferredMaintenanceWindow
    , cPubliclyAccessible
    , cRestoreStatus
    , cTags
    , cVpcId
    , cVpcSecurityGroups

    -- * ClusterNode
    , ClusterNode
    , clusterNode
    , cnNodeRole
    , cnPrivateIPAddress
    , cnPublicIPAddress

    -- * EC2SecurityGroup
    , EC2SecurityGroup
    , ec2SecurityGroup
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupOwnerId
    , ecsgStatus
    , ecsgTags

    -- * OrderableClusterOption
    , OrderableClusterOption
    , orderableClusterOption
    , ocoAvailabilityZones
    , ocoClusterType
    , ocoClusterVersion
    , ocoNodeType

    -- * SourceType
    , SourceType (..)

    -- * ClusterParameterGroupStatus
    , ClusterParameterGroupStatus
    , clusterParameterGroupStatus
    , cpgsParameterApplyStatus
    , cpgsParameterGroupName

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetAvailabilityZone
    , sSubnetIdentifier
    , sSubnetStatus

    -- * ClusterSecurityGroup
    , ClusterSecurityGroup
    , clusterSecurityGroup
    , csgClusterSecurityGroupName
    , csgDescription
    , csgEC2SecurityGroups
    , csgIPRanges
    , csgTags

    -- * DefaultClusterParameters
    , DefaultClusterParameters
    , defaultClusterParameters
    , dcpMarker
    , dcpParameterGroupFamily
    , dcpParameters

    -- * ClusterSubnetGroup
    , ClusterSubnetGroup
    , clusterSubnetGroup
    , csg1ClusterSubnetGroupName
    , csg1Description
    , csg1SubnetGroupStatus
    , csg1Subnets
    , csg1Tags
    , csg1VpcId

    -- * EventInfoMap
    , EventInfoMap
    , eventInfoMap
    , eimEventCategories
    , eimEventDescription
    , eimEventId
    , eimSeverity

    -- * ClusterSecurityGroupMembership
    , ClusterSecurityGroupMembership
    , clusterSecurityGroupMembership
    , csgmClusterSecurityGroupName
    , csgmStatus

    -- * ReservedNodeOffering
    , ReservedNodeOffering
    , reservedNodeOffering
    , rnoCurrencyCode
    , rnoDuration
    , rnoFixedPrice
    , rnoNodeType
    , rnoOfferingType
    , rnoRecurringCharges
    , rnoReservedNodeOfferingId
    , rnoUsagePrice

    -- * ReservedNode
    , ReservedNode
    , reservedNode
    , rnCurrencyCode
    , rnDuration
    , rnFixedPrice
    , rnNodeCount
    , rnNodeType
    , rnOfferingType
    , rnRecurringCharges
    , rnReservedNodeId
    , rnReservedNodeOfferingId
    , rnStartTime
    , rnState
    , rnUsagePrice

    -- * LoggingStatus
    , LoggingStatus
    , loggingStatus
    , lsBucketName
    , lsLastFailureMessage
    , lsLastFailureTime
    , lsLastSuccessfulDeliveryTime
    , lsLoggingEnabled
    , lsS3KeyPrefix

    -- * AccountWithRestoreAccess
    , AccountWithRestoreAccess
    , accountWithRestoreAccess
    , awraAccountId

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- * EventSubscription
    , EventSubscription
    , eventSubscription
    , esCustSubscriptionId
    , esCustomerAwsId
    , esEnabled
    , esEventCategoriesList
    , esSeverity
    , esSnsTopicArn
    , esSourceIdsList
    , esSourceType
    , esStatus
    , esSubscriptionCreationTime
    , esTags

    -- * HsmStatus
    , HsmStatus
    , hsmStatus
    , hsHsmClientCertificateIdentifier
    , hsHsmConfigurationIdentifier
    , hsStatus

    -- * ClusterParameterGroupNameMessage
    , ClusterParameterGroupNameMessage
    , clusterParameterGroupNameMessage
    , cpgnmParameterGroupName
    , cpgnmParameterGroupStatus

    -- * ElasticIpStatus
    , ElasticIpStatus
    , elasticIpStatus
    , eisElasticIp
    , eisStatus

    -- * ClusterVersion
    , ClusterVersion
    , clusterVersion
    , cvClusterParameterGroupFamily
    , cvClusterVersion
    , cvDescription

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- * IPRange
    , IPRange
    , iprange
    , iprCIDRIP
    , iprStatus
    , iprTags

    -- * TaggedResource
    , TaggedResource
    , taggedResource
    , trResourceName
    , trResourceType
    , trTag

    -- * EventCategoriesMap
    , EventCategoriesMap
    , eventCategoriesMap
    , ecmEvents
    , ecmSourceType

    -- * HsmConfiguration
    , HsmConfiguration
    , hsmConfiguration
    , hcDescription
    , hcHsmConfigurationIdentifier
    , hcHsmIpAddress
    , hcHsmPartitionName
    , hcTags

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvAutomatedSnapshotRetentionPeriod
    , pmvClusterIdentifier
    , pmvClusterType
    , pmvClusterVersion
    , pmvMasterUserPassword
    , pmvNodeType
    , pmvNumberOfNodes

    -- * VpcSecurityGroupMembership
    , VpcSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVpcSecurityGroupId

    -- * Parameter
    , Parameter
    , parameter
    , pAllowedValues
    , pDataType
    , pDescription
    , pIsModifiable
    , pMinimumEngineVersion
    , pParameterName
    , pParameterValue
    , pSource
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2012-12-01@ of the Amazon Redshift service.
data Redshift

instance AWSService Redshift where
    type Sg Redshift = V4
    type Er Redshift = RESTError

    service = service'
      where
        service' :: Service Redshift
        service' = Service
            { _svcAbbrev       = "Redshift"
            , _svcPrefix       = "redshift"
            , _svcVersion      = "2012-12-01"
            , _svcTargetPrefix = Nothing
            , _svcJSONVersion  = Nothing
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry Redshift
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 400 && (Just "Throttling") == e = True -- Throttling
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

ns :: Text
ns = "http://redshift.amazonaws.com/doc/2012-12-01/"
{-# INLINE ns #-}

data Snapshot = Snapshot
    { _sAccountsWithRestoreAccess              :: List "member" AccountWithRestoreAccess
    , _sActualIncrementalBackupSizeInMegaBytes :: Maybe Double
    , _sAvailabilityZone                       :: Maybe Text
    , _sBackupProgressInMegaBytes              :: Maybe Double
    , _sClusterCreateTime                      :: Maybe ISO8601
    , _sClusterIdentifier                      :: Maybe Text
    , _sClusterVersion                         :: Maybe Text
    , _sCurrentBackupRateInMegaBytesPerSecond  :: Maybe Double
    , _sDBName                                 :: Maybe Text
    , _sElapsedTimeInSeconds                   :: Maybe Integer
    , _sEncrypted                              :: Maybe Bool
    , _sEncryptedWithHSM                       :: Maybe Bool
    , _sEstimatedSecondsToCompletion           :: Maybe Integer
    , _sKmsKeyId                               :: Maybe Text
    , _sMasterUsername                         :: Maybe Text
    , _sNodeType                               :: Maybe Text
    , _sNumberOfNodes                          :: Maybe Int
    , _sOwnerAccount                           :: Maybe Text
    , _sPort                                   :: Maybe Int
    , _sSnapshotCreateTime                     :: Maybe ISO8601
    , _sSnapshotIdentifier                     :: Maybe Text
    , _sSnapshotType                           :: Maybe Text
    , _sSourceRegion                           :: Maybe Text
    , _sStatus                                 :: Maybe Text
    , _sTags                                   :: List "member" Tag
    , _sTotalBackupSizeInMegaBytes             :: Maybe Double
    , _sVpcId                                  :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'Snapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sAccountsWithRestoreAccess' @::@ ['AccountWithRestoreAccess']
--
-- * 'sActualIncrementalBackupSizeInMegaBytes' @::@ 'Maybe' 'Double'
--
-- * 'sAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'sBackupProgressInMegaBytes' @::@ 'Maybe' 'Double'
--
-- * 'sClusterCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'sClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'sClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'sCurrentBackupRateInMegaBytesPerSecond' @::@ 'Maybe' 'Double'
--
-- * 'sDBName' @::@ 'Maybe' 'Text'
--
-- * 'sElapsedTimeInSeconds' @::@ 'Maybe' 'Integer'
--
-- * 'sEncrypted' @::@ 'Maybe' 'Bool'
--
-- * 'sEncryptedWithHSM' @::@ 'Maybe' 'Bool'
--
-- * 'sEstimatedSecondsToCompletion' @::@ 'Maybe' 'Integer'
--
-- * 'sKmsKeyId' @::@ 'Maybe' 'Text'
--
-- * 'sMasterUsername' @::@ 'Maybe' 'Text'
--
-- * 'sNodeType' @::@ 'Maybe' 'Text'
--
-- * 'sNumberOfNodes' @::@ 'Maybe' 'Int'
--
-- * 'sOwnerAccount' @::@ 'Maybe' 'Text'
--
-- * 'sPort' @::@ 'Maybe' 'Int'
--
-- * 'sSnapshotCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'sSnapshotIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'sSnapshotType' @::@ 'Maybe' 'Text'
--
-- * 'sSourceRegion' @::@ 'Maybe' 'Text'
--
-- * 'sStatus' @::@ 'Maybe' 'Text'
--
-- * 'sTags' @::@ ['Tag']
--
-- * 'sTotalBackupSizeInMegaBytes' @::@ 'Maybe' 'Double'
--
-- * 'sVpcId' @::@ 'Maybe' 'Text'
--
snapshot :: Snapshot
snapshot = Snapshot
    { _sSnapshotIdentifier                     = Nothing
    , _sClusterIdentifier                      = Nothing
    , _sSnapshotCreateTime                     = Nothing
    , _sStatus                                 = Nothing
    , _sPort                                   = Nothing
    , _sAvailabilityZone                       = Nothing
    , _sClusterCreateTime                      = Nothing
    , _sMasterUsername                         = Nothing
    , _sClusterVersion                         = Nothing
    , _sSnapshotType                           = Nothing
    , _sNodeType                               = Nothing
    , _sNumberOfNodes                          = Nothing
    , _sDBName                                 = Nothing
    , _sVpcId                                  = Nothing
    , _sEncrypted                              = Nothing
    , _sKmsKeyId                               = Nothing
    , _sEncryptedWithHSM                       = Nothing
    , _sAccountsWithRestoreAccess              = mempty
    , _sOwnerAccount                           = Nothing
    , _sTotalBackupSizeInMegaBytes             = Nothing
    , _sActualIncrementalBackupSizeInMegaBytes = Nothing
    , _sBackupProgressInMegaBytes              = Nothing
    , _sCurrentBackupRateInMegaBytesPerSecond  = Nothing
    , _sEstimatedSecondsToCompletion           = Nothing
    , _sElapsedTimeInSeconds                   = Nothing
    , _sSourceRegion                           = Nothing
    , _sTags                                   = mempty
    }

-- | A list of the AWS customer accounts authorized to restore the snapshot.
-- Returns 'null' if no accounts are authorized. Visible only to the snapshot
-- owner.
sAccountsWithRestoreAccess :: Lens' Snapshot [AccountWithRestoreAccess]
sAccountsWithRestoreAccess =
    lens _sAccountsWithRestoreAccess
        (\s a -> s { _sAccountsWithRestoreAccess = a })
            . _List

-- | The size of the incremental backup.
sActualIncrementalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
sActualIncrementalBackupSizeInMegaBytes =
    lens _sActualIncrementalBackupSizeInMegaBytes
        (\s a -> s { _sActualIncrementalBackupSizeInMegaBytes = a })

-- | The Availability Zone in which the cluster was created.
sAvailabilityZone :: Lens' Snapshot (Maybe Text)
sAvailabilityZone =
    lens _sAvailabilityZone (\s a -> s { _sAvailabilityZone = a })

-- | The number of megabytes that have been transferred to the snapshot backup.
sBackupProgressInMegaBytes :: Lens' Snapshot (Maybe Double)
sBackupProgressInMegaBytes =
    lens _sBackupProgressInMegaBytes
        (\s a -> s { _sBackupProgressInMegaBytes = a })

-- | The time (UTC) when the cluster was originally created.
sClusterCreateTime :: Lens' Snapshot (Maybe UTCTime)
sClusterCreateTime =
    lens _sClusterCreateTime (\s a -> s { _sClusterCreateTime = a })
        . mapping _Time

-- | The identifier of the cluster for which the snapshot was taken.
sClusterIdentifier :: Lens' Snapshot (Maybe Text)
sClusterIdentifier =
    lens _sClusterIdentifier (\s a -> s { _sClusterIdentifier = a })

-- | The version ID of the Amazon Redshift engine that is running on the cluster.
sClusterVersion :: Lens' Snapshot (Maybe Text)
sClusterVersion = lens _sClusterVersion (\s a -> s { _sClusterVersion = a })

-- | The number of megabytes per second being transferred to the snapshot backup.
-- Returns '0' for a completed backup.
sCurrentBackupRateInMegaBytesPerSecond :: Lens' Snapshot (Maybe Double)
sCurrentBackupRateInMegaBytesPerSecond =
    lens _sCurrentBackupRateInMegaBytesPerSecond
        (\s a -> s { _sCurrentBackupRateInMegaBytesPerSecond = a })

-- | The name of the database that was created when the cluster was created.
sDBName :: Lens' Snapshot (Maybe Text)
sDBName = lens _sDBName (\s a -> s { _sDBName = a })

-- | The amount of time an in-progress snapshot backup has been running, or the
-- amount of time it took a completed backup to finish.
sElapsedTimeInSeconds :: Lens' Snapshot (Maybe Integer)
sElapsedTimeInSeconds =
    lens _sElapsedTimeInSeconds (\s a -> s { _sElapsedTimeInSeconds = a })

-- | If 'true', the data in the snapshot is encrypted at rest.
sEncrypted :: Lens' Snapshot (Maybe Bool)
sEncrypted = lens _sEncrypted (\s a -> s { _sEncrypted = a })

-- | A boolean that indicates whether the snapshot data is encrypted using the HSM
-- keys of the source cluster. 'true' indicates that the data is encrypted using
-- HSM keys.
sEncryptedWithHSM :: Lens' Snapshot (Maybe Bool)
sEncryptedWithHSM =
    lens _sEncryptedWithHSM (\s a -> s { _sEncryptedWithHSM = a })

-- | The estimate of the time remaining before the snapshot backup will complete.
-- Returns '0' for a completed backup.
sEstimatedSecondsToCompletion :: Lens' Snapshot (Maybe Integer)
sEstimatedSecondsToCompletion =
    lens _sEstimatedSecondsToCompletion
        (\s a -> s { _sEstimatedSecondsToCompletion = a })

-- | The AWS Key Management Service (KMS) key ID of the encryption key that was
-- used to encrypt data in the cluster from which the snapshot was taken.
sKmsKeyId :: Lens' Snapshot (Maybe Text)
sKmsKeyId = lens _sKmsKeyId (\s a -> s { _sKmsKeyId = a })

-- | The master user name for the cluster.
sMasterUsername :: Lens' Snapshot (Maybe Text)
sMasterUsername = lens _sMasterUsername (\s a -> s { _sMasterUsername = a })

-- | The node type of the nodes in the cluster.
sNodeType :: Lens' Snapshot (Maybe Text)
sNodeType = lens _sNodeType (\s a -> s { _sNodeType = a })

-- | The number of nodes in the cluster.
sNumberOfNodes :: Lens' Snapshot (Maybe Int)
sNumberOfNodes = lens _sNumberOfNodes (\s a -> s { _sNumberOfNodes = a })

-- | For manual snapshots, the AWS customer account used to create or copy the
-- snapshot. For automatic snapshots, the owner of the cluster. The owner can
-- perform all snapshot actions, such as sharing a manual snapshot.
sOwnerAccount :: Lens' Snapshot (Maybe Text)
sOwnerAccount = lens _sOwnerAccount (\s a -> s { _sOwnerAccount = a })

-- | The port that the cluster is listening on.
sPort :: Lens' Snapshot (Maybe Int)
sPort = lens _sPort (\s a -> s { _sPort = a })

-- | The time (UTC) when Amazon Redshift began the snapshot. A snapshot contains
-- a copy of the cluster data as of this exact time.
sSnapshotCreateTime :: Lens' Snapshot (Maybe UTCTime)
sSnapshotCreateTime =
    lens _sSnapshotCreateTime (\s a -> s { _sSnapshotCreateTime = a })
        . mapping _Time

-- | The snapshot identifier that is provided in the request.
sSnapshotIdentifier :: Lens' Snapshot (Maybe Text)
sSnapshotIdentifier =
    lens _sSnapshotIdentifier (\s a -> s { _sSnapshotIdentifier = a })

-- | The snapshot type. Snapshots created using 'CreateClusterSnapshot' and 'CopyClusterSnapshot' will be of type "manual".
sSnapshotType :: Lens' Snapshot (Maybe Text)
sSnapshotType = lens _sSnapshotType (\s a -> s { _sSnapshotType = a })

-- | The source region from which the snapshot was copied.
sSourceRegion :: Lens' Snapshot (Maybe Text)
sSourceRegion = lens _sSourceRegion (\s a -> s { _sSourceRegion = a })

-- | The snapshot status. The value of the status depends on the API operation
-- used.   'CreateClusterSnapshot' and 'CopyClusterSnapshot' returns status as
-- "creating".   'DescribeClusterSnapshots' returns status as "creating",
-- "available", "final snapshot", or "failed".  'DeleteClusterSnapshot' returns
-- status as "deleted".
sStatus :: Lens' Snapshot (Maybe Text)
sStatus = lens _sStatus (\s a -> s { _sStatus = a })

-- | The list of tags for the cluster snapshot.
sTags :: Lens' Snapshot [Tag]
sTags = lens _sTags (\s a -> s { _sTags = a }) . _List

-- | The size of the complete set of backup data that would be used to restore
-- the cluster.
sTotalBackupSizeInMegaBytes :: Lens' Snapshot (Maybe Double)
sTotalBackupSizeInMegaBytes =
    lens _sTotalBackupSizeInMegaBytes
        (\s a -> s { _sTotalBackupSizeInMegaBytes = a })

-- | The VPC identifier of the cluster if the snapshot is from a cluster in a VPC.
-- Otherwise, this field is not in the output.
sVpcId :: Lens' Snapshot (Maybe Text)
sVpcId = lens _sVpcId (\s a -> s { _sVpcId = a })

instance FromXML Snapshot where
    parseXML x = Snapshot
        <$> x .@? "AccountsWithRestoreAccess" .!@ mempty
        <*> x .@? "ActualIncrementalBackupSizeInMegaBytes"
        <*> x .@? "AvailabilityZone"
        <*> x .@? "BackupProgressInMegaBytes"
        <*> x .@? "ClusterCreateTime"
        <*> x .@? "ClusterIdentifier"
        <*> x .@? "ClusterVersion"
        <*> x .@? "CurrentBackupRateInMegaBytesPerSecond"
        <*> x .@? "DBName"
        <*> x .@? "ElapsedTimeInSeconds"
        <*> x .@? "Encrypted"
        <*> x .@? "EncryptedWithHSM"
        <*> x .@? "EstimatedSecondsToCompletion"
        <*> x .@? "KmsKeyId"
        <*> x .@? "MasterUsername"
        <*> x .@? "NodeType"
        <*> x .@? "NumberOfNodes"
        <*> x .@? "OwnerAccount"
        <*> x .@? "Port"
        <*> x .@? "SnapshotCreateTime"
        <*> x .@? "SnapshotIdentifier"
        <*> x .@? "SnapshotType"
        <*> x .@? "SourceRegion"
        <*> x .@? "Status"
        <*> x .@? "Tags" .!@ mempty
        <*> x .@? "TotalBackupSizeInMegaBytes"
        <*> x .@? "VpcId"

instance ToQuery Snapshot where
    toQuery Snapshot{..} = mconcat
        [ "AccountsWithRestoreAccess"              =? _sAccountsWithRestoreAccess
        , "ActualIncrementalBackupSizeInMegaBytes" =? _sActualIncrementalBackupSizeInMegaBytes
        , "AvailabilityZone"                       =? _sAvailabilityZone
        , "BackupProgressInMegaBytes"              =? _sBackupProgressInMegaBytes
        , "ClusterCreateTime"                      =? _sClusterCreateTime
        , "ClusterIdentifier"                      =? _sClusterIdentifier
        , "ClusterVersion"                         =? _sClusterVersion
        , "CurrentBackupRateInMegaBytesPerSecond"  =? _sCurrentBackupRateInMegaBytesPerSecond
        , "DBName"                                 =? _sDBName
        , "ElapsedTimeInSeconds"                   =? _sElapsedTimeInSeconds
        , "Encrypted"                              =? _sEncrypted
        , "EncryptedWithHSM"                       =? _sEncryptedWithHSM
        , "EstimatedSecondsToCompletion"           =? _sEstimatedSecondsToCompletion
        , "KmsKeyId"                               =? _sKmsKeyId
        , "MasterUsername"                         =? _sMasterUsername
        , "NodeType"                               =? _sNodeType
        , "NumberOfNodes"                          =? _sNumberOfNodes
        , "OwnerAccount"                           =? _sOwnerAccount
        , "Port"                                   =? _sPort
        , "SnapshotCreateTime"                     =? _sSnapshotCreateTime
        , "SnapshotIdentifier"                     =? _sSnapshotIdentifier
        , "SnapshotType"                           =? _sSnapshotType
        , "SourceRegion"                           =? _sSourceRegion
        , "Status"                                 =? _sStatus
        , "Tags"                                   =? _sTags
        , "TotalBackupSizeInMegaBytes"             =? _sTotalBackupSizeInMegaBytes
        , "VpcId"                                  =? _sVpcId
        ]

data ClusterParameterGroup = ClusterParameterGroup
    { _cpgDescription          :: Maybe Text
    , _cpgParameterGroupFamily :: Maybe Text
    , _cpgParameterGroupName   :: Maybe Text
    , _cpgTags                 :: List "member" Tag
    } deriving (Eq, Read, Show)

-- | 'ClusterParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgDescription' @::@ 'Maybe' 'Text'
--
-- * 'cpgParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'cpgParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cpgTags' @::@ ['Tag']
--
clusterParameterGroup :: ClusterParameterGroup
clusterParameterGroup = ClusterParameterGroup
    { _cpgParameterGroupName   = Nothing
    , _cpgParameterGroupFamily = Nothing
    , _cpgDescription          = Nothing
    , _cpgTags                 = mempty
    }

-- | The description of the parameter group.
cpgDescription :: Lens' ClusterParameterGroup (Maybe Text)
cpgDescription = lens _cpgDescription (\s a -> s { _cpgDescription = a })

-- | The name of the cluster parameter group family that this cluster parameter
-- group is compatible with.
cpgParameterGroupFamily :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupFamily =
    lens _cpgParameterGroupFamily (\s a -> s { _cpgParameterGroupFamily = a })

-- | The name of the cluster parameter group.
cpgParameterGroupName :: Lens' ClusterParameterGroup (Maybe Text)
cpgParameterGroupName =
    lens _cpgParameterGroupName (\s a -> s { _cpgParameterGroupName = a })

-- | The list of tags for the cluster parameter group.
cpgTags :: Lens' ClusterParameterGroup [Tag]
cpgTags = lens _cpgTags (\s a -> s { _cpgTags = a }) . _List

instance FromXML ClusterParameterGroup where
    parseXML x = ClusterParameterGroup
        <$> x .@? "Description"
        <*> x .@? "ParameterGroupFamily"
        <*> x .@? "ParameterGroupName"
        <*> x .@? "Tags" .!@ mempty

instance ToQuery ClusterParameterGroup where
    toQuery ClusterParameterGroup{..} = mconcat
        [ "Description"          =? _cpgDescription
        , "ParameterGroupFamily" =? _cpgParameterGroupFamily
        , "ParameterGroupName"   =? _cpgParameterGroupName
        , "Tags"                 =? _cpgTags
        ]

data RestoreStatus = RestoreStatus
    { _rsCurrentRestoreRateInMegaBytesPerSecond :: Maybe Double
    , _rsElapsedTimeInSeconds                   :: Maybe Integer
    , _rsEstimatedTimeToCompletionInSeconds     :: Maybe Integer
    , _rsProgressInMegaBytes                    :: Maybe Integer
    , _rsSnapshotSizeInMegaBytes                :: Maybe Integer
    , _rsStatus                                 :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RestoreStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsCurrentRestoreRateInMegaBytesPerSecond' @::@ 'Maybe' 'Double'
--
-- * 'rsElapsedTimeInSeconds' @::@ 'Maybe' 'Integer'
--
-- * 'rsEstimatedTimeToCompletionInSeconds' @::@ 'Maybe' 'Integer'
--
-- * 'rsProgressInMegaBytes' @::@ 'Maybe' 'Integer'
--
-- * 'rsSnapshotSizeInMegaBytes' @::@ 'Maybe' 'Integer'
--
-- * 'rsStatus' @::@ 'Maybe' 'Text'
--
restoreStatus :: RestoreStatus
restoreStatus = RestoreStatus
    { _rsStatus                                 = Nothing
    , _rsCurrentRestoreRateInMegaBytesPerSecond = Nothing
    , _rsSnapshotSizeInMegaBytes                = Nothing
    , _rsProgressInMegaBytes                    = Nothing
    , _rsElapsedTimeInSeconds                   = Nothing
    , _rsEstimatedTimeToCompletionInSeconds     = Nothing
    }

-- | The number of megabytes per second being transferred from the backup
-- storage. Returns the average rate for a completed backup.
rsCurrentRestoreRateInMegaBytesPerSecond :: Lens' RestoreStatus (Maybe Double)
rsCurrentRestoreRateInMegaBytesPerSecond =
    lens _rsCurrentRestoreRateInMegaBytesPerSecond
        (\s a -> s { _rsCurrentRestoreRateInMegaBytesPerSecond = a })

-- | The amount of time an in-progress restore has been running, or the amount of
-- time it took a completed restore to finish.
rsElapsedTimeInSeconds :: Lens' RestoreStatus (Maybe Integer)
rsElapsedTimeInSeconds =
    lens _rsElapsedTimeInSeconds (\s a -> s { _rsElapsedTimeInSeconds = a })

-- | The estimate of the time remaining before the restore will complete. Returns
-- 0 for a completed restore.
rsEstimatedTimeToCompletionInSeconds :: Lens' RestoreStatus (Maybe Integer)
rsEstimatedTimeToCompletionInSeconds =
    lens _rsEstimatedTimeToCompletionInSeconds
        (\s a -> s { _rsEstimatedTimeToCompletionInSeconds = a })

-- | The number of megabytes that have been transferred from snapshot storage.
rsProgressInMegaBytes :: Lens' RestoreStatus (Maybe Integer)
rsProgressInMegaBytes =
    lens _rsProgressInMegaBytes (\s a -> s { _rsProgressInMegaBytes = a })

-- | The size of the set of snapshot data used to restore the cluster.
rsSnapshotSizeInMegaBytes :: Lens' RestoreStatus (Maybe Integer)
rsSnapshotSizeInMegaBytes =
    lens _rsSnapshotSizeInMegaBytes
        (\s a -> s { _rsSnapshotSizeInMegaBytes = a })

-- | The status of the restore action. Returns starting, restoring, completed, or
-- failed.
rsStatus :: Lens' RestoreStatus (Maybe Text)
rsStatus = lens _rsStatus (\s a -> s { _rsStatus = a })

instance FromXML RestoreStatus where
    parseXML x = RestoreStatus
        <$> x .@? "CurrentRestoreRateInMegaBytesPerSecond"
        <*> x .@? "ElapsedTimeInSeconds"
        <*> x .@? "EstimatedTimeToCompletionInSeconds"
        <*> x .@? "ProgressInMegaBytes"
        <*> x .@? "SnapshotSizeInMegaBytes"
        <*> x .@? "Status"

instance ToQuery RestoreStatus where
    toQuery RestoreStatus{..} = mconcat
        [ "CurrentRestoreRateInMegaBytesPerSecond" =? _rsCurrentRestoreRateInMegaBytesPerSecond
        , "ElapsedTimeInSeconds"                   =? _rsElapsedTimeInSeconds
        , "EstimatedTimeToCompletionInSeconds"     =? _rsEstimatedTimeToCompletionInSeconds
        , "ProgressInMegaBytes"                    =? _rsProgressInMegaBytes
        , "SnapshotSizeInMegaBytes"                =? _rsSnapshotSizeInMegaBytes
        , "Status"                                 =? _rsStatus
        ]

data Event = Event
    { _eDate             :: Maybe ISO8601
    , _eEventCategories  :: List "member" Text
    , _eEventId          :: Maybe Text
    , _eMessage          :: Maybe Text
    , _eSeverity         :: Maybe Text
    , _eSourceIdentifier :: Maybe Text
    , _eSourceType       :: Maybe SourceType
    } deriving (Eq, Read, Show)

-- | 'Event' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'eEventCategories' @::@ ['Text']
--
-- * 'eEventId' @::@ 'Maybe' 'Text'
--
-- * 'eMessage' @::@ 'Maybe' 'Text'
--
-- * 'eSeverity' @::@ 'Maybe' 'Text'
--
-- * 'eSourceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'eSourceType' @::@ 'Maybe' 'SourceType'
--
event :: Event
event = Event
    { _eSourceIdentifier = Nothing
    , _eSourceType       = Nothing
    , _eMessage          = Nothing
    , _eEventCategories  = mempty
    , _eSeverity         = Nothing
    , _eDate             = Nothing
    , _eEventId          = Nothing
    }

-- | The date and time of the event.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\s a -> s { _eDate = a }) . mapping _Time

-- | A list of the event categories.
eEventCategories :: Lens' Event [Text]
eEventCategories = lens _eEventCategories (\s a -> s { _eEventCategories = a }) . _List

-- | The identifier of the event.
eEventId :: Lens' Event (Maybe Text)
eEventId = lens _eEventId (\s a -> s { _eEventId = a })

-- | The text of this event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\s a -> s { _eMessage = a })

-- | The severity of the event.
--
-- Values: ERROR, INFO
eSeverity :: Lens' Event (Maybe Text)
eSeverity = lens _eSeverity (\s a -> s { _eSeverity = a })

-- | The identifier for the source of the event.
eSourceIdentifier :: Lens' Event (Maybe Text)
eSourceIdentifier =
    lens _eSourceIdentifier (\s a -> s { _eSourceIdentifier = a })

-- | The source type for this event.
eSourceType :: Lens' Event (Maybe SourceType)
eSourceType = lens _eSourceType (\s a -> s { _eSourceType = a })

instance FromXML Event where
    parseXML x = Event
        <$> x .@? "Date"
        <*> x .@? "EventCategories" .!@ mempty
        <*> x .@? "EventId"
        <*> x .@? "Message"
        <*> x .@? "Severity"
        <*> x .@? "SourceIdentifier"
        <*> x .@? "SourceType"

instance ToQuery Event where
    toQuery Event{..} = mconcat
        [ "Date"             =? _eDate
        , "EventCategories"  =? _eEventCategories
        , "EventId"          =? _eEventId
        , "Message"          =? _eMessage
        , "Severity"         =? _eSeverity
        , "SourceIdentifier" =? _eSourceIdentifier
        , "SourceType"       =? _eSourceType
        ]

data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus
    { _cscsDestinationRegion :: Maybe Text
    , _cscsRetentionPeriod   :: Maybe Integer
    } deriving (Eq, Ord, Read, Show)

-- | 'ClusterSnapshotCopyStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cscsDestinationRegion' @::@ 'Maybe' 'Text'
--
-- * 'cscsRetentionPeriod' @::@ 'Maybe' 'Integer'
--
clusterSnapshotCopyStatus :: ClusterSnapshotCopyStatus
clusterSnapshotCopyStatus = ClusterSnapshotCopyStatus
    { _cscsDestinationRegion = Nothing
    , _cscsRetentionPeriod   = Nothing
    }

-- | The destination region that snapshots are automatically copied to when
-- cross-region snapshot copy is enabled.
cscsDestinationRegion :: Lens' ClusterSnapshotCopyStatus (Maybe Text)
cscsDestinationRegion =
    lens _cscsDestinationRegion (\s a -> s { _cscsDestinationRegion = a })

-- | The number of days that automated snapshots are retained in the destination
-- region after they are copied from a source region.
cscsRetentionPeriod :: Lens' ClusterSnapshotCopyStatus (Maybe Integer)
cscsRetentionPeriod =
    lens _cscsRetentionPeriod (\s a -> s { _cscsRetentionPeriod = a })

instance FromXML ClusterSnapshotCopyStatus where
    parseXML x = ClusterSnapshotCopyStatus
        <$> x .@? "DestinationRegion"
        <*> x .@? "RetentionPeriod"

instance ToQuery ClusterSnapshotCopyStatus where
    toQuery ClusterSnapshotCopyStatus{..} = mconcat
        [ "DestinationRegion" =? _cscsDestinationRegion
        , "RetentionPeriod"   =? _cscsRetentionPeriod
        ]

data Tag = Tag
    { _tagKey   :: Maybe Text
    , _tagValue :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Tag' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey' @::@ 'Maybe' 'Text'
--
-- * 'tagValue' @::@ 'Maybe' 'Text'
--
tag :: Tag
tag = Tag
    { _tagKey   = Nothing
    , _tagValue = Nothing
    }

-- | The key, or name, for the resource tag.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\s a -> s { _tagKey = a })

-- | The value for the resource tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\s a -> s { _tagValue = a })

instance FromXML Tag where
    parseXML x = Tag
        <$> x .@? "Key"
        <*> x .@? "Value"

instance ToQuery Tag where
    toQuery Tag{..} = mconcat
        [ "Key"   =? _tagKey
        , "Value" =? _tagValue
        ]

data HsmClientCertificate = HsmClientCertificate
    { _hccHsmClientCertificateIdentifier :: Maybe Text
    , _hccHsmClientCertificatePublicKey  :: Maybe Text
    , _hccTags                           :: List "member" Tag
    } deriving (Eq, Read, Show)

-- | 'HsmClientCertificate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hccHsmClientCertificateIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'hccHsmClientCertificatePublicKey' @::@ 'Maybe' 'Text'
--
-- * 'hccTags' @::@ ['Tag']
--
hsmClientCertificate :: HsmClientCertificate
hsmClientCertificate = HsmClientCertificate
    { _hccHsmClientCertificateIdentifier = Nothing
    , _hccHsmClientCertificatePublicKey  = Nothing
    , _hccTags                           = mempty
    }

-- | The identifier of the HSM client certificate.
hccHsmClientCertificateIdentifier :: Lens' HsmClientCertificate (Maybe Text)
hccHsmClientCertificateIdentifier =
    lens _hccHsmClientCertificateIdentifier
        (\s a -> s { _hccHsmClientCertificateIdentifier = a })

-- | The public key that the Amazon Redshift cluster will use to connect to the
-- HSM. You must register the public key in the HSM.
hccHsmClientCertificatePublicKey :: Lens' HsmClientCertificate (Maybe Text)
hccHsmClientCertificatePublicKey =
    lens _hccHsmClientCertificatePublicKey
        (\s a -> s { _hccHsmClientCertificatePublicKey = a })

-- | The list of tags for the HSM client certificate.
hccTags :: Lens' HsmClientCertificate [Tag]
hccTags = lens _hccTags (\s a -> s { _hccTags = a }) . _List

instance FromXML HsmClientCertificate where
    parseXML x = HsmClientCertificate
        <$> x .@? "HsmClientCertificateIdentifier"
        <*> x .@? "HsmClientCertificatePublicKey"
        <*> x .@? "Tags" .!@ mempty

instance ToQuery HsmClientCertificate where
    toQuery HsmClientCertificate{..} = mconcat
        [ "HsmClientCertificateIdentifier" =? _hccHsmClientCertificateIdentifier
        , "HsmClientCertificatePublicKey"  =? _hccHsmClientCertificatePublicKey
        , "Tags"                           =? _hccTags
        ]

data Cluster = Cluster
    { _cAllowVersionUpgrade              :: Maybe Bool
    , _cAutomatedSnapshotRetentionPeriod :: Maybe Int
    , _cAvailabilityZone                 :: Maybe Text
    , _cClusterCreateTime                :: Maybe ISO8601
    , _cClusterIdentifier                :: Maybe Text
    , _cClusterNodes                     :: List "member" ClusterNode
    , _cClusterParameterGroups           :: List "member" ClusterParameterGroupStatus
    , _cClusterPublicKey                 :: Maybe Text
    , _cClusterRevisionNumber            :: Maybe Text
    , _cClusterSecurityGroups            :: List "member" ClusterSecurityGroupMembership
    , _cClusterSnapshotCopyStatus        :: Maybe ClusterSnapshotCopyStatus
    , _cClusterStatus                    :: Maybe Text
    , _cClusterSubnetGroupName           :: Maybe Text
    , _cClusterVersion                   :: Maybe Text
    , _cDBName                           :: Maybe Text
    , _cElasticIpStatus                  :: Maybe ElasticIpStatus
    , _cEncrypted                        :: Maybe Bool
    , _cEndpoint                         :: Maybe Endpoint
    , _cHsmStatus                        :: Maybe HsmStatus
    , _cKmsKeyId                         :: Maybe Text
    , _cMasterUsername                   :: Maybe Text
    , _cModifyStatus                     :: Maybe Text
    , _cNodeType                         :: Maybe Text
    , _cNumberOfNodes                    :: Maybe Int
    , _cPendingModifiedValues            :: Maybe PendingModifiedValues
    , _cPreferredMaintenanceWindow       :: Maybe Text
    , _cPubliclyAccessible               :: Maybe Bool
    , _cRestoreStatus                    :: Maybe RestoreStatus
    , _cTags                             :: List "member" Tag
    , _cVpcId                            :: Maybe Text
    , _cVpcSecurityGroups                :: List "member" VpcSecurityGroupMembership
    } deriving (Eq, Read, Show)

-- | 'Cluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cAllowVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'cAutomatedSnapshotRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'cAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'cClusterCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'cClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'cClusterNodes' @::@ ['ClusterNode']
--
-- * 'cClusterParameterGroups' @::@ ['ClusterParameterGroupStatus']
--
-- * 'cClusterPublicKey' @::@ 'Maybe' 'Text'
--
-- * 'cClusterRevisionNumber' @::@ 'Maybe' 'Text'
--
-- * 'cClusterSecurityGroups' @::@ ['ClusterSecurityGroupMembership']
--
-- * 'cClusterSnapshotCopyStatus' @::@ 'Maybe' 'ClusterSnapshotCopyStatus'
--
-- * 'cClusterStatus' @::@ 'Maybe' 'Text'
--
-- * 'cClusterSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'cDBName' @::@ 'Maybe' 'Text'
--
-- * 'cElasticIpStatus' @::@ 'Maybe' 'ElasticIpStatus'
--
-- * 'cEncrypted' @::@ 'Maybe' 'Bool'
--
-- * 'cEndpoint' @::@ 'Maybe' 'Endpoint'
--
-- * 'cHsmStatus' @::@ 'Maybe' 'HsmStatus'
--
-- * 'cKmsKeyId' @::@ 'Maybe' 'Text'
--
-- * 'cMasterUsername' @::@ 'Maybe' 'Text'
--
-- * 'cModifyStatus' @::@ 'Maybe' 'Text'
--
-- * 'cNodeType' @::@ 'Maybe' 'Text'
--
-- * 'cNumberOfNodes' @::@ 'Maybe' 'Int'
--
-- * 'cPendingModifiedValues' @::@ 'Maybe' 'PendingModifiedValues'
--
-- * 'cPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'cPubliclyAccessible' @::@ 'Maybe' 'Bool'
--
-- * 'cRestoreStatus' @::@ 'Maybe' 'RestoreStatus'
--
-- * 'cTags' @::@ ['Tag']
--
-- * 'cVpcId' @::@ 'Maybe' 'Text'
--
-- * 'cVpcSecurityGroups' @::@ ['VpcSecurityGroupMembership']
--
cluster :: Cluster
cluster = Cluster
    { _cClusterIdentifier                = Nothing
    , _cNodeType                         = Nothing
    , _cClusterStatus                    = Nothing
    , _cModifyStatus                     = Nothing
    , _cMasterUsername                   = Nothing
    , _cDBName                           = Nothing
    , _cEndpoint                         = Nothing
    , _cClusterCreateTime                = Nothing
    , _cAutomatedSnapshotRetentionPeriod = Nothing
    , _cClusterSecurityGroups            = mempty
    , _cVpcSecurityGroups                = mempty
    , _cClusterParameterGroups           = mempty
    , _cClusterSubnetGroupName           = Nothing
    , _cVpcId                            = Nothing
    , _cAvailabilityZone                 = Nothing
    , _cPreferredMaintenanceWindow       = Nothing
    , _cPendingModifiedValues            = Nothing
    , _cClusterVersion                   = Nothing
    , _cAllowVersionUpgrade              = Nothing
    , _cNumberOfNodes                    = Nothing
    , _cPubliclyAccessible               = Nothing
    , _cEncrypted                        = Nothing
    , _cRestoreStatus                    = Nothing
    , _cHsmStatus                        = Nothing
    , _cClusterSnapshotCopyStatus        = Nothing
    , _cClusterPublicKey                 = Nothing
    , _cClusterNodes                     = mempty
    , _cElasticIpStatus                  = Nothing
    , _cClusterRevisionNumber            = Nothing
    , _cTags                             = mempty
    , _cKmsKeyId                         = Nothing
    }

-- | If 'true', major version upgrades will be applied automatically to the cluster
-- during the maintenance window.
cAllowVersionUpgrade :: Lens' Cluster (Maybe Bool)
cAllowVersionUpgrade =
    lens _cAllowVersionUpgrade (\s a -> s { _cAllowVersionUpgrade = a })

-- | The number of days that automatic cluster snapshots are retained.
cAutomatedSnapshotRetentionPeriod :: Lens' Cluster (Maybe Int)
cAutomatedSnapshotRetentionPeriod =
    lens _cAutomatedSnapshotRetentionPeriod
        (\s a -> s { _cAutomatedSnapshotRetentionPeriod = a })

-- | The name of the Availability Zone in which the cluster is located.
cAvailabilityZone :: Lens' Cluster (Maybe Text)
cAvailabilityZone =
    lens _cAvailabilityZone (\s a -> s { _cAvailabilityZone = a })

-- | The date and time that the cluster was created.
cClusterCreateTime :: Lens' Cluster (Maybe UTCTime)
cClusterCreateTime =
    lens _cClusterCreateTime (\s a -> s { _cClusterCreateTime = a })
        . mapping _Time

-- | The unique identifier of the cluster.
cClusterIdentifier :: Lens' Cluster (Maybe Text)
cClusterIdentifier =
    lens _cClusterIdentifier (\s a -> s { _cClusterIdentifier = a })

-- | The nodes in a cluster.
cClusterNodes :: Lens' Cluster [ClusterNode]
cClusterNodes = lens _cClusterNodes (\s a -> s { _cClusterNodes = a }) . _List

-- | The list of cluster parameter groups that are associated with this cluster.
cClusterParameterGroups :: Lens' Cluster [ClusterParameterGroupStatus]
cClusterParameterGroups =
    lens _cClusterParameterGroups (\s a -> s { _cClusterParameterGroups = a })
        . _List

-- | The public key for the cluster.
cClusterPublicKey :: Lens' Cluster (Maybe Text)
cClusterPublicKey =
    lens _cClusterPublicKey (\s a -> s { _cClusterPublicKey = a })

-- | The specific revision number of the database in the cluster.
cClusterRevisionNumber :: Lens' Cluster (Maybe Text)
cClusterRevisionNumber =
    lens _cClusterRevisionNumber (\s a -> s { _cClusterRevisionNumber = a })

-- | A list of cluster security group that are associated with the cluster. Each
-- security group is represented by an element that contains 'ClusterSecurityGroup.Name' and 'ClusterSecurityGroup.Status' subelements.
--
-- Cluster security groups are used when the cluster is not created in a VPC.
-- Clusters that are created in a VPC use VPC security groups, which are listed
-- by the VpcSecurityGroups parameter.
cClusterSecurityGroups :: Lens' Cluster [ClusterSecurityGroupMembership]
cClusterSecurityGroups =
    lens _cClusterSecurityGroups (\s a -> s { _cClusterSecurityGroups = a })
        . _List

-- | Returns the destination region and retention period that are configured for
-- cross-region snapshot copy.
cClusterSnapshotCopyStatus :: Lens' Cluster (Maybe ClusterSnapshotCopyStatus)
cClusterSnapshotCopyStatus =
    lens _cClusterSnapshotCopyStatus
        (\s a -> s { _cClusterSnapshotCopyStatus = a })

-- | The current state of this cluster. Possible values include 'available', 'creating', 'deleting', 'rebooting', 'renaming', and 'resizing'.
cClusterStatus :: Lens' Cluster (Maybe Text)
cClusterStatus = lens _cClusterStatus (\s a -> s { _cClusterStatus = a })

-- | The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
cClusterSubnetGroupName :: Lens' Cluster (Maybe Text)
cClusterSubnetGroupName =
    lens _cClusterSubnetGroupName (\s a -> s { _cClusterSubnetGroupName = a })

-- | The version ID of the Amazon Redshift engine that is running on the cluster.
cClusterVersion :: Lens' Cluster (Maybe Text)
cClusterVersion = lens _cClusterVersion (\s a -> s { _cClusterVersion = a })

-- | The name of the initial database that was created when the cluster was
-- created. This same name is returned for the life of the cluster. If an
-- initial database was not specified, a database named "dev" was created by
-- default.
cDBName :: Lens' Cluster (Maybe Text)
cDBName = lens _cDBName (\s a -> s { _cDBName = a })

-- | Describes the status of the elastic IP (EIP) address.
cElasticIpStatus :: Lens' Cluster (Maybe ElasticIpStatus)
cElasticIpStatus = lens _cElasticIpStatus (\s a -> s { _cElasticIpStatus = a })

-- | If 'true', data in the cluster is encrypted at rest.
cEncrypted :: Lens' Cluster (Maybe Bool)
cEncrypted = lens _cEncrypted (\s a -> s { _cEncrypted = a })

-- | The connection endpoint.
cEndpoint :: Lens' Cluster (Maybe Endpoint)
cEndpoint = lens _cEndpoint (\s a -> s { _cEndpoint = a })

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command.
--
-- Values: active, applying
cHsmStatus :: Lens' Cluster (Maybe HsmStatus)
cHsmStatus = lens _cHsmStatus (\s a -> s { _cHsmStatus = a })

-- | The AWS Key Management Service (KMS) key ID of the encryption key used to
-- encrypt data in the cluster.
cKmsKeyId :: Lens' Cluster (Maybe Text)
cKmsKeyId = lens _cKmsKeyId (\s a -> s { _cKmsKeyId = a })

-- | The master user name for the cluster. This name is used to connect to the
-- database that is specified in DBName.
cMasterUsername :: Lens' Cluster (Maybe Text)
cMasterUsername = lens _cMasterUsername (\s a -> s { _cMasterUsername = a })

-- | The status of a modify operation, if any, initiated for the cluster.
cModifyStatus :: Lens' Cluster (Maybe Text)
cModifyStatus = lens _cModifyStatus (\s a -> s { _cModifyStatus = a })

-- | The node type for the nodes in the cluster.
cNodeType :: Lens' Cluster (Maybe Text)
cNodeType = lens _cNodeType (\s a -> s { _cNodeType = a })

-- | The number of compute nodes in the cluster.
cNumberOfNodes :: Lens' Cluster (Maybe Int)
cNumberOfNodes = lens _cNumberOfNodes (\s a -> s { _cNumberOfNodes = a })

-- | If present, changes to the cluster are pending. Specific pending changes are
-- identified by subelements.
cPendingModifiedValues :: Lens' Cluster (Maybe PendingModifiedValues)
cPendingModifiedValues =
    lens _cPendingModifiedValues (\s a -> s { _cPendingModifiedValues = a })

-- | The weekly time range (in UTC) during which system maintenance can occur.
cPreferredMaintenanceWindow :: Lens' Cluster (Maybe Text)
cPreferredMaintenanceWindow =
    lens _cPreferredMaintenanceWindow
        (\s a -> s { _cPreferredMaintenanceWindow = a })

-- | If 'true', the cluster can be accessed from a public network.
cPubliclyAccessible :: Lens' Cluster (Maybe Bool)
cPubliclyAccessible =
    lens _cPubliclyAccessible (\s a -> s { _cPubliclyAccessible = a })

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
cRestoreStatus :: Lens' Cluster (Maybe RestoreStatus)
cRestoreStatus = lens _cRestoreStatus (\s a -> s { _cRestoreStatus = a })

-- | The list of tags for the cluster.
cTags :: Lens' Cluster [Tag]
cTags = lens _cTags (\s a -> s { _cTags = a }) . _List

-- | The identifier of the VPC the cluster is in, if the cluster is in a VPC.
cVpcId :: Lens' Cluster (Maybe Text)
cVpcId = lens _cVpcId (\s a -> s { _cVpcId = a })

-- | A list of Virtual Private Cloud (VPC) security groups that are associated
-- with the cluster. This parameter is returned only if the cluster is in a VPC.
cVpcSecurityGroups :: Lens' Cluster [VpcSecurityGroupMembership]
cVpcSecurityGroups =
    lens _cVpcSecurityGroups (\s a -> s { _cVpcSecurityGroups = a })
        . _List

instance FromXML Cluster where
    parseXML x = Cluster
        <$> x .@? "AllowVersionUpgrade"
        <*> x .@? "AutomatedSnapshotRetentionPeriod"
        <*> x .@? "AvailabilityZone"
        <*> x .@? "ClusterCreateTime"
        <*> x .@? "ClusterIdentifier"
        <*> x .@? "ClusterNodes" .!@ mempty
        <*> x .@? "ClusterParameterGroups" .!@ mempty
        <*> x .@? "ClusterPublicKey"
        <*> x .@? "ClusterRevisionNumber"
        <*> x .@? "ClusterSecurityGroups" .!@ mempty
        <*> x .@? "ClusterSnapshotCopyStatus"
        <*> x .@? "ClusterStatus"
        <*> x .@? "ClusterSubnetGroupName"
        <*> x .@? "ClusterVersion"
        <*> x .@? "DBName"
        <*> x .@? "ElasticIpStatus"
        <*> x .@? "Encrypted"
        <*> x .@? "Endpoint"
        <*> x .@? "HsmStatus"
        <*> x .@? "KmsKeyId"
        <*> x .@? "MasterUsername"
        <*> x .@? "ModifyStatus"
        <*> x .@? "NodeType"
        <*> x .@? "NumberOfNodes"
        <*> x .@? "PendingModifiedValues"
        <*> x .@? "PreferredMaintenanceWindow"
        <*> x .@? "PubliclyAccessible"
        <*> x .@? "RestoreStatus"
        <*> x .@? "Tags" .!@ mempty
        <*> x .@? "VpcId"
        <*> x .@? "VpcSecurityGroups" .!@ mempty

instance ToQuery Cluster where
    toQuery Cluster{..} = mconcat
        [ "AllowVersionUpgrade"              =? _cAllowVersionUpgrade
        , "AutomatedSnapshotRetentionPeriod" =? _cAutomatedSnapshotRetentionPeriod
        , "AvailabilityZone"                 =? _cAvailabilityZone
        , "ClusterCreateTime"                =? _cClusterCreateTime
        , "ClusterIdentifier"                =? _cClusterIdentifier
        , "ClusterNodes"                     =? _cClusterNodes
        , "ClusterParameterGroups"           =? _cClusterParameterGroups
        , "ClusterPublicKey"                 =? _cClusterPublicKey
        , "ClusterRevisionNumber"            =? _cClusterRevisionNumber
        , "ClusterSecurityGroups"            =? _cClusterSecurityGroups
        , "ClusterSnapshotCopyStatus"        =? _cClusterSnapshotCopyStatus
        , "ClusterStatus"                    =? _cClusterStatus
        , "ClusterSubnetGroupName"           =? _cClusterSubnetGroupName
        , "ClusterVersion"                   =? _cClusterVersion
        , "DBName"                           =? _cDBName
        , "ElasticIpStatus"                  =? _cElasticIpStatus
        , "Encrypted"                        =? _cEncrypted
        , "Endpoint"                         =? _cEndpoint
        , "HsmStatus"                        =? _cHsmStatus
        , "KmsKeyId"                         =? _cKmsKeyId
        , "MasterUsername"                   =? _cMasterUsername
        , "ModifyStatus"                     =? _cModifyStatus
        , "NodeType"                         =? _cNodeType
        , "NumberOfNodes"                    =? _cNumberOfNodes
        , "PendingModifiedValues"            =? _cPendingModifiedValues
        , "PreferredMaintenanceWindow"       =? _cPreferredMaintenanceWindow
        , "PubliclyAccessible"               =? _cPubliclyAccessible
        , "RestoreStatus"                    =? _cRestoreStatus
        , "Tags"                             =? _cTags
        , "VpcId"                            =? _cVpcId
        , "VpcSecurityGroups"                =? _cVpcSecurityGroups
        ]

data ClusterNode = ClusterNode
    { _cnNodeRole         :: Maybe Text
    , _cnPrivateIPAddress :: Maybe Text
    , _cnPublicIPAddress  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ClusterNode' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnNodeRole' @::@ 'Maybe' 'Text'
--
-- * 'cnPrivateIPAddress' @::@ 'Maybe' 'Text'
--
-- * 'cnPublicIPAddress' @::@ 'Maybe' 'Text'
--
clusterNode :: ClusterNode
clusterNode = ClusterNode
    { _cnNodeRole         = Nothing
    , _cnPrivateIPAddress = Nothing
    , _cnPublicIPAddress  = Nothing
    }

-- | Whether the node is a leader node or a compute node.
cnNodeRole :: Lens' ClusterNode (Maybe Text)
cnNodeRole = lens _cnNodeRole (\s a -> s { _cnNodeRole = a })

-- | The private IP address of a node within a cluster.
cnPrivateIPAddress :: Lens' ClusterNode (Maybe Text)
cnPrivateIPAddress =
    lens _cnPrivateIPAddress (\s a -> s { _cnPrivateIPAddress = a })

-- | The public IP address of a node within a cluster.
cnPublicIPAddress :: Lens' ClusterNode (Maybe Text)
cnPublicIPAddress =
    lens _cnPublicIPAddress (\s a -> s { _cnPublicIPAddress = a })

instance FromXML ClusterNode where
    parseXML x = ClusterNode
        <$> x .@? "NodeRole"
        <*> x .@? "PrivateIPAddress"
        <*> x .@? "PublicIPAddress"

instance ToQuery ClusterNode where
    toQuery ClusterNode{..} = mconcat
        [ "NodeRole"         =? _cnNodeRole
        , "PrivateIPAddress" =? _cnPrivateIPAddress
        , "PublicIPAddress"  =? _cnPublicIPAddress
        ]

data EC2SecurityGroup = EC2SecurityGroup
    { _ecsgEC2SecurityGroupName    :: Maybe Text
    , _ecsgEC2SecurityGroupOwnerId :: Maybe Text
    , _ecsgStatus                  :: Maybe Text
    , _ecsgTags                    :: List "member" Tag
    } deriving (Eq, Read, Show)

-- | 'EC2SecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ecsgEC2SecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ecsgEC2SecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'ecsgStatus' @::@ 'Maybe' 'Text'
--
-- * 'ecsgTags' @::@ ['Tag']
--
ec2SecurityGroup :: EC2SecurityGroup
ec2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus                  = Nothing
    , _ecsgEC2SecurityGroupName    = Nothing
    , _ecsgEC2SecurityGroupOwnerId = Nothing
    , _ecsgTags                    = mempty
    }

-- | The name of the EC2 Security Group.
ecsgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupName =
    lens _ecsgEC2SecurityGroupName
        (\s a -> s { _ecsgEC2SecurityGroupName = a })

-- | The AWS ID of the owner of the EC2 security group specified in the 'EC2SecurityGroupName' field.
ecsgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupOwnerId =
    lens _ecsgEC2SecurityGroupOwnerId
        (\s a -> s { _ecsgEC2SecurityGroupOwnerId = a })

-- | The status of the EC2 security group.
ecsgStatus :: Lens' EC2SecurityGroup (Maybe Text)
ecsgStatus = lens _ecsgStatus (\s a -> s { _ecsgStatus = a })

-- | The list of tags for the EC2 security group.
ecsgTags :: Lens' EC2SecurityGroup [Tag]
ecsgTags = lens _ecsgTags (\s a -> s { _ecsgTags = a }) . _List

instance FromXML EC2SecurityGroup where
    parseXML x = EC2SecurityGroup
        <$> x .@? "EC2SecurityGroupName"
        <*> x .@? "EC2SecurityGroupOwnerId"
        <*> x .@? "Status"
        <*> x .@? "Tags" .!@ mempty

instance ToQuery EC2SecurityGroup where
    toQuery EC2SecurityGroup{..} = mconcat
        [ "EC2SecurityGroupName"    =? _ecsgEC2SecurityGroupName
        , "EC2SecurityGroupOwnerId" =? _ecsgEC2SecurityGroupOwnerId
        , "Status"                  =? _ecsgStatus
        , "Tags"                    =? _ecsgTags
        ]

data OrderableClusterOption = OrderableClusterOption
    { _ocoAvailabilityZones :: List "member" AvailabilityZone
    , _ocoClusterType       :: Maybe Text
    , _ocoClusterVersion    :: Maybe Text
    , _ocoNodeType          :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'OrderableClusterOption' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ocoAvailabilityZones' @::@ ['AvailabilityZone']
--
-- * 'ocoClusterType' @::@ 'Maybe' 'Text'
--
-- * 'ocoClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'ocoNodeType' @::@ 'Maybe' 'Text'
--
orderableClusterOption :: OrderableClusterOption
orderableClusterOption = OrderableClusterOption
    { _ocoClusterVersion    = Nothing
    , _ocoClusterType       = Nothing
    , _ocoNodeType          = Nothing
    , _ocoAvailabilityZones = mempty
    }

-- | A list of availability zones for the orderable cluster.
ocoAvailabilityZones :: Lens' OrderableClusterOption [AvailabilityZone]
ocoAvailabilityZones =
    lens _ocoAvailabilityZones (\s a -> s { _ocoAvailabilityZones = a })
        . _List

-- | The cluster type, for example 'multi-node'.
ocoClusterType :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterType = lens _ocoClusterType (\s a -> s { _ocoClusterType = a })

-- | The version of the orderable cluster.
ocoClusterVersion :: Lens' OrderableClusterOption (Maybe Text)
ocoClusterVersion =
    lens _ocoClusterVersion (\s a -> s { _ocoClusterVersion = a })

-- | The node type for the orderable cluster.
ocoNodeType :: Lens' OrderableClusterOption (Maybe Text)
ocoNodeType = lens _ocoNodeType (\s a -> s { _ocoNodeType = a })

instance FromXML OrderableClusterOption where
    parseXML x = OrderableClusterOption
        <$> x .@? "AvailabilityZones" .!@ mempty
        <*> x .@? "ClusterType"
        <*> x .@? "ClusterVersion"
        <*> x .@? "NodeType"

instance ToQuery OrderableClusterOption where
    toQuery OrderableClusterOption{..} = mconcat
        [ "AvailabilityZones" =? _ocoAvailabilityZones
        , "ClusterType"       =? _ocoClusterType
        , "ClusterVersion"    =? _ocoClusterVersion
        , "NodeType"          =? _ocoNodeType
        ]

data SourceType
    = STCluster               -- ^ cluster
    | STClusterParameterGroup -- ^ cluster-parameter-group
    | STClusterSecurityGroup  -- ^ cluster-security-group
    | STClusterSnapshot       -- ^ cluster-snapshot
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable SourceType

instance FromText SourceType where
    parser = takeLowerText >>= \case
        "cluster"                 -> pure STCluster
        "cluster-parameter-group" -> pure STClusterParameterGroup
        "cluster-security-group"  -> pure STClusterSecurityGroup
        "cluster-snapshot"        -> pure STClusterSnapshot
        e                         -> fail $
            "Failure parsing SourceType from " ++ show e

instance ToText SourceType where
    toText = \case
        STCluster               -> "cluster"
        STClusterParameterGroup -> "cluster-parameter-group"
        STClusterSecurityGroup  -> "cluster-security-group"
        STClusterSnapshot       -> "cluster-snapshot"

instance ToByteString SourceType
instance ToHeader     SourceType
instance ToQuery      SourceType

instance FromXML SourceType where
    parseXML = parseXMLText "SourceType"

data ClusterParameterGroupStatus = ClusterParameterGroupStatus
    { _cpgsParameterApplyStatus :: Maybe Text
    , _cpgsParameterGroupName   :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ClusterParameterGroupStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgsParameterApplyStatus' @::@ 'Maybe' 'Text'
--
-- * 'cpgsParameterGroupName' @::@ 'Maybe' 'Text'
--
clusterParameterGroupStatus :: ClusterParameterGroupStatus
clusterParameterGroupStatus = ClusterParameterGroupStatus
    { _cpgsParameterGroupName   = Nothing
    , _cpgsParameterApplyStatus = Nothing
    }

-- | The status of parameter updates.
cpgsParameterApplyStatus :: Lens' ClusterParameterGroupStatus (Maybe Text)
cpgsParameterApplyStatus =
    lens _cpgsParameterApplyStatus
        (\s a -> s { _cpgsParameterApplyStatus = a })

-- | The name of the cluster parameter group.
cpgsParameterGroupName :: Lens' ClusterParameterGroupStatus (Maybe Text)
cpgsParameterGroupName =
    lens _cpgsParameterGroupName (\s a -> s { _cpgsParameterGroupName = a })

instance FromXML ClusterParameterGroupStatus where
    parseXML x = ClusterParameterGroupStatus
        <$> x .@? "ParameterApplyStatus"
        <*> x .@? "ParameterGroupName"

instance ToQuery ClusterParameterGroupStatus where
    toQuery ClusterParameterGroupStatus{..} = mconcat
        [ "ParameterApplyStatus" =? _cpgsParameterApplyStatus
        , "ParameterGroupName"   =? _cpgsParameterGroupName
        ]

data Subnet = Subnet
    { _sSubnetAvailabilityZone :: Maybe AvailabilityZone
    , _sSubnetIdentifier       :: Maybe Text
    , _sSubnetStatus           :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'Subnet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sSubnetAvailabilityZone' @::@ 'Maybe' 'AvailabilityZone'
--
-- * 'sSubnetIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'sSubnetStatus' @::@ 'Maybe' 'Text'
--
subnet :: Subnet
subnet = Subnet
    { _sSubnetIdentifier       = Nothing
    , _sSubnetAvailabilityZone = Nothing
    , _sSubnetStatus           = Nothing
    }

sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone =
    lens _sSubnetAvailabilityZone (\s a -> s { _sSubnetAvailabilityZone = a })

-- | The identifier of the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier =
    lens _sSubnetIdentifier (\s a -> s { _sSubnetIdentifier = a })

-- | The status of the subnet.
sSubnetStatus :: Lens' Subnet (Maybe Text)
sSubnetStatus = lens _sSubnetStatus (\s a -> s { _sSubnetStatus = a })

instance FromXML Subnet where
    parseXML x = Subnet
        <$> x .@? "SubnetAvailabilityZone"
        <*> x .@? "SubnetIdentifier"
        <*> x .@? "SubnetStatus"

instance ToQuery Subnet where
    toQuery Subnet{..} = mconcat
        [ "SubnetAvailabilityZone" =? _sSubnetAvailabilityZone
        , "SubnetIdentifier"       =? _sSubnetIdentifier
        , "SubnetStatus"           =? _sSubnetStatus
        ]

data ClusterSecurityGroup = ClusterSecurityGroup
    { _csgClusterSecurityGroupName :: Maybe Text
    , _csgDescription              :: Maybe Text
    , _csgEC2SecurityGroups        :: List "member" EC2SecurityGroup
    , _csgIPRanges                 :: List "member" IPRange
    , _csgTags                     :: List "member" Tag
    } deriving (Eq, Read, Show)

-- | 'ClusterSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgClusterSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'csgDescription' @::@ 'Maybe' 'Text'
--
-- * 'csgEC2SecurityGroups' @::@ ['EC2SecurityGroup']
--
-- * 'csgIPRanges' @::@ ['IPRange']
--
-- * 'csgTags' @::@ ['Tag']
--
clusterSecurityGroup :: ClusterSecurityGroup
clusterSecurityGroup = ClusterSecurityGroup
    { _csgClusterSecurityGroupName = Nothing
    , _csgDescription              = Nothing
    , _csgEC2SecurityGroups        = mempty
    , _csgIPRanges                 = mempty
    , _csgTags                     = mempty
    }

-- | The name of the cluster security group to which the operation was applied.
csgClusterSecurityGroupName :: Lens' ClusterSecurityGroup (Maybe Text)
csgClusterSecurityGroupName =
    lens _csgClusterSecurityGroupName
        (\s a -> s { _csgClusterSecurityGroupName = a })

-- | A description of the security group.
csgDescription :: Lens' ClusterSecurityGroup (Maybe Text)
csgDescription = lens _csgDescription (\s a -> s { _csgDescription = a })

-- | A list of EC2 security groups that are permitted to access clusters
-- associated with this cluster security group.
csgEC2SecurityGroups :: Lens' ClusterSecurityGroup [EC2SecurityGroup]
csgEC2SecurityGroups =
    lens _csgEC2SecurityGroups (\s a -> s { _csgEC2SecurityGroups = a })
        . _List

-- | A list of IP ranges (CIDR blocks) that are permitted to access clusters
-- associated with this cluster security group.
csgIPRanges :: Lens' ClusterSecurityGroup [IPRange]
csgIPRanges = lens _csgIPRanges (\s a -> s { _csgIPRanges = a }) . _List

-- | The list of tags for the cluster security group.
csgTags :: Lens' ClusterSecurityGroup [Tag]
csgTags = lens _csgTags (\s a -> s { _csgTags = a }) . _List

instance FromXML ClusterSecurityGroup where
    parseXML x = ClusterSecurityGroup
        <$> x .@? "ClusterSecurityGroupName"
        <*> x .@? "Description"
        <*> x .@? "EC2SecurityGroups" .!@ mempty
        <*> x .@? "IPRanges" .!@ mempty
        <*> x .@? "Tags" .!@ mempty

instance ToQuery ClusterSecurityGroup where
    toQuery ClusterSecurityGroup{..} = mconcat
        [ "ClusterSecurityGroupName" =? _csgClusterSecurityGroupName
        , "Description"              =? _csgDescription
        , "EC2SecurityGroups"        =? _csgEC2SecurityGroups
        , "IPRanges"                 =? _csgIPRanges
        , "Tags"                     =? _csgTags
        ]

data DefaultClusterParameters = DefaultClusterParameters
    { _dcpMarker               :: Maybe Text
    , _dcpParameterGroupFamily :: Maybe Text
    , _dcpParameters           :: List "member" Parameter
    } deriving (Eq, Read, Show)

-- | 'DefaultClusterParameters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcpParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'dcpParameters' @::@ ['Parameter']
--
defaultClusterParameters :: DefaultClusterParameters
defaultClusterParameters = DefaultClusterParameters
    { _dcpParameterGroupFamily = Nothing
    , _dcpMarker               = Nothing
    , _dcpParameters           = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker value
-- in the 'Marker' parameter and retrying the command. If the 'Marker' field is
-- empty, all response records have been retrieved for the request.
dcpMarker :: Lens' DefaultClusterParameters (Maybe Text)
dcpMarker = lens _dcpMarker (\s a -> s { _dcpMarker = a })

-- | The name of the cluster parameter group family to which the engine default
-- parameters apply.
dcpParameterGroupFamily :: Lens' DefaultClusterParameters (Maybe Text)
dcpParameterGroupFamily =
    lens _dcpParameterGroupFamily (\s a -> s { _dcpParameterGroupFamily = a })

-- | The list of cluster default parameters.
dcpParameters :: Lens' DefaultClusterParameters [Parameter]
dcpParameters = lens _dcpParameters (\s a -> s { _dcpParameters = a }) . _List

instance FromXML DefaultClusterParameters where
    parseXML x = DefaultClusterParameters
        <$> x .@? "Marker"
        <*> x .@? "ParameterGroupFamily"
        <*> x .@? "Parameters" .!@ mempty

instance ToQuery DefaultClusterParameters where
    toQuery DefaultClusterParameters{..} = mconcat
        [ "Marker"               =? _dcpMarker
        , "ParameterGroupFamily" =? _dcpParameterGroupFamily
        , "Parameters"           =? _dcpParameters
        ]

data ClusterSubnetGroup = ClusterSubnetGroup
    { _csg1ClusterSubnetGroupName :: Maybe Text
    , _csg1Description            :: Maybe Text
    , _csg1SubnetGroupStatus      :: Maybe Text
    , _csg1Subnets                :: List "member" Subnet
    , _csg1Tags                   :: List "member" Tag
    , _csg1VpcId                  :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ClusterSubnetGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csg1ClusterSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'csg1Description' @::@ 'Maybe' 'Text'
--
-- * 'csg1SubnetGroupStatus' @::@ 'Maybe' 'Text'
--
-- * 'csg1Subnets' @::@ ['Subnet']
--
-- * 'csg1Tags' @::@ ['Tag']
--
-- * 'csg1VpcId' @::@ 'Maybe' 'Text'
--
clusterSubnetGroup :: ClusterSubnetGroup
clusterSubnetGroup = ClusterSubnetGroup
    { _csg1ClusterSubnetGroupName = Nothing
    , _csg1Description            = Nothing
    , _csg1VpcId                  = Nothing
    , _csg1SubnetGroupStatus      = Nothing
    , _csg1Subnets                = mempty
    , _csg1Tags                   = mempty
    }

-- | The name of the cluster subnet group.
csg1ClusterSubnetGroupName :: Lens' ClusterSubnetGroup (Maybe Text)
csg1ClusterSubnetGroupName =
    lens _csg1ClusterSubnetGroupName
        (\s a -> s { _csg1ClusterSubnetGroupName = a })

-- | The description of the cluster subnet group.
csg1Description :: Lens' ClusterSubnetGroup (Maybe Text)
csg1Description = lens _csg1Description (\s a -> s { _csg1Description = a })

-- | The status of the cluster subnet group. Possible values are 'Complete', 'Incomplete' and 'Invalid'.
csg1SubnetGroupStatus :: Lens' ClusterSubnetGroup (Maybe Text)
csg1SubnetGroupStatus =
    lens _csg1SubnetGroupStatus (\s a -> s { _csg1SubnetGroupStatus = a })

-- | A list of the VPC 'Subnet' elements.
csg1Subnets :: Lens' ClusterSubnetGroup [Subnet]
csg1Subnets = lens _csg1Subnets (\s a -> s { _csg1Subnets = a }) . _List

-- | The list of tags for the cluster subnet group.
csg1Tags :: Lens' ClusterSubnetGroup [Tag]
csg1Tags = lens _csg1Tags (\s a -> s { _csg1Tags = a }) . _List

-- | The VPC ID of the cluster subnet group.
csg1VpcId :: Lens' ClusterSubnetGroup (Maybe Text)
csg1VpcId = lens _csg1VpcId (\s a -> s { _csg1VpcId = a })

instance FromXML ClusterSubnetGroup where
    parseXML x = ClusterSubnetGroup
        <$> x .@? "ClusterSubnetGroupName"
        <*> x .@? "Description"
        <*> x .@? "SubnetGroupStatus"
        <*> x .@? "Subnets" .!@ mempty
        <*> x .@? "Tags" .!@ mempty
        <*> x .@? "VpcId"

instance ToQuery ClusterSubnetGroup where
    toQuery ClusterSubnetGroup{..} = mconcat
        [ "ClusterSubnetGroupName" =? _csg1ClusterSubnetGroupName
        , "Description"            =? _csg1Description
        , "SubnetGroupStatus"      =? _csg1SubnetGroupStatus
        , "Subnets"                =? _csg1Subnets
        , "Tags"                   =? _csg1Tags
        , "VpcId"                  =? _csg1VpcId
        ]

data EventInfoMap = EventInfoMap
    { _eimEventCategories  :: List "member" Text
    , _eimEventDescription :: Maybe Text
    , _eimEventId          :: Maybe Text
    , _eimSeverity         :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'EventInfoMap' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eimEventCategories' @::@ ['Text']
--
-- * 'eimEventDescription' @::@ 'Maybe' 'Text'
--
-- * 'eimEventId' @::@ 'Maybe' 'Text'
--
-- * 'eimSeverity' @::@ 'Maybe' 'Text'
--
eventInfoMap :: EventInfoMap
eventInfoMap = EventInfoMap
    { _eimEventId          = Nothing
    , _eimEventCategories  = mempty
    , _eimEventDescription = Nothing
    , _eimSeverity         = Nothing
    }

-- | The category of an Amazon Redshift event.
eimEventCategories :: Lens' EventInfoMap [Text]
eimEventCategories =
    lens _eimEventCategories (\s a -> s { _eimEventCategories = a })
        . _List

-- | The description of an Amazon Redshift event.
eimEventDescription :: Lens' EventInfoMap (Maybe Text)
eimEventDescription =
    lens _eimEventDescription (\s a -> s { _eimEventDescription = a })

-- | The identifier of an Amazon Redshift event.
eimEventId :: Lens' EventInfoMap (Maybe Text)
eimEventId = lens _eimEventId (\s a -> s { _eimEventId = a })

-- | The severity of the event.
--
-- Values: ERROR, INFO
eimSeverity :: Lens' EventInfoMap (Maybe Text)
eimSeverity = lens _eimSeverity (\s a -> s { _eimSeverity = a })

instance FromXML EventInfoMap where
    parseXML x = EventInfoMap
        <$> x .@? "EventCategories" .!@ mempty
        <*> x .@? "EventDescription"
        <*> x .@? "EventId"
        <*> x .@? "Severity"

instance ToQuery EventInfoMap where
    toQuery EventInfoMap{..} = mconcat
        [ "EventCategories"  =? _eimEventCategories
        , "EventDescription" =? _eimEventDescription
        , "EventId"          =? _eimEventId
        , "Severity"         =? _eimSeverity
        ]

data ClusterSecurityGroupMembership = ClusterSecurityGroupMembership
    { _csgmClusterSecurityGroupName :: Maybe Text
    , _csgmStatus                   :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ClusterSecurityGroupMembership' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgmClusterSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'csgmStatus' @::@ 'Maybe' 'Text'
--
clusterSecurityGroupMembership :: ClusterSecurityGroupMembership
clusterSecurityGroupMembership = ClusterSecurityGroupMembership
    { _csgmClusterSecurityGroupName = Nothing
    , _csgmStatus                   = Nothing
    }

-- | The name of the cluster security group.
csgmClusterSecurityGroupName :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmClusterSecurityGroupName =
    lens _csgmClusterSecurityGroupName
        (\s a -> s { _csgmClusterSecurityGroupName = a })

-- | The status of the cluster security group.
csgmStatus :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmStatus = lens _csgmStatus (\s a -> s { _csgmStatus = a })

instance FromXML ClusterSecurityGroupMembership where
    parseXML x = ClusterSecurityGroupMembership
        <$> x .@? "ClusterSecurityGroupName"
        <*> x .@? "Status"

instance ToQuery ClusterSecurityGroupMembership where
    toQuery ClusterSecurityGroupMembership{..} = mconcat
        [ "ClusterSecurityGroupName" =? _csgmClusterSecurityGroupName
        , "Status"                   =? _csgmStatus
        ]

data ReservedNodeOffering = ReservedNodeOffering
    { _rnoCurrencyCode           :: Maybe Text
    , _rnoDuration               :: Maybe Int
    , _rnoFixedPrice             :: Maybe Double
    , _rnoNodeType               :: Maybe Text
    , _rnoOfferingType           :: Maybe Text
    , _rnoRecurringCharges       :: List "member" RecurringCharge
    , _rnoReservedNodeOfferingId :: Maybe Text
    , _rnoUsagePrice             :: Maybe Double
    } deriving (Eq, Read, Show)

-- | 'ReservedNodeOffering' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnoCurrencyCode' @::@ 'Maybe' 'Text'
--
-- * 'rnoDuration' @::@ 'Maybe' 'Int'
--
-- * 'rnoFixedPrice' @::@ 'Maybe' 'Double'
--
-- * 'rnoNodeType' @::@ 'Maybe' 'Text'
--
-- * 'rnoOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'rnoRecurringCharges' @::@ ['RecurringCharge']
--
-- * 'rnoReservedNodeOfferingId' @::@ 'Maybe' 'Text'
--
-- * 'rnoUsagePrice' @::@ 'Maybe' 'Double'
--
reservedNodeOffering :: ReservedNodeOffering
reservedNodeOffering = ReservedNodeOffering
    { _rnoReservedNodeOfferingId = Nothing
    , _rnoNodeType               = Nothing
    , _rnoDuration               = Nothing
    , _rnoFixedPrice             = Nothing
    , _rnoUsagePrice             = Nothing
    , _rnoCurrencyCode           = Nothing
    , _rnoOfferingType           = Nothing
    , _rnoRecurringCharges       = mempty
    }

-- | The currency code for the compute nodes offering.
rnoCurrencyCode :: Lens' ReservedNodeOffering (Maybe Text)
rnoCurrencyCode = lens _rnoCurrencyCode (\s a -> s { _rnoCurrencyCode = a })

-- | The duration, in seconds, for which the offering will reserve the node.
rnoDuration :: Lens' ReservedNodeOffering (Maybe Int)
rnoDuration = lens _rnoDuration (\s a -> s { _rnoDuration = a })

-- | The upfront fixed charge you will pay to purchase the specific reserved node
-- offering.
rnoFixedPrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoFixedPrice = lens _rnoFixedPrice (\s a -> s { _rnoFixedPrice = a })

-- | The node type offered by the reserved node offering.
rnoNodeType :: Lens' ReservedNodeOffering (Maybe Text)
rnoNodeType = lens _rnoNodeType (\s a -> s { _rnoNodeType = a })

-- | The anticipated utilization of the reserved node, as defined in the reserved
-- node offering.
rnoOfferingType :: Lens' ReservedNodeOffering (Maybe Text)
rnoOfferingType = lens _rnoOfferingType (\s a -> s { _rnoOfferingType = a })

-- | The charge to your account regardless of whether you are creating any
-- clusters using the node offering. Recurring charges are only in effect for
-- heavy-utilization reserved nodes.
rnoRecurringCharges :: Lens' ReservedNodeOffering [RecurringCharge]
rnoRecurringCharges =
    lens _rnoRecurringCharges (\s a -> s { _rnoRecurringCharges = a })
        . _List

-- | The offering identifier.
rnoReservedNodeOfferingId :: Lens' ReservedNodeOffering (Maybe Text)
rnoReservedNodeOfferingId =
    lens _rnoReservedNodeOfferingId
        (\s a -> s { _rnoReservedNodeOfferingId = a })

-- | The rate you are charged for each hour the cluster that is using the
-- offering is running.
rnoUsagePrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoUsagePrice = lens _rnoUsagePrice (\s a -> s { _rnoUsagePrice = a })

instance FromXML ReservedNodeOffering where
    parseXML x = ReservedNodeOffering
        <$> x .@? "CurrencyCode"
        <*> x .@? "Duration"
        <*> x .@? "FixedPrice"
        <*> x .@? "NodeType"
        <*> x .@? "OfferingType"
        <*> x .@? "RecurringCharges" .!@ mempty
        <*> x .@? "ReservedNodeOfferingId"
        <*> x .@? "UsagePrice"

instance ToQuery ReservedNodeOffering where
    toQuery ReservedNodeOffering{..} = mconcat
        [ "CurrencyCode"           =? _rnoCurrencyCode
        , "Duration"               =? _rnoDuration
        , "FixedPrice"             =? _rnoFixedPrice
        , "NodeType"               =? _rnoNodeType
        , "OfferingType"           =? _rnoOfferingType
        , "RecurringCharges"       =? _rnoRecurringCharges
        , "ReservedNodeOfferingId" =? _rnoReservedNodeOfferingId
        , "UsagePrice"             =? _rnoUsagePrice
        ]

data ReservedNode = ReservedNode
    { _rnCurrencyCode           :: Maybe Text
    , _rnDuration               :: Maybe Int
    , _rnFixedPrice             :: Maybe Double
    , _rnNodeCount              :: Maybe Int
    , _rnNodeType               :: Maybe Text
    , _rnOfferingType           :: Maybe Text
    , _rnRecurringCharges       :: List "member" RecurringCharge
    , _rnReservedNodeId         :: Maybe Text
    , _rnReservedNodeOfferingId :: Maybe Text
    , _rnStartTime              :: Maybe ISO8601
    , _rnState                  :: Maybe Text
    , _rnUsagePrice             :: Maybe Double
    } deriving (Eq, Read, Show)

-- | 'ReservedNode' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnCurrencyCode' @::@ 'Maybe' 'Text'
--
-- * 'rnDuration' @::@ 'Maybe' 'Int'
--
-- * 'rnFixedPrice' @::@ 'Maybe' 'Double'
--
-- * 'rnNodeCount' @::@ 'Maybe' 'Int'
--
-- * 'rnNodeType' @::@ 'Maybe' 'Text'
--
-- * 'rnOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'rnRecurringCharges' @::@ ['RecurringCharge']
--
-- * 'rnReservedNodeId' @::@ 'Maybe' 'Text'
--
-- * 'rnReservedNodeOfferingId' @::@ 'Maybe' 'Text'
--
-- * 'rnStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'rnState' @::@ 'Maybe' 'Text'
--
-- * 'rnUsagePrice' @::@ 'Maybe' 'Double'
--
reservedNode :: ReservedNode
reservedNode = ReservedNode
    { _rnReservedNodeId         = Nothing
    , _rnReservedNodeOfferingId = Nothing
    , _rnNodeType               = Nothing
    , _rnStartTime              = Nothing
    , _rnDuration               = Nothing
    , _rnFixedPrice             = Nothing
    , _rnUsagePrice             = Nothing
    , _rnCurrencyCode           = Nothing
    , _rnNodeCount              = Nothing
    , _rnState                  = Nothing
    , _rnOfferingType           = Nothing
    , _rnRecurringCharges       = mempty
    }

-- | The currency code for the reserved cluster.
rnCurrencyCode :: Lens' ReservedNode (Maybe Text)
rnCurrencyCode = lens _rnCurrencyCode (\s a -> s { _rnCurrencyCode = a })

-- | The duration of the node reservation in seconds.
rnDuration :: Lens' ReservedNode (Maybe Int)
rnDuration = lens _rnDuration (\s a -> s { _rnDuration = a })

-- | The fixed cost Amazon Redshift charged you for this reserved node.
rnFixedPrice :: Lens' ReservedNode (Maybe Double)
rnFixedPrice = lens _rnFixedPrice (\s a -> s { _rnFixedPrice = a })

-- | The number of reserved compute nodes.
rnNodeCount :: Lens' ReservedNode (Maybe Int)
rnNodeCount = lens _rnNodeCount (\s a -> s { _rnNodeCount = a })

-- | The node type of the reserved node.
rnNodeType :: Lens' ReservedNode (Maybe Text)
rnNodeType = lens _rnNodeType (\s a -> s { _rnNodeType = a })

-- | The anticipated utilization of the reserved node, as defined in the reserved
-- node offering.
rnOfferingType :: Lens' ReservedNode (Maybe Text)
rnOfferingType = lens _rnOfferingType (\s a -> s { _rnOfferingType = a })

-- | The recurring charges for the reserved node.
rnRecurringCharges :: Lens' ReservedNode [RecurringCharge]
rnRecurringCharges =
    lens _rnRecurringCharges (\s a -> s { _rnRecurringCharges = a })
        . _List

-- | The unique identifier for the reservation.
rnReservedNodeId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeId = lens _rnReservedNodeId (\s a -> s { _rnReservedNodeId = a })

-- | The identifier for the reserved node offering.
rnReservedNodeOfferingId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeOfferingId =
    lens _rnReservedNodeOfferingId
        (\s a -> s { _rnReservedNodeOfferingId = a })

-- | The time the reservation started. You purchase a reserved node offering for
-- a duration. This is the start time of that duration.
rnStartTime :: Lens' ReservedNode (Maybe UTCTime)
rnStartTime = lens _rnStartTime (\s a -> s { _rnStartTime = a }) . mapping _Time

-- | The state of the reserved compute node.
--
-- Possible Values:
--
-- pending-payment-This reserved node has recently been purchased, and the
-- sale has been approved, but payment has not yet been confirmed. active-This
-- reserved node is owned by the caller and is available for use. payment-failed-Payment failed for the purchase attempt.
--
rnState :: Lens' ReservedNode (Maybe Text)
rnState = lens _rnState (\s a -> s { _rnState = a })

-- | The hourly rate Amazon Redshift charge you for this reserved node.
rnUsagePrice :: Lens' ReservedNode (Maybe Double)
rnUsagePrice = lens _rnUsagePrice (\s a -> s { _rnUsagePrice = a })

instance FromXML ReservedNode where
    parseXML x = ReservedNode
        <$> x .@? "CurrencyCode"
        <*> x .@? "Duration"
        <*> x .@? "FixedPrice"
        <*> x .@? "NodeCount"
        <*> x .@? "NodeType"
        <*> x .@? "OfferingType"
        <*> x .@? "RecurringCharges" .!@ mempty
        <*> x .@? "ReservedNodeId"
        <*> x .@? "ReservedNodeOfferingId"
        <*> x .@? "StartTime"
        <*> x .@? "State"
        <*> x .@? "UsagePrice"

instance ToQuery ReservedNode where
    toQuery ReservedNode{..} = mconcat
        [ "CurrencyCode"           =? _rnCurrencyCode
        , "Duration"               =? _rnDuration
        , "FixedPrice"             =? _rnFixedPrice
        , "NodeCount"              =? _rnNodeCount
        , "NodeType"               =? _rnNodeType
        , "OfferingType"           =? _rnOfferingType
        , "RecurringCharges"       =? _rnRecurringCharges
        , "ReservedNodeId"         =? _rnReservedNodeId
        , "ReservedNodeOfferingId" =? _rnReservedNodeOfferingId
        , "StartTime"              =? _rnStartTime
        , "State"                  =? _rnState
        , "UsagePrice"             =? _rnUsagePrice
        ]

data LoggingStatus = LoggingStatus
    { _lsBucketName                 :: Maybe Text
    , _lsLastFailureMessage         :: Maybe Text
    , _lsLastFailureTime            :: Maybe ISO8601
    , _lsLastSuccessfulDeliveryTime :: Maybe ISO8601
    , _lsLoggingEnabled             :: Maybe Bool
    , _lsS3KeyPrefix                :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'LoggingStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsBucketName' @::@ 'Maybe' 'Text'
--
-- * 'lsLastFailureMessage' @::@ 'Maybe' 'Text'
--
-- * 'lsLastFailureTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'lsLastSuccessfulDeliveryTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'lsLoggingEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'lsS3KeyPrefix' @::@ 'Maybe' 'Text'
--
loggingStatus :: LoggingStatus
loggingStatus = LoggingStatus
    { _lsLoggingEnabled             = Nothing
    , _lsBucketName                 = Nothing
    , _lsS3KeyPrefix                = Nothing
    , _lsLastSuccessfulDeliveryTime = Nothing
    , _lsLastFailureTime            = Nothing
    , _lsLastFailureMessage         = Nothing
    }

-- | The name of the S3 bucket where the log files are stored.
lsBucketName :: Lens' LoggingStatus (Maybe Text)
lsBucketName = lens _lsBucketName (\s a -> s { _lsBucketName = a })

-- | The message indicating that logs failed to be delivered.
lsLastFailureMessage :: Lens' LoggingStatus (Maybe Text)
lsLastFailureMessage =
    lens _lsLastFailureMessage (\s a -> s { _lsLastFailureMessage = a })

-- | The last time when logs failed to be delivered.
lsLastFailureTime :: Lens' LoggingStatus (Maybe UTCTime)
lsLastFailureTime =
    lens _lsLastFailureTime (\s a -> s { _lsLastFailureTime = a })
        . mapping _Time

-- | The last time when logs were delivered.
lsLastSuccessfulDeliveryTime :: Lens' LoggingStatus (Maybe UTCTime)
lsLastSuccessfulDeliveryTime =
    lens _lsLastSuccessfulDeliveryTime
        (\s a -> s { _lsLastSuccessfulDeliveryTime = a })
            . mapping _Time

-- | 'true' if logging is on, 'false' if logging is off.
lsLoggingEnabled :: Lens' LoggingStatus (Maybe Bool)
lsLoggingEnabled = lens _lsLoggingEnabled (\s a -> s { _lsLoggingEnabled = a })

-- | The prefix applied to the log file names.
lsS3KeyPrefix :: Lens' LoggingStatus (Maybe Text)
lsS3KeyPrefix = lens _lsS3KeyPrefix (\s a -> s { _lsS3KeyPrefix = a })

instance FromXML LoggingStatus where
    parseXML x = LoggingStatus
        <$> x .@? "BucketName"
        <*> x .@? "LastFailureMessage"
        <*> x .@? "LastFailureTime"
        <*> x .@? "LastSuccessfulDeliveryTime"
        <*> x .@? "LoggingEnabled"
        <*> x .@? "S3KeyPrefix"

instance ToQuery LoggingStatus where
    toQuery LoggingStatus{..} = mconcat
        [ "BucketName"                 =? _lsBucketName
        , "LastFailureMessage"         =? _lsLastFailureMessage
        , "LastFailureTime"            =? _lsLastFailureTime
        , "LastSuccessfulDeliveryTime" =? _lsLastSuccessfulDeliveryTime
        , "LoggingEnabled"             =? _lsLoggingEnabled
        , "S3KeyPrefix"                =? _lsS3KeyPrefix
        ]

newtype AccountWithRestoreAccess = AccountWithRestoreAccess
    { _awraAccountId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'AccountWithRestoreAccess' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'awraAccountId' @::@ 'Maybe' 'Text'
--
accountWithRestoreAccess :: AccountWithRestoreAccess
accountWithRestoreAccess = AccountWithRestoreAccess
    { _awraAccountId = Nothing
    }

-- | The identifier of an AWS customer account authorized to restore a snapshot.
awraAccountId :: Lens' AccountWithRestoreAccess (Maybe Text)
awraAccountId = lens _awraAccountId (\s a -> s { _awraAccountId = a })

instance FromXML AccountWithRestoreAccess where
    parseXML x = AccountWithRestoreAccess
        <$> x .@? "AccountId"

instance ToQuery AccountWithRestoreAccess where
    toQuery AccountWithRestoreAccess{..} = mconcat
        [ "AccountId" =? _awraAccountId
        ]

newtype AvailabilityZone = AvailabilityZone
    { _azName :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'AvailabilityZone' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'azName' @::@ 'Maybe' 'Text'
--
availabilityZone :: AvailabilityZone
availabilityZone = AvailabilityZone
    { _azName = Nothing
    }

-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\s a -> s { _azName = a })

instance FromXML AvailabilityZone where
    parseXML x = AvailabilityZone
        <$> x .@? "Name"

instance ToQuery AvailabilityZone where
    toQuery AvailabilityZone{..} = mconcat
        [ "Name" =? _azName
        ]

data EventSubscription = EventSubscription
    { _esCustSubscriptionId       :: Maybe Text
    , _esCustomerAwsId            :: Maybe Text
    , _esEnabled                  :: Maybe Bool
    , _esEventCategoriesList      :: List "member" Text
    , _esSeverity                 :: Maybe Text
    , _esSnsTopicArn              :: Maybe Text
    , _esSourceIdsList            :: List "member" Text
    , _esSourceType               :: Maybe Text
    , _esStatus                   :: Maybe Text
    , _esSubscriptionCreationTime :: Maybe ISO8601
    , _esTags                     :: List "member" Tag
    } deriving (Eq, Read, Show)

-- | 'EventSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esCustSubscriptionId' @::@ 'Maybe' 'Text'
--
-- * 'esCustomerAwsId' @::@ 'Maybe' 'Text'
--
-- * 'esEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'esEventCategoriesList' @::@ ['Text']
--
-- * 'esSeverity' @::@ 'Maybe' 'Text'
--
-- * 'esSnsTopicArn' @::@ 'Maybe' 'Text'
--
-- * 'esSourceIdsList' @::@ ['Text']
--
-- * 'esSourceType' @::@ 'Maybe' 'Text'
--
-- * 'esStatus' @::@ 'Maybe' 'Text'
--
-- * 'esSubscriptionCreationTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'esTags' @::@ ['Tag']
--
eventSubscription :: EventSubscription
eventSubscription = EventSubscription
    { _esCustomerAwsId            = Nothing
    , _esCustSubscriptionId       = Nothing
    , _esSnsTopicArn              = Nothing
    , _esStatus                   = Nothing
    , _esSubscriptionCreationTime = Nothing
    , _esSourceType               = Nothing
    , _esSourceIdsList            = mempty
    , _esEventCategoriesList      = mempty
    , _esSeverity                 = Nothing
    , _esEnabled                  = Nothing
    , _esTags                     = mempty
    }

-- | The name of the Amazon Redshift event notification subscription.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId =
    lens _esCustSubscriptionId (\s a -> s { _esCustSubscriptionId = a })

-- | The AWS customer account associated with the Amazon Redshift event
-- notification subscription.
esCustomerAwsId :: Lens' EventSubscription (Maybe Text)
esCustomerAwsId = lens _esCustomerAwsId (\s a -> s { _esCustomerAwsId = a })

-- | A Boolean value indicating whether the subscription is enabled. 'true'
-- indicates the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled = lens _esEnabled (\s a -> s { _esEnabled = a })

-- | The list of Amazon Redshift event categories specified in the event
-- notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security
esEventCategoriesList :: Lens' EventSubscription [Text]
esEventCategoriesList =
    lens _esEventCategoriesList (\s a -> s { _esEventCategoriesList = a })
        . _List

-- | The event severity specified in the Amazon Redshift event notification
-- subscription.
--
-- Values: ERROR, INFO
esSeverity :: Lens' EventSubscription (Maybe Text)
esSeverity = lens _esSeverity (\s a -> s { _esSeverity = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
esSnsTopicArn :: Lens' EventSubscription (Maybe Text)
esSnsTopicArn = lens _esSnsTopicArn (\s a -> s { _esSnsTopicArn = a })

-- | A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
esSourceIdsList :: Lens' EventSubscription [Text]
esSourceIdsList = lens _esSourceIdsList (\s a -> s { _esSourceIdsList = a }) . _List

-- | The source type of the events returned the Amazon Redshift event
-- notification, such as cluster, or cluster-snapshot.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType = lens _esSourceType (\s a -> s { _esSourceType = a })

-- | The status of the Amazon Redshift event notification subscription.
--
-- Constraints:
--
-- Can be one of the following: active | no-permission | topic-not-exist The
-- status "no-permission" indicates that Amazon Redshift no longer has
-- permission to post to the Amazon SNS topic. The status "topic-not-exist"
-- indicates that the topic was deleted after the subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus = lens _esStatus (\s a -> s { _esStatus = a })

-- | The date and time the Amazon Redshift event notification subscription was
-- created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe UTCTime)
esSubscriptionCreationTime =
    lens _esSubscriptionCreationTime
        (\s a -> s { _esSubscriptionCreationTime = a })
            . mapping _Time

-- | The list of tags for the event subscription.
esTags :: Lens' EventSubscription [Tag]
esTags = lens _esTags (\s a -> s { _esTags = a }) . _List

instance FromXML EventSubscription where
    parseXML x = EventSubscription
        <$> x .@? "CustSubscriptionId"
        <*> x .@? "CustomerAwsId"
        <*> x .@? "Enabled"
        <*> x .@? "EventCategoriesList" .!@ mempty
        <*> x .@? "Severity"
        <*> x .@? "SnsTopicArn"
        <*> x .@? "SourceIdsList" .!@ mempty
        <*> x .@? "SourceType"
        <*> x .@? "Status"
        <*> x .@? "SubscriptionCreationTime"
        <*> x .@? "Tags" .!@ mempty

instance ToQuery EventSubscription where
    toQuery EventSubscription{..} = mconcat
        [ "CustSubscriptionId"       =? _esCustSubscriptionId
        , "CustomerAwsId"            =? _esCustomerAwsId
        , "Enabled"                  =? _esEnabled
        , "EventCategoriesList"      =? _esEventCategoriesList
        , "Severity"                 =? _esSeverity
        , "SnsTopicArn"              =? _esSnsTopicArn
        , "SourceIdsList"            =? _esSourceIdsList
        , "SourceType"               =? _esSourceType
        , "Status"                   =? _esStatus
        , "SubscriptionCreationTime" =? _esSubscriptionCreationTime
        , "Tags"                     =? _esTags
        ]

data HsmStatus = HsmStatus
    { _hsHsmClientCertificateIdentifier :: Maybe Text
    , _hsHsmConfigurationIdentifier     :: Maybe Text
    , _hsStatus                         :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'HsmStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hsHsmClientCertificateIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'hsHsmConfigurationIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'hsStatus' @::@ 'Maybe' 'Text'
--
hsmStatus :: HsmStatus
hsmStatus = HsmStatus
    { _hsHsmClientCertificateIdentifier = Nothing
    , _hsHsmConfigurationIdentifier     = Nothing
    , _hsStatus                         = Nothing
    }

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster
-- uses to retrieve the data encryption keys stored in an HSM.
hsHsmClientCertificateIdentifier :: Lens' HsmStatus (Maybe Text)
hsHsmClientCertificateIdentifier =
    lens _hsHsmClientCertificateIdentifier
        (\s a -> s { _hsHsmClientCertificateIdentifier = a })

-- | Specifies the name of the HSM configuration that contains the information the
-- Amazon Redshift cluster can use to retrieve and store keys in an HSM.
hsHsmConfigurationIdentifier :: Lens' HsmStatus (Maybe Text)
hsHsmConfigurationIdentifier =
    lens _hsHsmConfigurationIdentifier
        (\s a -> s { _hsHsmConfigurationIdentifier = a })

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command.
--
-- Values: active, applying
hsStatus :: Lens' HsmStatus (Maybe Text)
hsStatus = lens _hsStatus (\s a -> s { _hsStatus = a })

instance FromXML HsmStatus where
    parseXML x = HsmStatus
        <$> x .@? "HsmClientCertificateIdentifier"
        <*> x .@? "HsmConfigurationIdentifier"
        <*> x .@? "Status"

instance ToQuery HsmStatus where
    toQuery HsmStatus{..} = mconcat
        [ "HsmClientCertificateIdentifier" =? _hsHsmClientCertificateIdentifier
        , "HsmConfigurationIdentifier"     =? _hsHsmConfigurationIdentifier
        , "Status"                         =? _hsStatus
        ]

data ClusterParameterGroupNameMessage = ClusterParameterGroupNameMessage
    { _cpgnmParameterGroupName   :: Maybe Text
    , _cpgnmParameterGroupStatus :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ClusterParameterGroupNameMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgnmParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cpgnmParameterGroupStatus' @::@ 'Maybe' 'Text'
--
clusterParameterGroupNameMessage :: ClusterParameterGroupNameMessage
clusterParameterGroupNameMessage = ClusterParameterGroupNameMessage
    { _cpgnmParameterGroupName   = Nothing
    , _cpgnmParameterGroupStatus = Nothing
    }

-- | The name of the cluster parameter group.
cpgnmParameterGroupName :: Lens' ClusterParameterGroupNameMessage (Maybe Text)
cpgnmParameterGroupName =
    lens _cpgnmParameterGroupName (\s a -> s { _cpgnmParameterGroupName = a })

-- | The status of the parameter group. For example, if you made a change to a
-- parameter group name-value pair, then the change could be pending a reboot of
-- an associated cluster.
cpgnmParameterGroupStatus :: Lens' ClusterParameterGroupNameMessage (Maybe Text)
cpgnmParameterGroupStatus =
    lens _cpgnmParameterGroupStatus
        (\s a -> s { _cpgnmParameterGroupStatus = a })

instance FromXML ClusterParameterGroupNameMessage where
    parseXML x = ClusterParameterGroupNameMessage
        <$> x .@? "ParameterGroupName"
        <*> x .@? "ParameterGroupStatus"

instance ToQuery ClusterParameterGroupNameMessage where
    toQuery ClusterParameterGroupNameMessage{..} = mconcat
        [ "ParameterGroupName"   =? _cpgnmParameterGroupName
        , "ParameterGroupStatus" =? _cpgnmParameterGroupStatus
        ]

data ElasticIpStatus = ElasticIpStatus
    { _eisElasticIp :: Maybe Text
    , _eisStatus    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ElasticIpStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eisElasticIp' @::@ 'Maybe' 'Text'
--
-- * 'eisStatus' @::@ 'Maybe' 'Text'
--
elasticIpStatus :: ElasticIpStatus
elasticIpStatus = ElasticIpStatus
    { _eisElasticIp = Nothing
    , _eisStatus    = Nothing
    }

-- | The elastic IP (EIP) address for the cluster.
eisElasticIp :: Lens' ElasticIpStatus (Maybe Text)
eisElasticIp = lens _eisElasticIp (\s a -> s { _eisElasticIp = a })

-- | Describes the status of the elastic IP (EIP) address.
eisStatus :: Lens' ElasticIpStatus (Maybe Text)
eisStatus = lens _eisStatus (\s a -> s { _eisStatus = a })

instance FromXML ElasticIpStatus where
    parseXML x = ElasticIpStatus
        <$> x .@? "ElasticIp"
        <*> x .@? "Status"

instance ToQuery ElasticIpStatus where
    toQuery ElasticIpStatus{..} = mconcat
        [ "ElasticIp" =? _eisElasticIp
        , "Status"    =? _eisStatus
        ]

data ClusterVersion = ClusterVersion
    { _cvClusterParameterGroupFamily :: Maybe Text
    , _cvClusterVersion              :: Maybe Text
    , _cvDescription                 :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ClusterVersion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvClusterParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'cvClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'cvDescription' @::@ 'Maybe' 'Text'
--
clusterVersion :: ClusterVersion
clusterVersion = ClusterVersion
    { _cvClusterVersion              = Nothing
    , _cvClusterParameterGroupFamily = Nothing
    , _cvDescription                 = Nothing
    }

-- | The name of the cluster parameter group family for the cluster.
cvClusterParameterGroupFamily :: Lens' ClusterVersion (Maybe Text)
cvClusterParameterGroupFamily =
    lens _cvClusterParameterGroupFamily
        (\s a -> s { _cvClusterParameterGroupFamily = a })

-- | The version number used by the cluster.
cvClusterVersion :: Lens' ClusterVersion (Maybe Text)
cvClusterVersion = lens _cvClusterVersion (\s a -> s { _cvClusterVersion = a })

-- | The description of the cluster version.
cvDescription :: Lens' ClusterVersion (Maybe Text)
cvDescription = lens _cvDescription (\s a -> s { _cvDescription = a })

instance FromXML ClusterVersion where
    parseXML x = ClusterVersion
        <$> x .@? "ClusterParameterGroupFamily"
        <*> x .@? "ClusterVersion"
        <*> x .@? "Description"

instance ToQuery ClusterVersion where
    toQuery ClusterVersion{..} = mconcat
        [ "ClusterParameterGroupFamily" =? _cvClusterParameterGroupFamily
        , "ClusterVersion"              =? _cvClusterVersion
        , "Description"                 =? _cvDescription
        ]

data RecurringCharge = RecurringCharge
    { _rcRecurringChargeAmount    :: Maybe Double
    , _rcRecurringChargeFrequency :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RecurringCharge' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcRecurringChargeAmount' @::@ 'Maybe' 'Double'
--
-- * 'rcRecurringChargeFrequency' @::@ 'Maybe' 'Text'
--
recurringCharge :: RecurringCharge
recurringCharge = RecurringCharge
    { _rcRecurringChargeAmount    = Nothing
    , _rcRecurringChargeFrequency = Nothing
    }

-- | The amount charged per the period of time specified by the recurring charge
-- frequency.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount =
    lens _rcRecurringChargeAmount (\s a -> s { _rcRecurringChargeAmount = a })

-- | The frequency at which the recurring charge amount is applied.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency =
    lens _rcRecurringChargeFrequency
        (\s a -> s { _rcRecurringChargeFrequency = a })

instance FromXML RecurringCharge where
    parseXML x = RecurringCharge
        <$> x .@? "RecurringChargeAmount"
        <*> x .@? "RecurringChargeFrequency"

instance ToQuery RecurringCharge where
    toQuery RecurringCharge{..} = mconcat
        [ "RecurringChargeAmount"    =? _rcRecurringChargeAmount
        , "RecurringChargeFrequency" =? _rcRecurringChargeFrequency
        ]

data Endpoint = Endpoint
    { _eAddress :: Maybe Text
    , _ePort    :: Maybe Int
    } deriving (Eq, Ord, Read, Show)

-- | 'Endpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eAddress' @::@ 'Maybe' 'Text'
--
-- * 'ePort' @::@ 'Maybe' 'Int'
--
endpoint :: Endpoint
endpoint = Endpoint
    { _eAddress = Nothing
    , _ePort    = Nothing
    }

-- | The DNS address of the Cluster.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\s a -> s { _eAddress = a })

-- | The port that the database engine is listening on.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\s a -> s { _ePort = a })

instance FromXML Endpoint where
    parseXML x = Endpoint
        <$> x .@? "Address"
        <*> x .@? "Port"

instance ToQuery Endpoint where
    toQuery Endpoint{..} = mconcat
        [ "Address" =? _eAddress
        , "Port"    =? _ePort
        ]

data IPRange = IPRange
    { _iprCIDRIP :: Maybe Text
    , _iprStatus :: Maybe Text
    , _iprTags   :: List "member" Tag
    } deriving (Eq, Read, Show)

-- | 'IPRange' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iprCIDRIP' @::@ 'Maybe' 'Text'
--
-- * 'iprStatus' @::@ 'Maybe' 'Text'
--
-- * 'iprTags' @::@ ['Tag']
--
iprange :: IPRange
iprange = IPRange
    { _iprStatus = Nothing
    , _iprCIDRIP = Nothing
    , _iprTags   = mempty
    }

-- | The IP range in Classless Inter-Domain Routing (CIDR) notation.
iprCIDRIP :: Lens' IPRange (Maybe Text)
iprCIDRIP = lens _iprCIDRIP (\s a -> s { _iprCIDRIP = a })

-- | The status of the IP range, for example, "authorized".
iprStatus :: Lens' IPRange (Maybe Text)
iprStatus = lens _iprStatus (\s a -> s { _iprStatus = a })

-- | The list of tags for the IP range.
iprTags :: Lens' IPRange [Tag]
iprTags = lens _iprTags (\s a -> s { _iprTags = a }) . _List

instance FromXML IPRange where
    parseXML x = IPRange
        <$> x .@? "CIDRIP"
        <*> x .@? "Status"
        <*> x .@? "Tags" .!@ mempty

instance ToQuery IPRange where
    toQuery IPRange{..} = mconcat
        [ "CIDRIP" =? _iprCIDRIP
        , "Status" =? _iprStatus
        , "Tags"   =? _iprTags
        ]

data TaggedResource = TaggedResource
    { _trResourceName :: Maybe Text
    , _trResourceType :: Maybe Text
    , _trTag          :: Maybe Tag
    } deriving (Eq, Read, Show)

-- | 'TaggedResource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'trResourceName' @::@ 'Maybe' 'Text'
--
-- * 'trResourceType' @::@ 'Maybe' 'Text'
--
-- * 'trTag' @::@ 'Maybe' 'Tag'
--
taggedResource :: TaggedResource
taggedResource = TaggedResource
    { _trTag          = Nothing
    , _trResourceName = Nothing
    , _trResourceType = Nothing
    }

-- | The Amazon Resource Name (ARN) with which the tag is associated. For example, 'arn:aws:redshift:us-east-1:123456789:cluster:t1'.
trResourceName :: Lens' TaggedResource (Maybe Text)
trResourceName = lens _trResourceName (\s a -> s { _trResourceName = a })

-- | The type of resource with which the tag is associated. Valid resource types
-- are:  Cluster CIDR/IP EC2 security group Snapshot Cluster security group Subnet group
-- HSM connection HSM certificate Parameter group
--
-- For more information about Amazon Redshift resource types and constructing
-- ARNs, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/constructing-redshift-arn.html Constructing an Amazon Redshift Amazon Resource Name (ARN)> in the
-- Amazon Redshift Cluster Management Guide.
trResourceType :: Lens' TaggedResource (Maybe Text)
trResourceType = lens _trResourceType (\s a -> s { _trResourceType = a })

-- | The tag for the resource.
trTag :: Lens' TaggedResource (Maybe Tag)
trTag = lens _trTag (\s a -> s { _trTag = a })

instance FromXML TaggedResource where
    parseXML x = TaggedResource
        <$> x .@? "ResourceName"
        <*> x .@? "ResourceType"
        <*> x .@? "Tag"

instance ToQuery TaggedResource where
    toQuery TaggedResource{..} = mconcat
        [ "ResourceName" =? _trResourceName
        , "ResourceType" =? _trResourceType
        , "Tag"          =? _trTag
        ]

data EventCategoriesMap = EventCategoriesMap
    { _ecmEvents     :: List "member" EventInfoMap
    , _ecmSourceType :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'EventCategoriesMap' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ecmEvents' @::@ ['EventInfoMap']
--
-- * 'ecmSourceType' @::@ 'Maybe' 'Text'
--
eventCategoriesMap :: EventCategoriesMap
eventCategoriesMap = EventCategoriesMap
    { _ecmSourceType = Nothing
    , _ecmEvents     = mempty
    }

-- | The events in the event category.
ecmEvents :: Lens' EventCategoriesMap [EventInfoMap]
ecmEvents = lens _ecmEvents (\s a -> s { _ecmEvents = a }) . _List

-- | The Amazon Redshift source type, such as cluster or cluster-snapshot, that
-- the returned categories belong to.
ecmSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecmSourceType = lens _ecmSourceType (\s a -> s { _ecmSourceType = a })

instance FromXML EventCategoriesMap where
    parseXML x = EventCategoriesMap
        <$> x .@? "Events" .!@ mempty
        <*> x .@? "SourceType"

instance ToQuery EventCategoriesMap where
    toQuery EventCategoriesMap{..} = mconcat
        [ "Events"     =? _ecmEvents
        , "SourceType" =? _ecmSourceType
        ]

data HsmConfiguration = HsmConfiguration
    { _hcDescription                :: Maybe Text
    , _hcHsmConfigurationIdentifier :: Maybe Text
    , _hcHsmIpAddress               :: Maybe Text
    , _hcHsmPartitionName           :: Maybe Text
    , _hcTags                       :: List "member" Tag
    } deriving (Eq, Read, Show)

-- | 'HsmConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hcDescription' @::@ 'Maybe' 'Text'
--
-- * 'hcHsmConfigurationIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'hcHsmIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'hcHsmPartitionName' @::@ 'Maybe' 'Text'
--
-- * 'hcTags' @::@ ['Tag']
--
hsmConfiguration :: HsmConfiguration
hsmConfiguration = HsmConfiguration
    { _hcHsmConfigurationIdentifier = Nothing
    , _hcDescription                = Nothing
    , _hcHsmIpAddress               = Nothing
    , _hcHsmPartitionName           = Nothing
    , _hcTags                       = mempty
    }

-- | A text description of the HSM configuration.
hcDescription :: Lens' HsmConfiguration (Maybe Text)
hcDescription = lens _hcDescription (\s a -> s { _hcDescription = a })

-- | The name of the Amazon Redshift HSM configuration.
hcHsmConfigurationIdentifier :: Lens' HsmConfiguration (Maybe Text)
hcHsmConfigurationIdentifier =
    lens _hcHsmConfigurationIdentifier
        (\s a -> s { _hcHsmConfigurationIdentifier = a })

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
hcHsmIpAddress :: Lens' HsmConfiguration (Maybe Text)
hcHsmIpAddress = lens _hcHsmIpAddress (\s a -> s { _hcHsmIpAddress = a })

-- | The name of the partition in the HSM where the Amazon Redshift clusters will
-- store their database encryption keys.
hcHsmPartitionName :: Lens' HsmConfiguration (Maybe Text)
hcHsmPartitionName =
    lens _hcHsmPartitionName (\s a -> s { _hcHsmPartitionName = a })

-- | The list of tags for the HSM configuration.
hcTags :: Lens' HsmConfiguration [Tag]
hcTags = lens _hcTags (\s a -> s { _hcTags = a }) . _List

instance FromXML HsmConfiguration where
    parseXML x = HsmConfiguration
        <$> x .@? "Description"
        <*> x .@? "HsmConfigurationIdentifier"
        <*> x .@? "HsmIpAddress"
        <*> x .@? "HsmPartitionName"
        <*> x .@? "Tags" .!@ mempty

instance ToQuery HsmConfiguration where
    toQuery HsmConfiguration{..} = mconcat
        [ "Description"                =? _hcDescription
        , "HsmConfigurationIdentifier" =? _hcHsmConfigurationIdentifier
        , "HsmIpAddress"               =? _hcHsmIpAddress
        , "HsmPartitionName"           =? _hcHsmPartitionName
        , "Tags"                       =? _hcTags
        ]

data PendingModifiedValues = PendingModifiedValues
    { _pmvAutomatedSnapshotRetentionPeriod :: Maybe Int
    , _pmvClusterIdentifier                :: Maybe Text
    , _pmvClusterType                      :: Maybe Text
    , _pmvClusterVersion                   :: Maybe Text
    , _pmvMasterUserPassword               :: Maybe Text
    , _pmvNodeType                         :: Maybe Text
    , _pmvNumberOfNodes                    :: Maybe Int
    } deriving (Eq, Ord, Read, Show)

-- | 'PendingModifiedValues' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmvAutomatedSnapshotRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'pmvClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'pmvClusterType' @::@ 'Maybe' 'Text'
--
-- * 'pmvClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'pmvMasterUserPassword' @::@ 'Maybe' 'Text'
--
-- * 'pmvNodeType' @::@ 'Maybe' 'Text'
--
-- * 'pmvNumberOfNodes' @::@ 'Maybe' 'Int'
--
pendingModifiedValues :: PendingModifiedValues
pendingModifiedValues = PendingModifiedValues
    { _pmvMasterUserPassword               = Nothing
    , _pmvNodeType                         = Nothing
    , _pmvNumberOfNodes                    = Nothing
    , _pmvClusterType                      = Nothing
    , _pmvClusterVersion                   = Nothing
    , _pmvAutomatedSnapshotRetentionPeriod = Nothing
    , _pmvClusterIdentifier                = Nothing
    }

-- | The pending or in-progress change of the automated snapshot retention
-- period.
pmvAutomatedSnapshotRetentionPeriod :: Lens' PendingModifiedValues (Maybe Int)
pmvAutomatedSnapshotRetentionPeriod =
    lens _pmvAutomatedSnapshotRetentionPeriod
        (\s a -> s { _pmvAutomatedSnapshotRetentionPeriod = a })

-- | The pending or in-progress change of the new identifier for the cluster.
pmvClusterIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterIdentifier =
    lens _pmvClusterIdentifier (\s a -> s { _pmvClusterIdentifier = a })

-- | The pending or in-progress change of the cluster type.
pmvClusterType :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterType = lens _pmvClusterType (\s a -> s { _pmvClusterType = a })

-- | The pending or in-progress change of the service version.
pmvClusterVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvClusterVersion =
    lens _pmvClusterVersion (\s a -> s { _pmvClusterVersion = a })

-- | The pending or in-progress change of the master user password for the
-- cluster.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword =
    lens _pmvMasterUserPassword (\s a -> s { _pmvMasterUserPassword = a })

-- | The pending or in-progress change of the cluster's node type.
pmvNodeType :: Lens' PendingModifiedValues (Maybe Text)
pmvNodeType = lens _pmvNodeType (\s a -> s { _pmvNodeType = a })

-- | The pending or in-progress change of the number of nodes in the cluster.
pmvNumberOfNodes :: Lens' PendingModifiedValues (Maybe Int)
pmvNumberOfNodes = lens _pmvNumberOfNodes (\s a -> s { _pmvNumberOfNodes = a })

instance FromXML PendingModifiedValues where
    parseXML x = PendingModifiedValues
        <$> x .@? "AutomatedSnapshotRetentionPeriod"
        <*> x .@? "ClusterIdentifier"
        <*> x .@? "ClusterType"
        <*> x .@? "ClusterVersion"
        <*> x .@? "MasterUserPassword"
        <*> x .@? "NodeType"
        <*> x .@? "NumberOfNodes"

instance ToQuery PendingModifiedValues where
    toQuery PendingModifiedValues{..} = mconcat
        [ "AutomatedSnapshotRetentionPeriod" =? _pmvAutomatedSnapshotRetentionPeriod
        , "ClusterIdentifier"                =? _pmvClusterIdentifier
        , "ClusterType"                      =? _pmvClusterType
        , "ClusterVersion"                   =? _pmvClusterVersion
        , "MasterUserPassword"               =? _pmvMasterUserPassword
        , "NodeType"                         =? _pmvNodeType
        , "NumberOfNodes"                    =? _pmvNumberOfNodes
        ]

data VpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmStatus             :: Maybe Text
    , _vsgmVpcSecurityGroupId :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'VpcSecurityGroupMembership' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vsgmStatus' @::@ 'Maybe' 'Text'
--
-- * 'vsgmVpcSecurityGroupId' @::@ 'Maybe' 'Text'
--
vpcSecurityGroupMembership :: VpcSecurityGroupMembership
vpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmVpcSecurityGroupId = Nothing
    , _vsgmStatus             = Nothing
    }

vsgmStatus :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\s a -> s { _vsgmStatus = a })

vsgmVpcSecurityGroupId :: Lens' VpcSecurityGroupMembership (Maybe Text)
vsgmVpcSecurityGroupId =
    lens _vsgmVpcSecurityGroupId (\s a -> s { _vsgmVpcSecurityGroupId = a })

instance FromXML VpcSecurityGroupMembership where
    parseXML x = VpcSecurityGroupMembership
        <$> x .@? "Status"
        <*> x .@? "VpcSecurityGroupId"

instance ToQuery VpcSecurityGroupMembership where
    toQuery VpcSecurityGroupMembership{..} = mconcat
        [ "Status"             =? _vsgmStatus
        , "VpcSecurityGroupId" =? _vsgmVpcSecurityGroupId
        ]

data Parameter = Parameter
    { _pAllowedValues        :: Maybe Text
    , _pDataType             :: Maybe Text
    , _pDescription          :: Maybe Text
    , _pIsModifiable         :: Maybe Bool
    , _pMinimumEngineVersion :: Maybe Text
    , _pParameterName        :: Maybe Text
    , _pParameterValue       :: Maybe Text
    , _pSource               :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Parameter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pAllowedValues' @::@ 'Maybe' 'Text'
--
-- * 'pDataType' @::@ 'Maybe' 'Text'
--
-- * 'pDescription' @::@ 'Maybe' 'Text'
--
-- * 'pIsModifiable' @::@ 'Maybe' 'Bool'
--
-- * 'pMinimumEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'pParameterName' @::@ 'Maybe' 'Text'
--
-- * 'pParameterValue' @::@ 'Maybe' 'Text'
--
-- * 'pSource' @::@ 'Maybe' 'Text'
--
parameter :: Parameter
parameter = Parameter
    { _pParameterName        = Nothing
    , _pParameterValue       = Nothing
    , _pDescription          = Nothing
    , _pSource               = Nothing
    , _pDataType             = Nothing
    , _pAllowedValues        = Nothing
    , _pIsModifiable         = Nothing
    , _pMinimumEngineVersion = Nothing
    }

-- | The valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\s a -> s { _pAllowedValues = a })

-- | The data type of the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\s a -> s { _pDataType = a })

-- | A description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\s a -> s { _pDescription = a })

-- | If 'true', the parameter can be modified. Some parameters have security or
-- operational implications that prevent them from being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\s a -> s { _pIsModifiable = a })

-- | The earliest engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion =
    lens _pMinimumEngineVersion (\s a -> s { _pMinimumEngineVersion = a })

-- | The name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\s a -> s { _pParameterName = a })

-- | The value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\s a -> s { _pParameterValue = a })

-- | The source of the parameter value, such as "engine-default" or "user".
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\s a -> s { _pSource = a })

instance FromXML Parameter where
    parseXML x = Parameter
        <$> x .@? "AllowedValues"
        <*> x .@? "DataType"
        <*> x .@? "Description"
        <*> x .@? "IsModifiable"
        <*> x .@? "MinimumEngineVersion"
        <*> x .@? "ParameterName"
        <*> x .@? "ParameterValue"
        <*> x .@? "Source"

instance ToQuery Parameter where
    toQuery Parameter{..} = mconcat
        [ "AllowedValues"        =? _pAllowedValues
        , "DataType"             =? _pDataType
        , "Description"          =? _pDescription
        , "IsModifiable"         =? _pIsModifiable
        , "MinimumEngineVersion" =? _pMinimumEngineVersion
        , "ParameterName"        =? _pParameterName
        , "ParameterValue"       =? _pParameterValue
        , "Source"               =? _pSource
        ]
