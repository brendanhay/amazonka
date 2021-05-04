{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Lens
  ( -- * Operations

    -- ** DeleteHsm
    deleteHsm_eniIp,
    deleteHsm_eniId,
    deleteHsm_hsmId,
    deleteHsm_clusterId,
    deleteHsmResponse_hsmId,
    deleteHsmResponse_httpStatus,

    -- ** DeleteBackup
    deleteBackup_backupId,
    deleteBackupResponse_backup,
    deleteBackupResponse_httpStatus,

    -- ** DescribeClusters
    describeClusters_nextToken,
    describeClusters_maxResults,
    describeClusters_filters,
    describeClustersResponse_nextToken,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,

    -- ** RestoreBackup
    restoreBackup_backupId,
    restoreBackupResponse_backup,
    restoreBackupResponse_httpStatus,

    -- ** CreateCluster
    createCluster_sourceBackupId,
    createCluster_tagList,
    createCluster_backupRetentionPolicy,
    createCluster_hsmType,
    createCluster_subnetIds,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceId,
    untagResource_tagKeyList,
    untagResourceResponse_httpStatus,

    -- ** CopyBackupToRegion
    copyBackupToRegion_tagList,
    copyBackupToRegion_destinationRegion,
    copyBackupToRegion_backupId,
    copyBackupToRegionResponse_destinationBackup,
    copyBackupToRegionResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceId,
    tagResource_tagList,
    tagResourceResponse_httpStatus,

    -- ** ModifyCluster
    modifyCluster_backupRetentionPolicy,
    modifyCluster_clusterId,
    modifyClusterResponse_cluster,
    modifyClusterResponse_httpStatus,

    -- ** ModifyBackupAttributes
    modifyBackupAttributes_backupId,
    modifyBackupAttributes_neverExpires,
    modifyBackupAttributesResponse_backup,
    modifyBackupAttributesResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_clusterId,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceId,
    listTagsResponse_nextToken,
    listTagsResponse_httpStatus,
    listTagsResponse_tagList,

    -- ** DescribeBackups
    describeBackups_nextToken,
    describeBackups_maxResults,
    describeBackups_sortAscending,
    describeBackups_filters,
    describeBackupsResponse_nextToken,
    describeBackupsResponse_backups,
    describeBackupsResponse_httpStatus,

    -- ** CreateHsm
    createHsm_ipAddress,
    createHsm_clusterId,
    createHsm_availabilityZone,
    createHsmResponse_hsm,
    createHsmResponse_httpStatus,

    -- ** InitializeCluster
    initializeCluster_clusterId,
    initializeCluster_signedCert,
    initializeCluster_trustAnchor,
    initializeClusterResponse_stateMessage,
    initializeClusterResponse_state,
    initializeClusterResponse_httpStatus,

    -- * Types

    -- ** Backup
    backup_clusterId,
    backup_backupState,
    backup_sourceBackup,
    backup_copyTimestamp,
    backup_createTimestamp,
    backup_neverExpires,
    backup_sourceCluster,
    backup_deleteTimestamp,
    backup_tagList,
    backup_sourceRegion,
    backup_backupId,

    -- ** BackupRetentionPolicy
    backupRetentionPolicy_value,
    backupRetentionPolicy_type,

    -- ** Certificates
    certificates_awsHardwareCertificate,
    certificates_hsmCertificate,
    certificates_clusterCsr,
    certificates_clusterCertificate,
    certificates_manufacturerHardwareCertificate,

    -- ** Cluster
    cluster_clusterId,
    cluster_stateMessage,
    cluster_backupPolicy,
    cluster_createTimestamp,
    cluster_subnetMapping,
    cluster_state,
    cluster_preCoPassword,
    cluster_securityGroup,
    cluster_hsmType,
    cluster_sourceBackupId,
    cluster_certificates,
    cluster_tagList,
    cluster_vpcId,
    cluster_hsms,
    cluster_backupRetentionPolicy,

    -- ** DestinationBackup
    destinationBackup_sourceBackup,
    destinationBackup_createTimestamp,
    destinationBackup_sourceCluster,
    destinationBackup_sourceRegion,

    -- ** Hsm
    hsm_clusterId,
    hsm_stateMessage,
    hsm_eniIp,
    hsm_eniId,
    hsm_state,
    hsm_availabilityZone,
    hsm_subnetId,
    hsm_hsmId,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.CloudHSMv2.CopyBackupToRegion
import Network.AWS.CloudHSMv2.CreateCluster
import Network.AWS.CloudHSMv2.CreateHsm
import Network.AWS.CloudHSMv2.DeleteBackup
import Network.AWS.CloudHSMv2.DeleteCluster
import Network.AWS.CloudHSMv2.DeleteHsm
import Network.AWS.CloudHSMv2.DescribeBackups
import Network.AWS.CloudHSMv2.DescribeClusters
import Network.AWS.CloudHSMv2.InitializeCluster
import Network.AWS.CloudHSMv2.ListTags
import Network.AWS.CloudHSMv2.ModifyBackupAttributes
import Network.AWS.CloudHSMv2.ModifyCluster
import Network.AWS.CloudHSMv2.RestoreBackup
import Network.AWS.CloudHSMv2.TagResource
import Network.AWS.CloudHSMv2.Types.Backup
import Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy
import Network.AWS.CloudHSMv2.Types.Certificates
import Network.AWS.CloudHSMv2.Types.Cluster
import Network.AWS.CloudHSMv2.Types.DestinationBackup
import Network.AWS.CloudHSMv2.Types.Hsm
import Network.AWS.CloudHSMv2.Types.Tag
import Network.AWS.CloudHSMv2.UntagResource
