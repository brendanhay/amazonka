{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMV2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMV2.Lens
  ( -- * Operations

    -- ** DescribeClusters
    describeClusters_filters,
    describeClusters_nextToken,
    describeClusters_maxResults,
    describeClustersResponse_nextToken,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,

    -- ** DeleteBackup
    deleteBackup_backupId,
    deleteBackupResponse_backup,
    deleteBackupResponse_httpStatus,

    -- ** InitializeCluster
    initializeCluster_clusterId,
    initializeCluster_signedCert,
    initializeCluster_trustAnchor,
    initializeClusterResponse_stateMessage,
    initializeClusterResponse_state,
    initializeClusterResponse_httpStatus,

    -- ** CreateHsm
    createHsm_ipAddress,
    createHsm_clusterId,
    createHsm_availabilityZone,
    createHsmResponse_hsm,
    createHsmResponse_httpStatus,

    -- ** DescribeBackups
    describeBackups_sortAscending,
    describeBackups_filters,
    describeBackups_nextToken,
    describeBackups_maxResults,
    describeBackupsResponse_backups,
    describeBackupsResponse_nextToken,
    describeBackupsResponse_httpStatus,

    -- ** CopyBackupToRegion
    copyBackupToRegion_tagList,
    copyBackupToRegion_destinationRegion,
    copyBackupToRegion_backupId,
    copyBackupToRegionResponse_destinationBackup,
    copyBackupToRegionResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_clusterId,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** CreateCluster
    createCluster_backupRetentionPolicy,
    createCluster_tagList,
    createCluster_sourceBackupId,
    createCluster_hsmType,
    createCluster_subnetIds,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** RestoreBackup
    restoreBackup_backupId,
    restoreBackupResponse_backup,
    restoreBackupResponse_httpStatus,

    -- ** DeleteHsm
    deleteHsm_eniId,
    deleteHsm_hsmId,
    deleteHsm_eniIp,
    deleteHsm_clusterId,
    deleteHsmResponse_hsmId,
    deleteHsmResponse_httpStatus,

    -- ** ModifyCluster
    modifyCluster_backupRetentionPolicy,
    modifyCluster_clusterId,
    modifyClusterResponse_cluster,
    modifyClusterResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceId,
    tagResource_tagList,
    tagResourceResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceId,
    listTagsResponse_nextToken,
    listTagsResponse_httpStatus,
    listTagsResponse_tagList,

    -- ** UntagResource
    untagResource_resourceId,
    untagResource_tagKeyList,
    untagResourceResponse_httpStatus,

    -- ** ModifyBackupAttributes
    modifyBackupAttributes_backupId,
    modifyBackupAttributes_neverExpires,
    modifyBackupAttributesResponse_backup,
    modifyBackupAttributesResponse_httpStatus,

    -- * Types

    -- ** Backup
    backup_deleteTimestamp,
    backup_sourceCluster,
    backup_neverExpires,
    backup_sourceRegion,
    backup_tagList,
    backup_sourceBackup,
    backup_clusterId,
    backup_createTimestamp,
    backup_copyTimestamp,
    backup_backupState,
    backup_backupId,

    -- ** BackupRetentionPolicy
    backupRetentionPolicy_value,
    backupRetentionPolicy_type,

    -- ** Certificates
    certificates_manufacturerHardwareCertificate,
    certificates_clusterCsr,
    certificates_hsmCertificate,
    certificates_clusterCertificate,
    certificates_awsHardwareCertificate,

    -- ** Cluster
    cluster_preCoPassword,
    cluster_stateMessage,
    cluster_state,
    cluster_subnetMapping,
    cluster_backupRetentionPolicy,
    cluster_hsms,
    cluster_vpcId,
    cluster_tagList,
    cluster_sourceBackupId,
    cluster_certificates,
    cluster_securityGroup,
    cluster_clusterId,
    cluster_createTimestamp,
    cluster_backupPolicy,
    cluster_hsmType,

    -- ** DestinationBackup
    destinationBackup_sourceCluster,
    destinationBackup_sourceRegion,
    destinationBackup_sourceBackup,
    destinationBackup_createTimestamp,

    -- ** Hsm
    hsm_stateMessage,
    hsm_state,
    hsm_eniId,
    hsm_subnetId,
    hsm_availabilityZone,
    hsm_clusterId,
    hsm_eniIp,
    hsm_hsmId,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.CloudHSMV2.CopyBackupToRegion
import Network.AWS.CloudHSMV2.CreateCluster
import Network.AWS.CloudHSMV2.CreateHsm
import Network.AWS.CloudHSMV2.DeleteBackup
import Network.AWS.CloudHSMV2.DeleteCluster
import Network.AWS.CloudHSMV2.DeleteHsm
import Network.AWS.CloudHSMV2.DescribeBackups
import Network.AWS.CloudHSMV2.DescribeClusters
import Network.AWS.CloudHSMV2.InitializeCluster
import Network.AWS.CloudHSMV2.ListTags
import Network.AWS.CloudHSMV2.ModifyBackupAttributes
import Network.AWS.CloudHSMV2.ModifyCluster
import Network.AWS.CloudHSMV2.RestoreBackup
import Network.AWS.CloudHSMV2.TagResource
import Network.AWS.CloudHSMV2.Types.Backup
import Network.AWS.CloudHSMV2.Types.BackupRetentionPolicy
import Network.AWS.CloudHSMV2.Types.Certificates
import Network.AWS.CloudHSMV2.Types.Cluster
import Network.AWS.CloudHSMV2.Types.DestinationBackup
import Network.AWS.CloudHSMV2.Types.Hsm
import Network.AWS.CloudHSMV2.Types.Tag
import Network.AWS.CloudHSMV2.UntagResource
