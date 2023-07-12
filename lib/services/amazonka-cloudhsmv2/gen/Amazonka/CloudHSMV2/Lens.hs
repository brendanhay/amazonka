{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudHSMV2.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudHSMV2.Lens
  ( -- * Operations

    -- ** CopyBackupToRegion
    copyBackupToRegion_tagList,
    copyBackupToRegion_destinationRegion,
    copyBackupToRegion_backupId,
    copyBackupToRegionResponse_destinationBackup,
    copyBackupToRegionResponse_httpStatus,

    -- ** CreateCluster
    createCluster_backupRetentionPolicy,
    createCluster_sourceBackupId,
    createCluster_tagList,
    createCluster_hsmType,
    createCluster_subnetIds,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CreateHsm
    createHsm_ipAddress,
    createHsm_clusterId,
    createHsm_availabilityZone,
    createHsmResponse_hsm,
    createHsmResponse_httpStatus,

    -- ** DeleteBackup
    deleteBackup_backupId,
    deleteBackupResponse_backup,
    deleteBackupResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_clusterId,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** DeleteHsm
    deleteHsm_eniId,
    deleteHsm_eniIp,
    deleteHsm_hsmId,
    deleteHsm_clusterId,
    deleteHsmResponse_hsmId,
    deleteHsmResponse_httpStatus,

    -- ** DescribeBackups
    describeBackups_filters,
    describeBackups_maxResults,
    describeBackups_nextToken,
    describeBackups_sortAscending,
    describeBackupsResponse_backups,
    describeBackupsResponse_nextToken,
    describeBackupsResponse_httpStatus,

    -- ** DescribeClusters
    describeClusters_filters,
    describeClusters_maxResults,
    describeClusters_nextToken,
    describeClustersResponse_clusters,
    describeClustersResponse_nextToken,
    describeClustersResponse_httpStatus,

    -- ** InitializeCluster
    initializeCluster_clusterId,
    initializeCluster_signedCert,
    initializeCluster_trustAnchor,
    initializeClusterResponse_state,
    initializeClusterResponse_stateMessage,
    initializeClusterResponse_httpStatus,

    -- ** ListTags
    listTags_maxResults,
    listTags_nextToken,
    listTags_resourceId,
    listTagsResponse_nextToken,
    listTagsResponse_httpStatus,
    listTagsResponse_tagList,

    -- ** ModifyBackupAttributes
    modifyBackupAttributes_backupId,
    modifyBackupAttributes_neverExpires,
    modifyBackupAttributesResponse_backup,
    modifyBackupAttributesResponse_httpStatus,

    -- ** ModifyCluster
    modifyCluster_backupRetentionPolicy,
    modifyCluster_clusterId,
    modifyClusterResponse_cluster,
    modifyClusterResponse_httpStatus,

    -- ** RestoreBackup
    restoreBackup_backupId,
    restoreBackupResponse_backup,
    restoreBackupResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceId,
    tagResource_tagList,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceId,
    untagResource_tagKeyList,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** Backup
    backup_backupState,
    backup_clusterId,
    backup_copyTimestamp,
    backup_createTimestamp,
    backup_deleteTimestamp,
    backup_neverExpires,
    backup_sourceBackup,
    backup_sourceCluster,
    backup_sourceRegion,
    backup_tagList,
    backup_backupId,

    -- ** BackupRetentionPolicy
    backupRetentionPolicy_type,
    backupRetentionPolicy_value,

    -- ** Certificates
    certificates_awsHardwareCertificate,
    certificates_clusterCertificate,
    certificates_clusterCsr,
    certificates_hsmCertificate,
    certificates_manufacturerHardwareCertificate,

    -- ** Cluster
    cluster_backupPolicy,
    cluster_backupRetentionPolicy,
    cluster_certificates,
    cluster_clusterId,
    cluster_createTimestamp,
    cluster_hsmType,
    cluster_hsms,
    cluster_preCoPassword,
    cluster_securityGroup,
    cluster_sourceBackupId,
    cluster_state,
    cluster_stateMessage,
    cluster_subnetMapping,
    cluster_tagList,
    cluster_vpcId,

    -- ** DestinationBackup
    destinationBackup_createTimestamp,
    destinationBackup_sourceBackup,
    destinationBackup_sourceCluster,
    destinationBackup_sourceRegion,

    -- ** Hsm
    hsm_availabilityZone,
    hsm_clusterId,
    hsm_eniId,
    hsm_eniIp,
    hsm_state,
    hsm_stateMessage,
    hsm_subnetId,
    hsm_hsmId,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.CloudHSMV2.CopyBackupToRegion
import Amazonka.CloudHSMV2.CreateCluster
import Amazonka.CloudHSMV2.CreateHsm
import Amazonka.CloudHSMV2.DeleteBackup
import Amazonka.CloudHSMV2.DeleteCluster
import Amazonka.CloudHSMV2.DeleteHsm
import Amazonka.CloudHSMV2.DescribeBackups
import Amazonka.CloudHSMV2.DescribeClusters
import Amazonka.CloudHSMV2.InitializeCluster
import Amazonka.CloudHSMV2.ListTags
import Amazonka.CloudHSMV2.ModifyBackupAttributes
import Amazonka.CloudHSMV2.ModifyCluster
import Amazonka.CloudHSMV2.RestoreBackup
import Amazonka.CloudHSMV2.TagResource
import Amazonka.CloudHSMV2.Types.Backup
import Amazonka.CloudHSMV2.Types.BackupRetentionPolicy
import Amazonka.CloudHSMV2.Types.Certificates
import Amazonka.CloudHSMV2.Types.Cluster
import Amazonka.CloudHSMV2.Types.DestinationBackup
import Amazonka.CloudHSMV2.Types.Hsm
import Amazonka.CloudHSMV2.Types.Tag
import Amazonka.CloudHSMV2.UntagResource
