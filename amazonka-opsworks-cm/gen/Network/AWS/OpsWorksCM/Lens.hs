{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Lens
  ( -- * Operations

    -- ** DeleteBackup
    deleteBackup_backupId,
    deleteBackupResponse_httpStatus,

    -- ** UpdateServer
    updateServer_preferredBackupWindow,
    updateServer_disableAutomatedBackup,
    updateServer_preferredMaintenanceWindow,
    updateServer_backupRetentionCount,
    updateServer_serverName,
    updateServerResponse_server,
    updateServerResponse_httpStatus,

    -- ** DeleteServer
    deleteServer_serverName,
    deleteServerResponse_httpStatus,

    -- ** CreateServer
    createServer_securityGroupIds,
    createServer_preferredBackupWindow,
    createServer_disableAutomatedBackup,
    createServer_customPrivateKey,
    createServer_engineAttributes,
    createServer_customDomain,
    createServer_backupId,
    createServer_subnetIds,
    createServer_keyPair,
    createServer_associatePublicIpAddress,
    createServer_engineVersion,
    createServer_preferredMaintenanceWindow,
    createServer_tags,
    createServer_backupRetentionCount,
    createServer_engineModel,
    createServer_customCertificate,
    createServer_engine,
    createServer_serverName,
    createServer_instanceProfileArn,
    createServer_instanceType,
    createServer_serviceRoleArn,
    createServerResponse_server,
    createServerResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributesResponse_attributes,
    describeAccountAttributesResponse_httpStatus,

    -- ** ExportServerEngineAttribute
    exportServerEngineAttribute_inputAttributes,
    exportServerEngineAttribute_exportAttributeName,
    exportServerEngineAttribute_serverName,
    exportServerEngineAttributeResponse_engineAttribute,
    exportServerEngineAttributeResponse_serverName,
    exportServerEngineAttributeResponse_httpStatus,

    -- ** DescribeServers
    describeServers_nextToken,
    describeServers_maxResults,
    describeServers_serverName,
    describeServersResponse_nextToken,
    describeServersResponse_servers,
    describeServersResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeNodeAssociationStatus
    describeNodeAssociationStatus_nodeAssociationStatusToken,
    describeNodeAssociationStatus_serverName,
    describeNodeAssociationStatusResponse_engineAttributes,
    describeNodeAssociationStatusResponse_httpStatus,
    describeNodeAssociationStatusResponse_nodeAssociationStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DisassociateNode
    disassociateNode_engineAttributes,
    disassociateNode_serverName,
    disassociateNode_nodeName,
    disassociateNodeResponse_nodeAssociationStatusToken,
    disassociateNodeResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_nextToken,
    describeEvents_maxResults,
    describeEvents_serverName,
    describeEventsResponse_nextToken,
    describeEventsResponse_serverEvents,
    describeEventsResponse_httpStatus,

    -- ** CreateBackup
    createBackup_tags,
    createBackup_description,
    createBackup_serverName,
    createBackupResponse_backup,
    createBackupResponse_httpStatus,

    -- ** AssociateNode
    associateNode_serverName,
    associateNode_nodeName,
    associateNode_engineAttributes,
    associateNodeResponse_nodeAssociationStatusToken,
    associateNodeResponse_httpStatus,

    -- ** DescribeBackups
    describeBackups_nextToken,
    describeBackups_maxResults,
    describeBackups_backupId,
    describeBackups_serverName,
    describeBackupsResponse_nextToken,
    describeBackupsResponse_backups,
    describeBackupsResponse_httpStatus,

    -- ** UpdateServerEngineAttributes
    updateServerEngineAttributes_attributeValue,
    updateServerEngineAttributes_serverName,
    updateServerEngineAttributes_attributeName,
    updateServerEngineAttributesResponse_server,
    updateServerEngineAttributesResponse_httpStatus,

    -- ** StartMaintenance
    startMaintenance_engineAttributes,
    startMaintenance_serverName,
    startMaintenanceResponse_server,
    startMaintenanceResponse_httpStatus,

    -- ** RestoreServer
    restoreServer_instanceType,
    restoreServer_keyPair,
    restoreServer_backupId,
    restoreServer_serverName,
    restoreServerResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** AccountAttribute
    accountAttribute_used,
    accountAttribute_name,
    accountAttribute_maximum,

    -- ** Backup
    backup_securityGroupIds,
    backup_instanceProfileArn,
    backup_preferredBackupWindow,
    backup_status,
    backup_serviceRoleArn,
    backup_userArn,
    backup_instanceType,
    backup_backupType,
    backup_backupId,
    backup_s3DataUrl,
    backup_backupArn,
    backup_subnetIds,
    backup_keyPair,
    backup_s3DataSize,
    backup_createdAt,
    backup_serverName,
    backup_s3LogUrl,
    backup_engineVersion,
    backup_preferredMaintenanceWindow,
    backup_toolsVersion,
    backup_engineModel,
    backup_engine,
    backup_description,
    backup_statusDescription,

    -- ** EngineAttribute
    engineAttribute_name,
    engineAttribute_value,

    -- ** Server
    server_securityGroupIds,
    server_instanceProfileArn,
    server_preferredBackupWindow,
    server_status,
    server_disableAutomatedBackup,
    server_serviceRoleArn,
    server_instanceType,
    server_engineAttributes,
    server_customDomain,
    server_subnetIds,
    server_keyPair,
    server_createdAt,
    server_serverName,
    server_associatePublicIpAddress,
    server_engineVersion,
    server_preferredMaintenanceWindow,
    server_backupRetentionCount,
    server_maintenanceStatus,
    server_cloudFormationStackArn,
    server_engineModel,
    server_engine,
    server_endpoint,
    server_serverArn,
    server_statusReason,

    -- ** ServerEvent
    serverEvent_logUrl,
    serverEvent_message,
    serverEvent_createdAt,
    serverEvent_serverName,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.OpsWorksCM.AssociateNode
import Network.AWS.OpsWorksCM.CreateBackup
import Network.AWS.OpsWorksCM.CreateServer
import Network.AWS.OpsWorksCM.DeleteBackup
import Network.AWS.OpsWorksCM.DeleteServer
import Network.AWS.OpsWorksCM.DescribeAccountAttributes
import Network.AWS.OpsWorksCM.DescribeBackups
import Network.AWS.OpsWorksCM.DescribeEvents
import Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus
import Network.AWS.OpsWorksCM.DescribeServers
import Network.AWS.OpsWorksCM.DisassociateNode
import Network.AWS.OpsWorksCM.ExportServerEngineAttribute
import Network.AWS.OpsWorksCM.ListTagsForResource
import Network.AWS.OpsWorksCM.RestoreServer
import Network.AWS.OpsWorksCM.StartMaintenance
import Network.AWS.OpsWorksCM.TagResource
import Network.AWS.OpsWorksCM.Types.AccountAttribute
import Network.AWS.OpsWorksCM.Types.Backup
import Network.AWS.OpsWorksCM.Types.EngineAttribute
import Network.AWS.OpsWorksCM.Types.Server
import Network.AWS.OpsWorksCM.Types.ServerEvent
import Network.AWS.OpsWorksCM.Types.Tag
import Network.AWS.OpsWorksCM.UntagResource
import Network.AWS.OpsWorksCM.UpdateServer
import Network.AWS.OpsWorksCM.UpdateServerEngineAttributes
