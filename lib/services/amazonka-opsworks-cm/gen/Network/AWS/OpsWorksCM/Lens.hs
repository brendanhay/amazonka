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

    -- ** AssociateNode
    associateNode_serverName,
    associateNode_nodeName,
    associateNode_engineAttributes,
    associateNodeResponse_nodeAssociationStatusToken,
    associateNodeResponse_httpStatus,

    -- ** UpdateServer
    updateServer_disableAutomatedBackup,
    updateServer_preferredMaintenanceWindow,
    updateServer_preferredBackupWindow,
    updateServer_backupRetentionCount,
    updateServer_serverName,
    updateServerResponse_server,
    updateServerResponse_httpStatus,

    -- ** DeleteServer
    deleteServer_serverName,
    deleteServerResponse_httpStatus,

    -- ** DeleteBackup
    deleteBackup_backupId,
    deleteBackupResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_nextToken,
    describeEvents_maxResults,
    describeEvents_serverName,
    describeEventsResponse_serverEvents,
    describeEventsResponse_nextToken,
    describeEventsResponse_httpStatus,

    -- ** DisassociateNode
    disassociateNode_engineAttributes,
    disassociateNode_serverName,
    disassociateNode_nodeName,
    disassociateNodeResponse_nodeAssociationStatusToken,
    disassociateNodeResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateBackup
    createBackup_description,
    createBackup_tags,
    createBackup_serverName,
    createBackupResponse_backup,
    createBackupResponse_httpStatus,

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

    -- ** DescribeBackups
    describeBackups_serverName,
    describeBackups_backupId,
    describeBackups_nextToken,
    describeBackups_maxResults,
    describeBackupsResponse_backups,
    describeBackupsResponse_nextToken,
    describeBackupsResponse_httpStatus,

    -- ** CreateServer
    createServer_engineVersion,
    createServer_disableAutomatedBackup,
    createServer_securityGroupIds,
    createServer_associatePublicIpAddress,
    createServer_subnetIds,
    createServer_keyPair,
    createServer_backupId,
    createServer_customDomain,
    createServer_customPrivateKey,
    createServer_engineModel,
    createServer_engineAttributes,
    createServer_preferredMaintenanceWindow,
    createServer_preferredBackupWindow,
    createServer_customCertificate,
    createServer_tags,
    createServer_backupRetentionCount,
    createServer_engine,
    createServer_serverName,
    createServer_instanceProfileArn,
    createServer_instanceType,
    createServer_serviceRoleArn,
    createServerResponse_server,
    createServerResponse_httpStatus,

    -- ** RestoreServer
    restoreServer_keyPair,
    restoreServer_instanceType,
    restoreServer_backupId,
    restoreServer_serverName,
    restoreServerResponse_server,
    restoreServerResponse_httpStatus,

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

    -- ** DescribeAccountAttributes
    describeAccountAttributesResponse_attributes,
    describeAccountAttributesResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeServers
    describeServers_serverName,
    describeServers_nextToken,
    describeServers_maxResults,
    describeServersResponse_servers,
    describeServersResponse_nextToken,
    describeServersResponse_httpStatus,

    -- ** ExportServerEngineAttribute
    exportServerEngineAttribute_inputAttributes,
    exportServerEngineAttribute_exportAttributeName,
    exportServerEngineAttribute_serverName,
    exportServerEngineAttributeResponse_serverName,
    exportServerEngineAttributeResponse_engineAttribute,
    exportServerEngineAttributeResponse_httpStatus,

    -- * Types

    -- ** AccountAttribute
    accountAttribute_used,
    accountAttribute_maximum,
    accountAttribute_name,

    -- ** Backup
    backup_engineVersion,
    backup_serviceRoleArn,
    backup_status,
    backup_instanceProfileArn,
    backup_securityGroupIds,
    backup_statusDescription,
    backup_serverName,
    backup_subnetIds,
    backup_keyPair,
    backup_createdAt,
    backup_backupId,
    backup_engine,
    backup_instanceType,
    backup_engineModel,
    backup_preferredMaintenanceWindow,
    backup_userArn,
    backup_preferredBackupWindow,
    backup_s3LogUrl,
    backup_s3DataSize,
    backup_backupArn,
    backup_s3DataUrl,
    backup_description,
    backup_backupType,
    backup_toolsVersion,

    -- ** EngineAttribute
    engineAttribute_value,
    engineAttribute_name,

    -- ** Server
    server_engineVersion,
    server_serviceRoleArn,
    server_disableAutomatedBackup,
    server_status,
    server_instanceProfileArn,
    server_securityGroupIds,
    server_associatePublicIpAddress,
    server_serverName,
    server_subnetIds,
    server_keyPair,
    server_createdAt,
    server_serverArn,
    server_customDomain,
    server_engine,
    server_maintenanceStatus,
    server_instanceType,
    server_engineModel,
    server_engineAttributes,
    server_preferredMaintenanceWindow,
    server_preferredBackupWindow,
    server_statusReason,
    server_endpoint,
    server_cloudFormationStackArn,
    server_backupRetentionCount,

    -- ** ServerEvent
    serverEvent_logUrl,
    serverEvent_serverName,
    serverEvent_createdAt,
    serverEvent_message,

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
