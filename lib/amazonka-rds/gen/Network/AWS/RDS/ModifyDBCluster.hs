{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify a setting for an Amazon Aurora DB cluster. You can change one or more database configuration parameters by specifying these parameters and the new values in the request. For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.ModifyDBCluster
  ( -- * Creating a Request
    modifyDBCluster,
    ModifyDBCluster,

    -- * Request Lenses
    mdcEngineVersion,
    mdcEnableGlobalWriteForwarding,
    mdcDeletionProtection,
    mdcMasterUserPassword,
    mdcEnableHTTPEndpoint,
    mdcAllowMajorVersionUpgrade,
    mdcDomain,
    mdcBacktrackWindow,
    mdcCloudwatchLogsExportConfiguration,
    mdcPreferredMaintenanceWindow,
    mdcPreferredBackupWindow,
    mdcBackupRetentionPeriod,
    mdcVPCSecurityGroupIds,
    mdcDBClusterParameterGroupName,
    mdcScalingConfiguration,
    mdcApplyImmediately,
    mdcOptionGroupName,
    mdcCopyTagsToSnapshot,
    mdcNewDBClusterIdentifier,
    mdcDBInstanceParameterGroupName,
    mdcDomainIAMRoleName,
    mdcPort,
    mdcEnableIAMDatabaseAuthentication,
    mdcDBClusterIdentifier,

    -- * Destructuring the Response
    modifyDBClusterResponse,
    ModifyDBClusterResponse,

    -- * Response Lenses
    mdcrsDBCluster,
    mdcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'modifyDBCluster' smart constructor.
data ModifyDBCluster = ModifyDBCluster'
  { _mdcEngineVersion ::
      !(Maybe Text),
    _mdcEnableGlobalWriteForwarding :: !(Maybe Bool),
    _mdcDeletionProtection :: !(Maybe Bool),
    _mdcMasterUserPassword :: !(Maybe Text),
    _mdcEnableHTTPEndpoint :: !(Maybe Bool),
    _mdcAllowMajorVersionUpgrade :: !(Maybe Bool),
    _mdcDomain :: !(Maybe Text),
    _mdcBacktrackWindow :: !(Maybe Integer),
    _mdcCloudwatchLogsExportConfiguration ::
      !(Maybe CloudwatchLogsExportConfiguration),
    _mdcPreferredMaintenanceWindow :: !(Maybe Text),
    _mdcPreferredBackupWindow :: !(Maybe Text),
    _mdcBackupRetentionPeriod :: !(Maybe Int),
    _mdcVPCSecurityGroupIds :: !(Maybe [Text]),
    _mdcDBClusterParameterGroupName :: !(Maybe Text),
    _mdcScalingConfiguration :: !(Maybe ScalingConfiguration),
    _mdcApplyImmediately :: !(Maybe Bool),
    _mdcOptionGroupName :: !(Maybe Text),
    _mdcCopyTagsToSnapshot :: !(Maybe Bool),
    _mdcNewDBClusterIdentifier :: !(Maybe Text),
    _mdcDBInstanceParameterGroupName :: !(Maybe Text),
    _mdcDomainIAMRoleName :: !(Maybe Text),
    _mdcPort :: !(Maybe Int),
    _mdcEnableIAMDatabaseAuthentication :: !(Maybe Bool),
    _mdcDBClusterIdentifier :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyDBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdcEngineVersion' - The version number of the database engine to which you want to upgrade. Changing this parameter results in an outage. The change is applied during the next maintenance window unless @ApplyImmediately@ is enabled. To list all of the available engine versions for @aurora@ (for MySQL 5.6-compatible Aurora), use the following command: @aws rds describe-db-engine-versions --engine aurora --query "DBEngineVersions[].EngineVersion"@  To list all of the available engine versions for @aurora-mysql@ (for MySQL 5.7-compatible Aurora), use the following command: @aws rds describe-db-engine-versions --engine aurora-mysql --query "DBEngineVersions[].EngineVersion"@  To list all of the available engine versions for @aurora-postgresql@ , use the following command: @aws rds describe-db-engine-versions --engine aurora-postgresql --query "DBEngineVersions[].EngineVersion"@
--
-- * 'mdcEnableGlobalWriteForwarding' - A value that indicates whether to enable write operations to be forwarded from this cluster to the primary cluster in an Aurora global database. The resulting changes are replicated back to this cluster. This parameter only applies to DB clusters that are secondary clusters in an Aurora global database. By default, Aurora disallows write operations for secondary clusters.
--
-- * 'mdcDeletionProtection' - A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
--
-- * 'mdcMasterUserPassword' - The new password for the master database user. This password can contain any printable ASCII character except "/", """, or "@". Constraints: Must contain from 8 to 41 characters.
--
-- * 'mdcEnableHTTPEndpoint' - A value that indicates whether to enable the HTTP endpoint for an Aurora Serverless DB cluster. By default, the HTTP endpoint is disabled. When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- * 'mdcAllowMajorVersionUpgrade' - A value that indicates whether major version upgrades are allowed. Constraints: You must allow major version upgrades when specifying a value for the @EngineVersion@ parameter that is a different major version than the DB cluster's current version.
--
-- * 'mdcDomain' - The Active Directory directory ID to move the DB cluster to. Specify @none@ to remove the cluster from its current domain. The domain must be created prior to this operation.  For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
--
-- * 'mdcBacktrackWindow' - The target backtrack window, in seconds. To disable backtracking, set this value to 0. Default: 0 Constraints:     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
--
-- * 'mdcCloudwatchLogsExportConfiguration' - The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB cluster.
--
-- * 'mdcPreferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./  Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun. Constraints: Minimum 30-minute window.
--
-- * 'mdcPreferredBackupWindow' - The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter.  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./  Constraints:     * Must be in the format @hh24:mi-hh24:mi@ .     * Must be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
--
-- * 'mdcBackupRetentionPeriod' - The number of days for which automated backups are retained. You must specify a minimum value of 1. Default: 1 Constraints:     * Must be a value from 1 to 35
--
-- * 'mdcVPCSecurityGroupIds' - A list of VPC security groups that the DB cluster will belong to.
--
-- * 'mdcDBClusterParameterGroupName' - The name of the DB cluster parameter group to use for the DB cluster.
--
-- * 'mdcScalingConfiguration' - The scaling properties of the DB cluster. You can only modify scaling properties for DB clusters in @serverless@ DB engine mode.
--
-- * 'mdcApplyImmediately' - A value that indicates whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB cluster. If this parameter is disabled, changes to the DB cluster are applied during the next maintenance window. The @ApplyImmediately@ parameter only affects the @EnableIAMDatabaseAuthentication@ , @MasterUserPassword@ , and @NewDBClusterIdentifier@ values. If the @ApplyImmediately@ parameter is disabled, then changes to the @EnableIAMDatabaseAuthentication@ , @MasterUserPassword@ , and @NewDBClusterIdentifier@ values are applied during the next maintenance window. All other changes are applied immediately, regardless of the value of the @ApplyImmediately@ parameter. By default, this parameter is disabled.
--
-- * 'mdcOptionGroupName' - A value that indicates that the DB cluster should be associated with the specified option group. Changing this parameter doesn't result in an outage except in the following case, and the change is applied during the next maintenance window unless the @ApplyImmediately@ is enabled for this request. If the parameter change results in an option group that enables OEM, this change can cause a brief (sub-second) period during which new connections are rejected but existing connections are not interrupted.  Permanent options can't be removed from an option group. The option group can't be removed from a DB cluster once it is associated with a DB cluster.
--
-- * 'mdcCopyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB cluster to snapshots of the DB cluster. The default is not to copy them.
--
-- * 'mdcNewDBClusterIdentifier' - The new DB cluster identifier for the DB cluster when renaming a DB cluster. This value is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens     * The first character must be a letter     * Can't end with a hyphen or contain two consecutive hyphens Example: @my-cluster2@
--
-- * 'mdcDBInstanceParameterGroupName' - The name of the DB parameter group to apply to all instances of the DB cluster.  Default: The existing name setting Constraints:     * The DB parameter group must be in the same DB parameter group family as this DB cluster.     * The @DBInstanceParameterGroupName@ parameter is only valid in combination with the @AllowMajorVersionUpgrade@ parameter.
--
-- * 'mdcDomainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- * 'mdcPort' - The port number on which the DB cluster accepts connections. Constraints: Value must be @1150-65535@  Default: The same port as the original DB cluster.
--
-- * 'mdcEnableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
--
-- * 'mdcDBClusterIdentifier' - The DB cluster identifier for the cluster being modified. This parameter isn't case-sensitive. Constraints: This identifier must match the identifier of an existing DB cluster.
modifyDBCluster ::
  -- | 'mdcDBClusterIdentifier'
  Text ->
  ModifyDBCluster
modifyDBCluster pDBClusterIdentifier_ =
  ModifyDBCluster'
    { _mdcEngineVersion = Nothing,
      _mdcEnableGlobalWriteForwarding = Nothing,
      _mdcDeletionProtection = Nothing,
      _mdcMasterUserPassword = Nothing,
      _mdcEnableHTTPEndpoint = Nothing,
      _mdcAllowMajorVersionUpgrade = Nothing,
      _mdcDomain = Nothing,
      _mdcBacktrackWindow = Nothing,
      _mdcCloudwatchLogsExportConfiguration = Nothing,
      _mdcPreferredMaintenanceWindow = Nothing,
      _mdcPreferredBackupWindow = Nothing,
      _mdcBackupRetentionPeriod = Nothing,
      _mdcVPCSecurityGroupIds = Nothing,
      _mdcDBClusterParameterGroupName = Nothing,
      _mdcScalingConfiguration = Nothing,
      _mdcApplyImmediately = Nothing,
      _mdcOptionGroupName = Nothing,
      _mdcCopyTagsToSnapshot = Nothing,
      _mdcNewDBClusterIdentifier = Nothing,
      _mdcDBInstanceParameterGroupName = Nothing,
      _mdcDomainIAMRoleName = Nothing,
      _mdcPort = Nothing,
      _mdcEnableIAMDatabaseAuthentication = Nothing,
      _mdcDBClusterIdentifier = pDBClusterIdentifier_
    }

-- | The version number of the database engine to which you want to upgrade. Changing this parameter results in an outage. The change is applied during the next maintenance window unless @ApplyImmediately@ is enabled. To list all of the available engine versions for @aurora@ (for MySQL 5.6-compatible Aurora), use the following command: @aws rds describe-db-engine-versions --engine aurora --query "DBEngineVersions[].EngineVersion"@  To list all of the available engine versions for @aurora-mysql@ (for MySQL 5.7-compatible Aurora), use the following command: @aws rds describe-db-engine-versions --engine aurora-mysql --query "DBEngineVersions[].EngineVersion"@  To list all of the available engine versions for @aurora-postgresql@ , use the following command: @aws rds describe-db-engine-versions --engine aurora-postgresql --query "DBEngineVersions[].EngineVersion"@
mdcEngineVersion :: Lens' ModifyDBCluster (Maybe Text)
mdcEngineVersion = lens _mdcEngineVersion (\s a -> s {_mdcEngineVersion = a})

-- | A value that indicates whether to enable write operations to be forwarded from this cluster to the primary cluster in an Aurora global database. The resulting changes are replicated back to this cluster. This parameter only applies to DB clusters that are secondary clusters in an Aurora global database. By default, Aurora disallows write operations for secondary clusters.
mdcEnableGlobalWriteForwarding :: Lens' ModifyDBCluster (Maybe Bool)
mdcEnableGlobalWriteForwarding = lens _mdcEnableGlobalWriteForwarding (\s a -> s {_mdcEnableGlobalWriteForwarding = a})

-- | A value that indicates whether the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled.
mdcDeletionProtection :: Lens' ModifyDBCluster (Maybe Bool)
mdcDeletionProtection = lens _mdcDeletionProtection (\s a -> s {_mdcDeletionProtection = a})

-- | The new password for the master database user. This password can contain any printable ASCII character except "/", """, or "@". Constraints: Must contain from 8 to 41 characters.
mdcMasterUserPassword :: Lens' ModifyDBCluster (Maybe Text)
mdcMasterUserPassword = lens _mdcMasterUserPassword (\s a -> s {_mdcMasterUserPassword = a})

-- | A value that indicates whether to enable the HTTP endpoint for an Aurora Serverless DB cluster. By default, the HTTP endpoint is disabled. When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
mdcEnableHTTPEndpoint :: Lens' ModifyDBCluster (Maybe Bool)
mdcEnableHTTPEndpoint = lens _mdcEnableHTTPEndpoint (\s a -> s {_mdcEnableHTTPEndpoint = a})

-- | A value that indicates whether major version upgrades are allowed. Constraints: You must allow major version upgrades when specifying a value for the @EngineVersion@ parameter that is a different major version than the DB cluster's current version.
mdcAllowMajorVersionUpgrade :: Lens' ModifyDBCluster (Maybe Bool)
mdcAllowMajorVersionUpgrade = lens _mdcAllowMajorVersionUpgrade (\s a -> s {_mdcAllowMajorVersionUpgrade = a})

-- | The Active Directory directory ID to move the DB cluster to. Specify @none@ to remove the cluster from its current domain. The domain must be created prior to this operation.  For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon Aurora User Guide/ .
mdcDomain :: Lens' ModifyDBCluster (Maybe Text)
mdcDomain = lens _mdcDomain (\s a -> s {_mdcDomain = a})

-- | The target backtrack window, in seconds. To disable backtracking, set this value to 0. Default: 0 Constraints:     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
mdcBacktrackWindow :: Lens' ModifyDBCluster (Maybe Integer)
mdcBacktrackWindow = lens _mdcBacktrackWindow (\s a -> s {_mdcBacktrackWindow = a})

-- | The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB cluster.
mdcCloudwatchLogsExportConfiguration :: Lens' ModifyDBCluster (Maybe CloudwatchLogsExportConfiguration)
mdcCloudwatchLogsExportConfiguration = lens _mdcCloudwatchLogsExportConfiguration (\s a -> s {_mdcCloudwatchLogsExportConfiguration = a})

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./  Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun. Constraints: Minimum 30-minute window.
mdcPreferredMaintenanceWindow :: Lens' ModifyDBCluster (Maybe Text)
mdcPreferredMaintenanceWindow = lens _mdcPreferredMaintenanceWindow (\s a -> s {_mdcPreferredMaintenanceWindow = a})

-- | The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter.  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./  Constraints:     * Must be in the format @hh24:mi-hh24:mi@ .     * Must be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
mdcPreferredBackupWindow :: Lens' ModifyDBCluster (Maybe Text)
mdcPreferredBackupWindow = lens _mdcPreferredBackupWindow (\s a -> s {_mdcPreferredBackupWindow = a})

-- | The number of days for which automated backups are retained. You must specify a minimum value of 1. Default: 1 Constraints:     * Must be a value from 1 to 35
mdcBackupRetentionPeriod :: Lens' ModifyDBCluster (Maybe Int)
mdcBackupRetentionPeriod = lens _mdcBackupRetentionPeriod (\s a -> s {_mdcBackupRetentionPeriod = a})

-- | A list of VPC security groups that the DB cluster will belong to.
mdcVPCSecurityGroupIds :: Lens' ModifyDBCluster [Text]
mdcVPCSecurityGroupIds = lens _mdcVPCSecurityGroupIds (\s a -> s {_mdcVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | The name of the DB cluster parameter group to use for the DB cluster.
mdcDBClusterParameterGroupName :: Lens' ModifyDBCluster (Maybe Text)
mdcDBClusterParameterGroupName = lens _mdcDBClusterParameterGroupName (\s a -> s {_mdcDBClusterParameterGroupName = a})

-- | The scaling properties of the DB cluster. You can only modify scaling properties for DB clusters in @serverless@ DB engine mode.
mdcScalingConfiguration :: Lens' ModifyDBCluster (Maybe ScalingConfiguration)
mdcScalingConfiguration = lens _mdcScalingConfiguration (\s a -> s {_mdcScalingConfiguration = a})

-- | A value that indicates whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB cluster. If this parameter is disabled, changes to the DB cluster are applied during the next maintenance window. The @ApplyImmediately@ parameter only affects the @EnableIAMDatabaseAuthentication@ , @MasterUserPassword@ , and @NewDBClusterIdentifier@ values. If the @ApplyImmediately@ parameter is disabled, then changes to the @EnableIAMDatabaseAuthentication@ , @MasterUserPassword@ , and @NewDBClusterIdentifier@ values are applied during the next maintenance window. All other changes are applied immediately, regardless of the value of the @ApplyImmediately@ parameter. By default, this parameter is disabled.
mdcApplyImmediately :: Lens' ModifyDBCluster (Maybe Bool)
mdcApplyImmediately = lens _mdcApplyImmediately (\s a -> s {_mdcApplyImmediately = a})

-- | A value that indicates that the DB cluster should be associated with the specified option group. Changing this parameter doesn't result in an outage except in the following case, and the change is applied during the next maintenance window unless the @ApplyImmediately@ is enabled for this request. If the parameter change results in an option group that enables OEM, this change can cause a brief (sub-second) period during which new connections are rejected but existing connections are not interrupted.  Permanent options can't be removed from an option group. The option group can't be removed from a DB cluster once it is associated with a DB cluster.
mdcOptionGroupName :: Lens' ModifyDBCluster (Maybe Text)
mdcOptionGroupName = lens _mdcOptionGroupName (\s a -> s {_mdcOptionGroupName = a})

-- | A value that indicates whether to copy all tags from the DB cluster to snapshots of the DB cluster. The default is not to copy them.
mdcCopyTagsToSnapshot :: Lens' ModifyDBCluster (Maybe Bool)
mdcCopyTagsToSnapshot = lens _mdcCopyTagsToSnapshot (\s a -> s {_mdcCopyTagsToSnapshot = a})

-- | The new DB cluster identifier for the DB cluster when renaming a DB cluster. This value is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens     * The first character must be a letter     * Can't end with a hyphen or contain two consecutive hyphens Example: @my-cluster2@
mdcNewDBClusterIdentifier :: Lens' ModifyDBCluster (Maybe Text)
mdcNewDBClusterIdentifier = lens _mdcNewDBClusterIdentifier (\s a -> s {_mdcNewDBClusterIdentifier = a})

-- | The name of the DB parameter group to apply to all instances of the DB cluster.  Default: The existing name setting Constraints:     * The DB parameter group must be in the same DB parameter group family as this DB cluster.     * The @DBInstanceParameterGroupName@ parameter is only valid in combination with the @AllowMajorVersionUpgrade@ parameter.
mdcDBInstanceParameterGroupName :: Lens' ModifyDBCluster (Maybe Text)
mdcDBInstanceParameterGroupName = lens _mdcDBInstanceParameterGroupName (\s a -> s {_mdcDBInstanceParameterGroupName = a})

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
mdcDomainIAMRoleName :: Lens' ModifyDBCluster (Maybe Text)
mdcDomainIAMRoleName = lens _mdcDomainIAMRoleName (\s a -> s {_mdcDomainIAMRoleName = a})

-- | The port number on which the DB cluster accepts connections. Constraints: Value must be @1150-65535@  Default: The same port as the original DB cluster.
mdcPort :: Lens' ModifyDBCluster (Maybe Int)
mdcPort = lens _mdcPort (\s a -> s {_mdcPort = a})

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication> in the /Amazon Aurora User Guide./
mdcEnableIAMDatabaseAuthentication :: Lens' ModifyDBCluster (Maybe Bool)
mdcEnableIAMDatabaseAuthentication = lens _mdcEnableIAMDatabaseAuthentication (\s a -> s {_mdcEnableIAMDatabaseAuthentication = a})

-- | The DB cluster identifier for the cluster being modified. This parameter isn't case-sensitive. Constraints: This identifier must match the identifier of an existing DB cluster.
mdcDBClusterIdentifier :: Lens' ModifyDBCluster Text
mdcDBClusterIdentifier = lens _mdcDBClusterIdentifier (\s a -> s {_mdcDBClusterIdentifier = a})

instance AWSRequest ModifyDBCluster where
  type Rs ModifyDBCluster = ModifyDBClusterResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "ModifyDBClusterResult"
      ( \s h x ->
          ModifyDBClusterResponse'
            <$> (x .@? "DBCluster") <*> (pure (fromEnum s))
      )

instance Hashable ModifyDBCluster

instance NFData ModifyDBCluster

instance ToHeaders ModifyDBCluster where
  toHeaders = const mempty

instance ToPath ModifyDBCluster where
  toPath = const "/"

instance ToQuery ModifyDBCluster where
  toQuery ModifyDBCluster' {..} =
    mconcat
      [ "Action" =: ("ModifyDBCluster" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "EngineVersion" =: _mdcEngineVersion,
        "EnableGlobalWriteForwarding" =: _mdcEnableGlobalWriteForwarding,
        "DeletionProtection" =: _mdcDeletionProtection,
        "MasterUserPassword" =: _mdcMasterUserPassword,
        "EnableHttpEndpoint" =: _mdcEnableHTTPEndpoint,
        "AllowMajorVersionUpgrade" =: _mdcAllowMajorVersionUpgrade,
        "Domain" =: _mdcDomain,
        "BacktrackWindow" =: _mdcBacktrackWindow,
        "CloudwatchLogsExportConfiguration"
          =: _mdcCloudwatchLogsExportConfiguration,
        "PreferredMaintenanceWindow" =: _mdcPreferredMaintenanceWindow,
        "PreferredBackupWindow" =: _mdcPreferredBackupWindow,
        "BackupRetentionPeriod" =: _mdcBackupRetentionPeriod,
        "VpcSecurityGroupIds"
          =: toQuery
            (toQueryList "VpcSecurityGroupId" <$> _mdcVPCSecurityGroupIds),
        "DBClusterParameterGroupName" =: _mdcDBClusterParameterGroupName,
        "ScalingConfiguration" =: _mdcScalingConfiguration,
        "ApplyImmediately" =: _mdcApplyImmediately,
        "OptionGroupName" =: _mdcOptionGroupName,
        "CopyTagsToSnapshot" =: _mdcCopyTagsToSnapshot,
        "NewDBClusterIdentifier" =: _mdcNewDBClusterIdentifier,
        "DBInstanceParameterGroupName" =: _mdcDBInstanceParameterGroupName,
        "DomainIAMRoleName" =: _mdcDomainIAMRoleName,
        "Port" =: _mdcPort,
        "EnableIAMDatabaseAuthentication"
          =: _mdcEnableIAMDatabaseAuthentication,
        "DBClusterIdentifier" =: _mdcDBClusterIdentifier
      ]

-- | /See:/ 'modifyDBClusterResponse' smart constructor.
data ModifyDBClusterResponse = ModifyDBClusterResponse'
  { _mdcrsDBCluster ::
      !(Maybe DBCluster),
    _mdcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyDBClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdcrsDBCluster' - Undocumented member.
--
-- * 'mdcrsResponseStatus' - -- | The response status code.
modifyDBClusterResponse ::
  -- | 'mdcrsResponseStatus'
  Int ->
  ModifyDBClusterResponse
modifyDBClusterResponse pResponseStatus_ =
  ModifyDBClusterResponse'
    { _mdcrsDBCluster = Nothing,
      _mdcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
mdcrsDBCluster :: Lens' ModifyDBClusterResponse (Maybe DBCluster)
mdcrsDBCluster = lens _mdcrsDBCluster (\s a -> s {_mdcrsDBCluster = a})

-- | -- | The response status code.
mdcrsResponseStatus :: Lens' ModifyDBClusterResponse Int
mdcrsResponseStatus = lens _mdcrsResponseStatus (\s a -> s {_mdcrsResponseStatus = a})

instance NFData ModifyDBClusterResponse
