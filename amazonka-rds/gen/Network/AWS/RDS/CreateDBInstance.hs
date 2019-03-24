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
-- Module      : Network.AWS.RDS.CreateDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance.
--
--
module Network.AWS.RDS.CreateDBInstance
    (
    -- * Creating a Request
      createDBInstance
    , CreateDBInstance
    -- * Request Lenses
    , cdiEngineVersion
    , cdiDBSecurityGroups
    , cdiDeletionProtection
    , cdiStorageEncrypted
    , cdiDBClusterIdentifier
    , cdiMasterUserPassword
    , cdiPubliclyAccessible
    , cdiAutoMinorVersionUpgrade
    , cdiMasterUsername
    , cdiDBSubnetGroupName
    , cdiMonitoringRoleARN
    , cdiIOPS
    , cdiDomain
    , cdiMonitoringInterval
    , cdiTDECredentialPassword
    , cdiProcessorFeatures
    , cdiPromotionTier
    , cdiLicenseModel
    , cdiPreferredMaintenanceWindow
    , cdiPerformanceInsightsRetentionPeriod
    , cdiCharacterSetName
    , cdiEnablePerformanceInsights
    , cdiKMSKeyId
    , cdiDBParameterGroupName
    , cdiPreferredBackupWindow
    , cdiAvailabilityZone
    , cdiBackupRetentionPeriod
    , cdiPerformanceInsightsKMSKeyId
    , cdiVPCSecurityGroupIds
    , cdiMultiAZ
    , cdiAllocatedStorage
    , cdiOptionGroupName
    , cdiCopyTagsToSnapshot
    , cdiTimezone
    , cdiTDECredentialARN
    , cdiDomainIAMRoleName
    , cdiTags
    , cdiPort
    , cdiEnableIAMDatabaseAuthentication
    , cdiStorageType
    , cdiEnableCloudwatchLogsExports
    , cdiDBName
    , cdiDBInstanceIdentifier
    , cdiDBInstanceClass
    , cdiEngine

    -- * Destructuring the Response
    , createDBInstanceResponse
    , CreateDBInstanceResponse
    -- * Response Lenses
    , cdirsDBInstance
    , cdirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createDBInstance' smart constructor.
data CreateDBInstance = CreateDBInstance'
  { _cdiEngineVersion                      :: !(Maybe Text)
  , _cdiDBSecurityGroups                   :: !(Maybe [Text])
  , _cdiDeletionProtection                 :: !(Maybe Bool)
  , _cdiStorageEncrypted                   :: !(Maybe Bool)
  , _cdiDBClusterIdentifier                :: !(Maybe Text)
  , _cdiMasterUserPassword                 :: !(Maybe Text)
  , _cdiPubliclyAccessible                 :: !(Maybe Bool)
  , _cdiAutoMinorVersionUpgrade            :: !(Maybe Bool)
  , _cdiMasterUsername                     :: !(Maybe Text)
  , _cdiDBSubnetGroupName                  :: !(Maybe Text)
  , _cdiMonitoringRoleARN                  :: !(Maybe Text)
  , _cdiIOPS                               :: !(Maybe Int)
  , _cdiDomain                             :: !(Maybe Text)
  , _cdiMonitoringInterval                 :: !(Maybe Int)
  , _cdiTDECredentialPassword              :: !(Maybe Text)
  , _cdiProcessorFeatures                  :: !(Maybe [ProcessorFeature])
  , _cdiPromotionTier                      :: !(Maybe Int)
  , _cdiLicenseModel                       :: !(Maybe Text)
  , _cdiPreferredMaintenanceWindow         :: !(Maybe Text)
  , _cdiPerformanceInsightsRetentionPeriod :: !(Maybe Int)
  , _cdiCharacterSetName                   :: !(Maybe Text)
  , _cdiEnablePerformanceInsights          :: !(Maybe Bool)
  , _cdiKMSKeyId                           :: !(Maybe Text)
  , _cdiDBParameterGroupName               :: !(Maybe Text)
  , _cdiPreferredBackupWindow              :: !(Maybe Text)
  , _cdiAvailabilityZone                   :: !(Maybe Text)
  , _cdiBackupRetentionPeriod              :: !(Maybe Int)
  , _cdiPerformanceInsightsKMSKeyId        :: !(Maybe Text)
  , _cdiVPCSecurityGroupIds                :: !(Maybe [Text])
  , _cdiMultiAZ                            :: !(Maybe Bool)
  , _cdiAllocatedStorage                   :: !(Maybe Int)
  , _cdiOptionGroupName                    :: !(Maybe Text)
  , _cdiCopyTagsToSnapshot                 :: !(Maybe Bool)
  , _cdiTimezone                           :: !(Maybe Text)
  , _cdiTDECredentialARN                   :: !(Maybe Text)
  , _cdiDomainIAMRoleName                  :: !(Maybe Text)
  , _cdiTags                               :: !(Maybe [Tag])
  , _cdiPort                               :: !(Maybe Int)
  , _cdiEnableIAMDatabaseAuthentication    :: !(Maybe Bool)
  , _cdiStorageType                        :: !(Maybe Text)
  , _cdiEnableCloudwatchLogsExports        :: !(Maybe [Text])
  , _cdiDBName                             :: !(Maybe Text)
  , _cdiDBInstanceIdentifier               :: !Text
  , _cdiDBInstanceClass                    :: !Text
  , _cdiEngine                             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdiEngineVersion' - The version number of the database engine to use. For a list of valid engine versions, call 'DescribeDBEngineVersions' . The following are the database engines and links to information about the major and minor versions that are available with Amazon RDS. Not every database engine is available for every AWS Region. __Amazon Aurora__  Not applicable. The version number of the database engine to be used by the DB instance is managed by the DB cluster. For more information, see 'CreateDBCluster' . __MariaDB__  See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MariaDB.html#MariaDB.Concepts.VersionMgmt MariaDB on Amazon RDS Versions> in the /Amazon RDS User Guide./  __Microsoft SQL Server__  See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.FeatureSupport Version and Feature Support on Amazon RDS> in the /Amazon RDS User Guide./  __MySQL__  See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions> in the /Amazon RDS User Guide./  __Oracle__  See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.Oracle.PatchComposition.html Oracle Database Engine Release Notes> in the /Amazon RDS User Guide./  __PostgreSQL__  See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts.General.DBVersions Supported PostgreSQL Database Versions> in the /Amazon RDS User Guide./
--
-- * 'cdiDBSecurityGroups' - A list of DB security groups to associate with this DB instance. Default: The default DB security group for the database engine.
--
-- * 'cdiDeletionProtection' - Indicates if the DB instance should have deletion protection enabled. The database can't be deleted when this value is set to true. The default is false. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- * 'cdiStorageEncrypted' - Specifies whether the DB instance is encrypted. __Amazon Aurora__  Not applicable. The encryption for DB instances is managed by the DB cluster. For more information, see 'CreateDBCluster' . Default: false
--
-- * 'cdiDBClusterIdentifier' - The identifier of the DB cluster that the instance will belong to. For information on creating a DB cluster, see 'CreateDBCluster' . Type: String
--
-- * 'cdiMasterUserPassword' - The password for the master user. The password can include any printable ASCII character except "/", """, or "@". __Amazon Aurora__  Not applicable. The password for the master user is managed by the DB cluster. For more information, see 'CreateDBCluster' . __MariaDB__  Constraints: Must contain from 8 to 41 characters. __Microsoft SQL Server__  Constraints: Must contain from 8 to 128 characters. __MySQL__  Constraints: Must contain from 8 to 41 characters. __Oracle__  Constraints: Must contain from 8 to 30 characters. __PostgreSQL__  Constraints: Must contain from 8 to 128 characters.
--
-- * 'cdiPubliclyAccessible' - Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether @DBSubnetGroupName@ is specified. If @DBSubnetGroupName@ is not specified, and @PubliclyAccessible@ is not specified, the following applies:     * If the default VPC in the target region doesn
