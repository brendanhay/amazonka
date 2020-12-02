{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.AccountQuota
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.AccountQuota where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a quota for an AWS account.
--
--
-- The following are account quotas:
--
--     * @AllocatedStorage@ - The total allocated storage per account, in GiB. The used value is the total allocated storage in the account, in GiB.
--
--     * @AuthorizationsPerDBSecurityGroup@ - The number of ingress rules per DB security group. The used value is the highest number of ingress rules in a DB security group in the account. Other DB security groups in the account might have a lower number of ingress rules.
--
--     * @CustomEndpointsPerDBCluster@ - The number of custom endpoints per DB cluster. The used value is the highest number of custom endpoints in a DB clusters in the account. Other DB clusters in the account might have a lower number of custom endpoints.
--
--     * @DBClusterParameterGroups@ - The number of DB cluster parameter groups per account, excluding default parameter groups. The used value is the count of nondefault DB cluster parameter groups in the account.
--
--     * @DBClusterRoles@ - The number of associated AWS Identity and Access Management (IAM) roles per DB cluster. The used value is the highest number of associated IAM roles for a DB cluster in the account. Other DB clusters in the account might have a lower number of associated IAM roles.
--
--     * @DBClusters@ - The number of DB clusters per account. The used value is the count of DB clusters in the account.
--
--     * @DBInstanceRoles@ - The number of associated IAM roles per DB instance. The used value is the highest number of associated IAM roles for a DB instance in the account. Other DB instances in the account might have a lower number of associated IAM roles.
--
--     * @DBInstances@ - The number of DB instances per account. The used value is the count of the DB instances in the account.
--
-- Amazon RDS DB instances, Amazon Aurora DB instances, Amazon Neptune instances, and Amazon DocumentDB instances apply to this quota.
--
--     * @DBParameterGroups@ - The number of DB parameter groups per account, excluding default parameter groups. The used value is the count of nondefault DB parameter groups in the account.
--
--     * @DBSecurityGroups@ - The number of DB security groups (not VPC security groups) per account, excluding the default security group. The used value is the count of nondefault DB security groups in the account.
--
--     * @DBSubnetGroups@ - The number of DB subnet groups per account. The used value is the count of the DB subnet groups in the account.
--
--     * @EventSubscriptions@ - The number of event subscriptions per account. The used value is the count of the event subscriptions in the account.
--
--     * @ManualClusterSnapshots@ - The number of manual DB cluster snapshots per account. The used value is the count of the manual DB cluster snapshots in the account.
--
--     * @ManualSnapshots@ - The number of manual DB instance snapshots per account. The used value is the count of the manual DB instance snapshots in the account.
--
--     * @OptionGroups@ - The number of DB option groups per account, excluding default option groups. The used value is the count of nondefault DB option groups in the account.
--
--     * @ReadReplicasPerMaster@ - The number of read replicas per DB instance. The used value is the highest number of read replicas for a DB instance in the account. Other DB instances in the account might have a lower number of read replicas.
--
--     * @ReservedDBInstances@ - The number of reserved DB instances per account. The used value is the count of the active reserved DB instances in the account.
--
--     * @SubnetsPerDBSubnetGroup@ - The number of subnets per DB subnet group. The used value is highest number of subnets for a DB subnet group in the account. Other DB subnet groups in the account might have a lower number of subnets.
--
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Limits.html Quotas for Amazon RDS> in the /Amazon RDS User Guide/ and <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_Limits.html Quotas for Amazon Aurora> in the /Amazon Aurora User Guide/ .
--
--
-- /See:/ 'accountQuota' smart constructor.
data AccountQuota = AccountQuota'
  { _aqMax :: !(Maybe Integer),
    _aqUsed :: !(Maybe Integer),
    _aqAccountQuotaName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountQuota' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aqMax' - The maximum allowed value for the quota.
--
-- * 'aqUsed' - The amount currently used toward the quota maximum.
--
-- * 'aqAccountQuotaName' - The name of the Amazon RDS quota for this AWS account.
accountQuota ::
  AccountQuota
accountQuota =
  AccountQuota'
    { _aqMax = Nothing,
      _aqUsed = Nothing,
      _aqAccountQuotaName = Nothing
    }

-- | The maximum allowed value for the quota.
aqMax :: Lens' AccountQuota (Maybe Integer)
aqMax = lens _aqMax (\s a -> s {_aqMax = a})

-- | The amount currently used toward the quota maximum.
aqUsed :: Lens' AccountQuota (Maybe Integer)
aqUsed = lens _aqUsed (\s a -> s {_aqUsed = a})

-- | The name of the Amazon RDS quota for this AWS account.
aqAccountQuotaName :: Lens' AccountQuota (Maybe Text)
aqAccountQuotaName = lens _aqAccountQuotaName (\s a -> s {_aqAccountQuotaName = a})

instance FromXML AccountQuota where
  parseXML x =
    AccountQuota'
      <$> (x .@? "Max") <*> (x .@? "Used") <*> (x .@? "AccountQuotaName")

instance Hashable AccountQuota

instance NFData AccountQuota
