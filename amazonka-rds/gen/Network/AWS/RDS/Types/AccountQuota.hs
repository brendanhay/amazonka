{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.AccountQuota
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.AccountQuota where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a quota for an AWS account.
--
-- The following are account quotas:
--
-- -   @AllocatedStorage@ - The total allocated storage per account, in
--     GiB. The used value is the total allocated storage in the account,
--     in GiB.
--
-- -   @AuthorizationsPerDBSecurityGroup@ - The number of ingress rules per
--     DB security group. The used value is the highest number of ingress
--     rules in a DB security group in the account. Other DB security
--     groups in the account might have a lower number of ingress rules.
--
-- -   @CustomEndpointsPerDBCluster@ - The number of custom endpoints per
--     DB cluster. The used value is the highest number of custom endpoints
--     in a DB clusters in the account. Other DB clusters in the account
--     might have a lower number of custom endpoints.
--
-- -   @DBClusterParameterGroups@ - The number of DB cluster parameter
--     groups per account, excluding default parameter groups. The used
--     value is the count of nondefault DB cluster parameter groups in the
--     account.
--
-- -   @DBClusterRoles@ - The number of associated AWS Identity and Access
--     Management (IAM) roles per DB cluster. The used value is the highest
--     number of associated IAM roles for a DB cluster in the account.
--     Other DB clusters in the account might have a lower number of
--     associated IAM roles.
--
-- -   @DBClusters@ - The number of DB clusters per account. The used value
--     is the count of DB clusters in the account.
--
-- -   @DBInstanceRoles@ - The number of associated IAM roles per DB
--     instance. The used value is the highest number of associated IAM
--     roles for a DB instance in the account. Other DB instances in the
--     account might have a lower number of associated IAM roles.
--
-- -   @DBInstances@ - The number of DB instances per account. The used
--     value is the count of the DB instances in the account.
--
--     Amazon RDS DB instances, Amazon Aurora DB instances, Amazon Neptune
--     instances, and Amazon DocumentDB instances apply to this quota.
--
-- -   @DBParameterGroups@ - The number of DB parameter groups per account,
--     excluding default parameter groups. The used value is the count of
--     nondefault DB parameter groups in the account.
--
-- -   @DBSecurityGroups@ - The number of DB security groups (not VPC
--     security groups) per account, excluding the default security group.
--     The used value is the count of nondefault DB security groups in the
--     account.
--
-- -   @DBSubnetGroups@ - The number of DB subnet groups per account. The
--     used value is the count of the DB subnet groups in the account.
--
-- -   @EventSubscriptions@ - The number of event subscriptions per
--     account. The used value is the count of the event subscriptions in
--     the account.
--
-- -   @ManualClusterSnapshots@ - The number of manual DB cluster snapshots
--     per account. The used value is the count of the manual DB cluster
--     snapshots in the account.
--
-- -   @ManualSnapshots@ - The number of manual DB instance snapshots per
--     account. The used value is the count of the manual DB instance
--     snapshots in the account.
--
-- -   @OptionGroups@ - The number of DB option groups per account,
--     excluding default option groups. The used value is the count of
--     nondefault DB option groups in the account.
--
-- -   @ReadReplicasPerMaster@ - The number of read replicas per DB
--     instance. The used value is the highest number of read replicas for
--     a DB instance in the account. Other DB instances in the account
--     might have a lower number of read replicas.
--
-- -   @ReservedDBInstances@ - The number of reserved DB instances per
--     account. The used value is the count of the active reserved DB
--     instances in the account.
--
-- -   @SubnetsPerDBSubnetGroup@ - The number of subnets per DB subnet
--     group. The used value is highest number of subnets for a DB subnet
--     group in the account. Other DB subnet groups in the account might
--     have a lower number of subnets.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Limits.html Quotas for Amazon RDS>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_Limits.html Quotas for Amazon Aurora>
-- in the /Amazon Aurora User Guide/.
--
-- /See:/ 'newAccountQuota' smart constructor.
data AccountQuota = AccountQuota'
  { -- | The amount currently used toward the quota maximum.
    used :: Prelude.Maybe Prelude.Integer,
    -- | The name of the Amazon RDS quota for this AWS account.
    accountQuotaName :: Prelude.Maybe Prelude.Text,
    -- | The maximum allowed value for the quota.
    max :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccountQuota' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'used', 'accountQuota_used' - The amount currently used toward the quota maximum.
--
-- 'accountQuotaName', 'accountQuota_accountQuotaName' - The name of the Amazon RDS quota for this AWS account.
--
-- 'max', 'accountQuota_max' - The maximum allowed value for the quota.
newAccountQuota ::
  AccountQuota
newAccountQuota =
  AccountQuota'
    { used = Prelude.Nothing,
      accountQuotaName = Prelude.Nothing,
      max = Prelude.Nothing
    }

-- | The amount currently used toward the quota maximum.
accountQuota_used :: Lens.Lens' AccountQuota (Prelude.Maybe Prelude.Integer)
accountQuota_used = Lens.lens (\AccountQuota' {used} -> used) (\s@AccountQuota' {} a -> s {used = a} :: AccountQuota)

-- | The name of the Amazon RDS quota for this AWS account.
accountQuota_accountQuotaName :: Lens.Lens' AccountQuota (Prelude.Maybe Prelude.Text)
accountQuota_accountQuotaName = Lens.lens (\AccountQuota' {accountQuotaName} -> accountQuotaName) (\s@AccountQuota' {} a -> s {accountQuotaName = a} :: AccountQuota)

-- | The maximum allowed value for the quota.
accountQuota_max :: Lens.Lens' AccountQuota (Prelude.Maybe Prelude.Integer)
accountQuota_max = Lens.lens (\AccountQuota' {max} -> max) (\s@AccountQuota' {} a -> s {max = a} :: AccountQuota)

instance Prelude.FromXML AccountQuota where
  parseXML x =
    AccountQuota'
      Prelude.<$> (x Prelude..@? "Used")
      Prelude.<*> (x Prelude..@? "AccountQuotaName")
      Prelude.<*> (x Prelude..@? "Max")

instance Prelude.Hashable AccountQuota

instance Prelude.NFData AccountQuota
