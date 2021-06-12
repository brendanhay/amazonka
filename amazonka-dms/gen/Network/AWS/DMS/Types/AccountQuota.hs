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
-- Module      : Network.AWS.DMS.Types.AccountQuota
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.AccountQuota where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a quota for an AWS account, for example, the number of
-- replication instances allowed.
--
-- /See:/ 'newAccountQuota' smart constructor.
data AccountQuota = AccountQuota'
  { -- | The amount currently used toward the quota maximum.
    used :: Core.Maybe Core.Integer,
    -- | The name of the AWS DMS quota for this AWS account.
    accountQuotaName :: Core.Maybe Core.Text,
    -- | The maximum allowed value for the quota.
    max :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'accountQuotaName', 'accountQuota_accountQuotaName' - The name of the AWS DMS quota for this AWS account.
--
-- 'max', 'accountQuota_max' - The maximum allowed value for the quota.
newAccountQuota ::
  AccountQuota
newAccountQuota =
  AccountQuota'
    { used = Core.Nothing,
      accountQuotaName = Core.Nothing,
      max = Core.Nothing
    }

-- | The amount currently used toward the quota maximum.
accountQuota_used :: Lens.Lens' AccountQuota (Core.Maybe Core.Integer)
accountQuota_used = Lens.lens (\AccountQuota' {used} -> used) (\s@AccountQuota' {} a -> s {used = a} :: AccountQuota)

-- | The name of the AWS DMS quota for this AWS account.
accountQuota_accountQuotaName :: Lens.Lens' AccountQuota (Core.Maybe Core.Text)
accountQuota_accountQuotaName = Lens.lens (\AccountQuota' {accountQuotaName} -> accountQuotaName) (\s@AccountQuota' {} a -> s {accountQuotaName = a} :: AccountQuota)

-- | The maximum allowed value for the quota.
accountQuota_max :: Lens.Lens' AccountQuota (Core.Maybe Core.Integer)
accountQuota_max = Lens.lens (\AccountQuota' {max} -> max) (\s@AccountQuota' {} a -> s {max = a} :: AccountQuota)

instance Core.FromJSON AccountQuota where
  parseJSON =
    Core.withObject
      "AccountQuota"
      ( \x ->
          AccountQuota'
            Core.<$> (x Core..:? "Used")
            Core.<*> (x Core..:? "AccountQuotaName")
            Core.<*> (x Core..:? "Max")
      )

instance Core.Hashable AccountQuota

instance Core.NFData AccountQuota
