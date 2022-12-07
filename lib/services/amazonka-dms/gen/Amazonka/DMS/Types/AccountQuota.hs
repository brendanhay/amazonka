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
-- Module      : Amazonka.DMS.Types.AccountQuota
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.AccountQuota where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a quota for an Amazon Web Services account, for example the
-- number of replication instances allowed.
--
-- /See:/ 'newAccountQuota' smart constructor.
data AccountQuota = AccountQuota'
  { -- | The maximum allowed value for the quota.
    max :: Prelude.Maybe Prelude.Integer,
    -- | The amount currently used toward the quota maximum.
    used :: Prelude.Maybe Prelude.Integer,
    -- | The name of the DMS quota for this Amazon Web Services account.
    accountQuotaName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountQuota' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'accountQuota_max' - The maximum allowed value for the quota.
--
-- 'used', 'accountQuota_used' - The amount currently used toward the quota maximum.
--
-- 'accountQuotaName', 'accountQuota_accountQuotaName' - The name of the DMS quota for this Amazon Web Services account.
newAccountQuota ::
  AccountQuota
newAccountQuota =
  AccountQuota'
    { max = Prelude.Nothing,
      used = Prelude.Nothing,
      accountQuotaName = Prelude.Nothing
    }

-- | The maximum allowed value for the quota.
accountQuota_max :: Lens.Lens' AccountQuota (Prelude.Maybe Prelude.Integer)
accountQuota_max = Lens.lens (\AccountQuota' {max} -> max) (\s@AccountQuota' {} a -> s {max = a} :: AccountQuota)

-- | The amount currently used toward the quota maximum.
accountQuota_used :: Lens.Lens' AccountQuota (Prelude.Maybe Prelude.Integer)
accountQuota_used = Lens.lens (\AccountQuota' {used} -> used) (\s@AccountQuota' {} a -> s {used = a} :: AccountQuota)

-- | The name of the DMS quota for this Amazon Web Services account.
accountQuota_accountQuotaName :: Lens.Lens' AccountQuota (Prelude.Maybe Prelude.Text)
accountQuota_accountQuotaName = Lens.lens (\AccountQuota' {accountQuotaName} -> accountQuotaName) (\s@AccountQuota' {} a -> s {accountQuotaName = a} :: AccountQuota)

instance Data.FromJSON AccountQuota where
  parseJSON =
    Data.withObject
      "AccountQuota"
      ( \x ->
          AccountQuota'
            Prelude.<$> (x Data..:? "Max")
            Prelude.<*> (x Data..:? "Used")
            Prelude.<*> (x Data..:? "AccountQuotaName")
      )

instance Prelude.Hashable AccountQuota where
  hashWithSalt _salt AccountQuota' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` used
      `Prelude.hashWithSalt` accountQuotaName

instance Prelude.NFData AccountQuota where
  rnf AccountQuota' {..} =
    Prelude.rnf max
      `Prelude.seq` Prelude.rnf used
      `Prelude.seq` Prelude.rnf accountQuotaName
