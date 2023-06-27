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
-- Module      : Amazonka.FMS.Types.AdminAccountSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.AdminAccountSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.OrganizationStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains high level information about the Firewall Manager administrator
-- account.
--
-- /See:/ 'newAdminAccountSummary' smart constructor.
data AdminAccountSummary = AdminAccountSummary'
  { -- | The Amazon Web Services account ID of the Firewall Manager
    -- administrator\'s account.
    adminAccount :: Prelude.Maybe Prelude.Text,
    -- | A boolean value that indicates if the administrator is the default
    -- administrator. If true, then this is the default administrator account.
    -- The default administrator can manage third-party firewalls and has full
    -- administrative scope. There is only one default administrator account
    -- per organization. For information about Firewall Manager default
    -- administrator accounts, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/fms-administrators.html Managing Firewall Manager administrators>
    -- in the /Firewall Manager Developer Guide/.
    defaultAdmin :: Prelude.Maybe Prelude.Bool,
    -- | The current status of the request to onboard a member account as an
    -- Firewall Manager administator.
    --
    -- -   @ONBOARDING@ - The account is onboarding to Firewall Manager as an
    --     administrator.
    --
    -- -   @ONBOARDING_COMPLETE@ - Firewall Manager The account is onboarded to
    --     Firewall Manager as an administrator, and can perform actions on the
    --     resources defined in their AdminScope.
    --
    -- -   @OFFBOARDING@ - The account is being removed as an Firewall Manager
    --     administrator.
    --
    -- -   @OFFBOARDING_COMPLETE@ - The account has been removed as an Firewall
    --     Manager administrator.
    status :: Prelude.Maybe OrganizationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminAccountSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccount', 'adminAccountSummary_adminAccount' - The Amazon Web Services account ID of the Firewall Manager
-- administrator\'s account.
--
-- 'defaultAdmin', 'adminAccountSummary_defaultAdmin' - A boolean value that indicates if the administrator is the default
-- administrator. If true, then this is the default administrator account.
-- The default administrator can manage third-party firewalls and has full
-- administrative scope. There is only one default administrator account
-- per organization. For information about Firewall Manager default
-- administrator accounts, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/fms-administrators.html Managing Firewall Manager administrators>
-- in the /Firewall Manager Developer Guide/.
--
-- 'status', 'adminAccountSummary_status' - The current status of the request to onboard a member account as an
-- Firewall Manager administator.
--
-- -   @ONBOARDING@ - The account is onboarding to Firewall Manager as an
--     administrator.
--
-- -   @ONBOARDING_COMPLETE@ - Firewall Manager The account is onboarded to
--     Firewall Manager as an administrator, and can perform actions on the
--     resources defined in their AdminScope.
--
-- -   @OFFBOARDING@ - The account is being removed as an Firewall Manager
--     administrator.
--
-- -   @OFFBOARDING_COMPLETE@ - The account has been removed as an Firewall
--     Manager administrator.
newAdminAccountSummary ::
  AdminAccountSummary
newAdminAccountSummary =
  AdminAccountSummary'
    { adminAccount =
        Prelude.Nothing,
      defaultAdmin = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the Firewall Manager
-- administrator\'s account.
adminAccountSummary_adminAccount :: Lens.Lens' AdminAccountSummary (Prelude.Maybe Prelude.Text)
adminAccountSummary_adminAccount = Lens.lens (\AdminAccountSummary' {adminAccount} -> adminAccount) (\s@AdminAccountSummary' {} a -> s {adminAccount = a} :: AdminAccountSummary)

-- | A boolean value that indicates if the administrator is the default
-- administrator. If true, then this is the default administrator account.
-- The default administrator can manage third-party firewalls and has full
-- administrative scope. There is only one default administrator account
-- per organization. For information about Firewall Manager default
-- administrator accounts, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/fms-administrators.html Managing Firewall Manager administrators>
-- in the /Firewall Manager Developer Guide/.
adminAccountSummary_defaultAdmin :: Lens.Lens' AdminAccountSummary (Prelude.Maybe Prelude.Bool)
adminAccountSummary_defaultAdmin = Lens.lens (\AdminAccountSummary' {defaultAdmin} -> defaultAdmin) (\s@AdminAccountSummary' {} a -> s {defaultAdmin = a} :: AdminAccountSummary)

-- | The current status of the request to onboard a member account as an
-- Firewall Manager administator.
--
-- -   @ONBOARDING@ - The account is onboarding to Firewall Manager as an
--     administrator.
--
-- -   @ONBOARDING_COMPLETE@ - Firewall Manager The account is onboarded to
--     Firewall Manager as an administrator, and can perform actions on the
--     resources defined in their AdminScope.
--
-- -   @OFFBOARDING@ - The account is being removed as an Firewall Manager
--     administrator.
--
-- -   @OFFBOARDING_COMPLETE@ - The account has been removed as an Firewall
--     Manager administrator.
adminAccountSummary_status :: Lens.Lens' AdminAccountSummary (Prelude.Maybe OrganizationStatus)
adminAccountSummary_status = Lens.lens (\AdminAccountSummary' {status} -> status) (\s@AdminAccountSummary' {} a -> s {status = a} :: AdminAccountSummary)

instance Data.FromJSON AdminAccountSummary where
  parseJSON =
    Data.withObject
      "AdminAccountSummary"
      ( \x ->
          AdminAccountSummary'
            Prelude.<$> (x Data..:? "AdminAccount")
            Prelude.<*> (x Data..:? "DefaultAdmin")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable AdminAccountSummary where
  hashWithSalt _salt AdminAccountSummary' {..} =
    _salt
      `Prelude.hashWithSalt` adminAccount
      `Prelude.hashWithSalt` defaultAdmin
      `Prelude.hashWithSalt` status

instance Prelude.NFData AdminAccountSummary where
  rnf AdminAccountSummary' {..} =
    Prelude.rnf adminAccount
      `Prelude.seq` Prelude.rnf defaultAdmin
      `Prelude.seq` Prelude.rnf status
