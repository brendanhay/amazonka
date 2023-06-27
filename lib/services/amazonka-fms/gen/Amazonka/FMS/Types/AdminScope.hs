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
-- Module      : Amazonka.FMS.Types.AdminScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.AdminScope where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.AccountScope
import Amazonka.FMS.Types.OrganizationalUnitScope
import Amazonka.FMS.Types.PolicyTypeScope
import Amazonka.FMS.Types.RegionScope
import qualified Amazonka.Prelude as Prelude

-- | Defines the resources that the Firewall Manager administrator can
-- manage. For more information about administrative scope, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/fms-administrators.html Managing Firewall Manager administrators>
-- in the /Firewall Manager Developer Guide/.
--
-- /See:/ 'newAdminScope' smart constructor.
data AdminScope = AdminScope'
  { -- | Defines the accounts that the specified Firewall Manager administrator
    -- can apply policies to.
    accountScope :: Prelude.Maybe AccountScope,
    -- | Defines the Organizations organizational units that the specified
    -- Firewall Manager administrator can apply policies to. For more
    -- information about OUs in Organizations, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_ous.html Managing organizational units (OUs)>
    -- in the /Organizations User Guide/.
    organizationalUnitScope :: Prelude.Maybe OrganizationalUnitScope,
    -- | Defines the Firewall Manager policy types that the specified Firewall
    -- Manager administrator can create and manage.
    policyTypeScope :: Prelude.Maybe PolicyTypeScope,
    -- | Defines the Amazon Web Services Regions that the specified Firewall
    -- Manager administrator can perform actions in.
    regionScope :: Prelude.Maybe RegionScope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountScope', 'adminScope_accountScope' - Defines the accounts that the specified Firewall Manager administrator
-- can apply policies to.
--
-- 'organizationalUnitScope', 'adminScope_organizationalUnitScope' - Defines the Organizations organizational units that the specified
-- Firewall Manager administrator can apply policies to. For more
-- information about OUs in Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_ous.html Managing organizational units (OUs)>
-- in the /Organizations User Guide/.
--
-- 'policyTypeScope', 'adminScope_policyTypeScope' - Defines the Firewall Manager policy types that the specified Firewall
-- Manager administrator can create and manage.
--
-- 'regionScope', 'adminScope_regionScope' - Defines the Amazon Web Services Regions that the specified Firewall
-- Manager administrator can perform actions in.
newAdminScope ::
  AdminScope
newAdminScope =
  AdminScope'
    { accountScope = Prelude.Nothing,
      organizationalUnitScope = Prelude.Nothing,
      policyTypeScope = Prelude.Nothing,
      regionScope = Prelude.Nothing
    }

-- | Defines the accounts that the specified Firewall Manager administrator
-- can apply policies to.
adminScope_accountScope :: Lens.Lens' AdminScope (Prelude.Maybe AccountScope)
adminScope_accountScope = Lens.lens (\AdminScope' {accountScope} -> accountScope) (\s@AdminScope' {} a -> s {accountScope = a} :: AdminScope)

-- | Defines the Organizations organizational units that the specified
-- Firewall Manager administrator can apply policies to. For more
-- information about OUs in Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_ous.html Managing organizational units (OUs)>
-- in the /Organizations User Guide/.
adminScope_organizationalUnitScope :: Lens.Lens' AdminScope (Prelude.Maybe OrganizationalUnitScope)
adminScope_organizationalUnitScope = Lens.lens (\AdminScope' {organizationalUnitScope} -> organizationalUnitScope) (\s@AdminScope' {} a -> s {organizationalUnitScope = a} :: AdminScope)

-- | Defines the Firewall Manager policy types that the specified Firewall
-- Manager administrator can create and manage.
adminScope_policyTypeScope :: Lens.Lens' AdminScope (Prelude.Maybe PolicyTypeScope)
adminScope_policyTypeScope = Lens.lens (\AdminScope' {policyTypeScope} -> policyTypeScope) (\s@AdminScope' {} a -> s {policyTypeScope = a} :: AdminScope)

-- | Defines the Amazon Web Services Regions that the specified Firewall
-- Manager administrator can perform actions in.
adminScope_regionScope :: Lens.Lens' AdminScope (Prelude.Maybe RegionScope)
adminScope_regionScope = Lens.lens (\AdminScope' {regionScope} -> regionScope) (\s@AdminScope' {} a -> s {regionScope = a} :: AdminScope)

instance Data.FromJSON AdminScope where
  parseJSON =
    Data.withObject
      "AdminScope"
      ( \x ->
          AdminScope'
            Prelude.<$> (x Data..:? "AccountScope")
            Prelude.<*> (x Data..:? "OrganizationalUnitScope")
            Prelude.<*> (x Data..:? "PolicyTypeScope")
            Prelude.<*> (x Data..:? "RegionScope")
      )

instance Prelude.Hashable AdminScope where
  hashWithSalt _salt AdminScope' {..} =
    _salt
      `Prelude.hashWithSalt` accountScope
      `Prelude.hashWithSalt` organizationalUnitScope
      `Prelude.hashWithSalt` policyTypeScope
      `Prelude.hashWithSalt` regionScope

instance Prelude.NFData AdminScope where
  rnf AdminScope' {..} =
    Prelude.rnf accountScope
      `Prelude.seq` Prelude.rnf organizationalUnitScope
      `Prelude.seq` Prelude.rnf policyTypeScope
      `Prelude.seq` Prelude.rnf regionScope

instance Data.ToJSON AdminScope where
  toJSON AdminScope' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountScope" Data..=) Prelude.<$> accountScope,
            ("OrganizationalUnitScope" Data..=)
              Prelude.<$> organizationalUnitScope,
            ("PolicyTypeScope" Data..=)
              Prelude.<$> policyTypeScope,
            ("RegionScope" Data..=) Prelude.<$> regionScope
          ]
      )
