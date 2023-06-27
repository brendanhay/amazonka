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
-- Module      : Amazonka.FMS.Types.OrganizationalUnitScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.OrganizationalUnitScope where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the Organizations organizational units (OUs) that the specified
-- Firewall Manager administrator can apply policies to. For more
-- information about OUs in Organizations, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_ous.html Managing organizational units (OUs)>
-- in the /Organizations User Guide/.
--
-- /See:/ 'newOrganizationalUnitScope' smart constructor.
data OrganizationalUnitScope = OrganizationalUnitScope'
  { -- | A boolean value that indicates if the administrator can apply policies
    -- to all OUs within an organization. If true, the administrator can manage
    -- all OUs within the organization. You can either enable management of all
    -- OUs through this operation, or you can specify OUs to manage in
    -- @OrganizationalUnitScope$OrganizationalUnits@. You cannot specify both.
    allOrganizationalUnitsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A boolean value that excludes the OUs in
    -- @OrganizationalUnitScope$OrganizationalUnits@ from the administrator\'s
    -- scope. If true, the Firewall Manager administrator can apply policies to
    -- all OUs in the organization except for the OUs listed in
    -- @OrganizationalUnitScope$OrganizationalUnits@. You can either specify a
    -- list of OUs to exclude by @OrganizationalUnitScope$OrganizationalUnits@,
    -- or you can enable management of all OUs by
    -- @OrganizationalUnitScope$AllOrganizationalUnitsEnabled@. You cannot
    -- specify both.
    excludeSpecifiedOrganizationalUnits :: Prelude.Maybe Prelude.Bool,
    -- | The list of OUs within the organization that the specified Firewall
    -- Manager administrator either can or cannot apply policies to, based on
    -- the value of
    -- @OrganizationalUnitScope$ExcludeSpecifiedOrganizationalUnits@. If
    -- @OrganizationalUnitScope$ExcludeSpecifiedOrganizationalUnits@ is set to
    -- @true@, then the Firewall Manager administrator can apply policies to
    -- all OUs in the organization except for the OUs in this list. If
    -- @OrganizationalUnitScope$ExcludeSpecifiedOrganizationalUnits@ is set to
    -- @false@, then the Firewall Manager administrator can only apply policies
    -- to the OUs in this list.
    organizationalUnits :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationalUnitScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allOrganizationalUnitsEnabled', 'organizationalUnitScope_allOrganizationalUnitsEnabled' - A boolean value that indicates if the administrator can apply policies
-- to all OUs within an organization. If true, the administrator can manage
-- all OUs within the organization. You can either enable management of all
-- OUs through this operation, or you can specify OUs to manage in
-- @OrganizationalUnitScope$OrganizationalUnits@. You cannot specify both.
--
-- 'excludeSpecifiedOrganizationalUnits', 'organizationalUnitScope_excludeSpecifiedOrganizationalUnits' - A boolean value that excludes the OUs in
-- @OrganizationalUnitScope$OrganizationalUnits@ from the administrator\'s
-- scope. If true, the Firewall Manager administrator can apply policies to
-- all OUs in the organization except for the OUs listed in
-- @OrganizationalUnitScope$OrganizationalUnits@. You can either specify a
-- list of OUs to exclude by @OrganizationalUnitScope$OrganizationalUnits@,
-- or you can enable management of all OUs by
-- @OrganizationalUnitScope$AllOrganizationalUnitsEnabled@. You cannot
-- specify both.
--
-- 'organizationalUnits', 'organizationalUnitScope_organizationalUnits' - The list of OUs within the organization that the specified Firewall
-- Manager administrator either can or cannot apply policies to, based on
-- the value of
-- @OrganizationalUnitScope$ExcludeSpecifiedOrganizationalUnits@. If
-- @OrganizationalUnitScope$ExcludeSpecifiedOrganizationalUnits@ is set to
-- @true@, then the Firewall Manager administrator can apply policies to
-- all OUs in the organization except for the OUs in this list. If
-- @OrganizationalUnitScope$ExcludeSpecifiedOrganizationalUnits@ is set to
-- @false@, then the Firewall Manager administrator can only apply policies
-- to the OUs in this list.
newOrganizationalUnitScope ::
  OrganizationalUnitScope
newOrganizationalUnitScope =
  OrganizationalUnitScope'
    { allOrganizationalUnitsEnabled =
        Prelude.Nothing,
      excludeSpecifiedOrganizationalUnits =
        Prelude.Nothing,
      organizationalUnits = Prelude.Nothing
    }

-- | A boolean value that indicates if the administrator can apply policies
-- to all OUs within an organization. If true, the administrator can manage
-- all OUs within the organization. You can either enable management of all
-- OUs through this operation, or you can specify OUs to manage in
-- @OrganizationalUnitScope$OrganizationalUnits@. You cannot specify both.
organizationalUnitScope_allOrganizationalUnitsEnabled :: Lens.Lens' OrganizationalUnitScope (Prelude.Maybe Prelude.Bool)
organizationalUnitScope_allOrganizationalUnitsEnabled = Lens.lens (\OrganizationalUnitScope' {allOrganizationalUnitsEnabled} -> allOrganizationalUnitsEnabled) (\s@OrganizationalUnitScope' {} a -> s {allOrganizationalUnitsEnabled = a} :: OrganizationalUnitScope)

-- | A boolean value that excludes the OUs in
-- @OrganizationalUnitScope$OrganizationalUnits@ from the administrator\'s
-- scope. If true, the Firewall Manager administrator can apply policies to
-- all OUs in the organization except for the OUs listed in
-- @OrganizationalUnitScope$OrganizationalUnits@. You can either specify a
-- list of OUs to exclude by @OrganizationalUnitScope$OrganizationalUnits@,
-- or you can enable management of all OUs by
-- @OrganizationalUnitScope$AllOrganizationalUnitsEnabled@. You cannot
-- specify both.
organizationalUnitScope_excludeSpecifiedOrganizationalUnits :: Lens.Lens' OrganizationalUnitScope (Prelude.Maybe Prelude.Bool)
organizationalUnitScope_excludeSpecifiedOrganizationalUnits = Lens.lens (\OrganizationalUnitScope' {excludeSpecifiedOrganizationalUnits} -> excludeSpecifiedOrganizationalUnits) (\s@OrganizationalUnitScope' {} a -> s {excludeSpecifiedOrganizationalUnits = a} :: OrganizationalUnitScope)

-- | The list of OUs within the organization that the specified Firewall
-- Manager administrator either can or cannot apply policies to, based on
-- the value of
-- @OrganizationalUnitScope$ExcludeSpecifiedOrganizationalUnits@. If
-- @OrganizationalUnitScope$ExcludeSpecifiedOrganizationalUnits@ is set to
-- @true@, then the Firewall Manager administrator can apply policies to
-- all OUs in the organization except for the OUs in this list. If
-- @OrganizationalUnitScope$ExcludeSpecifiedOrganizationalUnits@ is set to
-- @false@, then the Firewall Manager administrator can only apply policies
-- to the OUs in this list.
organizationalUnitScope_organizationalUnits :: Lens.Lens' OrganizationalUnitScope (Prelude.Maybe [Prelude.Text])
organizationalUnitScope_organizationalUnits = Lens.lens (\OrganizationalUnitScope' {organizationalUnits} -> organizationalUnits) (\s@OrganizationalUnitScope' {} a -> s {organizationalUnits = a} :: OrganizationalUnitScope) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON OrganizationalUnitScope where
  parseJSON =
    Data.withObject
      "OrganizationalUnitScope"
      ( \x ->
          OrganizationalUnitScope'
            Prelude.<$> (x Data..:? "AllOrganizationalUnitsEnabled")
            Prelude.<*> (x Data..:? "ExcludeSpecifiedOrganizationalUnits")
            Prelude.<*> ( x
                            Data..:? "OrganizationalUnits"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable OrganizationalUnitScope where
  hashWithSalt _salt OrganizationalUnitScope' {..} =
    _salt
      `Prelude.hashWithSalt` allOrganizationalUnitsEnabled
      `Prelude.hashWithSalt` excludeSpecifiedOrganizationalUnits
      `Prelude.hashWithSalt` organizationalUnits

instance Prelude.NFData OrganizationalUnitScope where
  rnf OrganizationalUnitScope' {..} =
    Prelude.rnf allOrganizationalUnitsEnabled
      `Prelude.seq` Prelude.rnf excludeSpecifiedOrganizationalUnits
      `Prelude.seq` Prelude.rnf organizationalUnits

instance Data.ToJSON OrganizationalUnitScope where
  toJSON OrganizationalUnitScope' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllOrganizationalUnitsEnabled" Data..=)
              Prelude.<$> allOrganizationalUnitsEnabled,
            ("ExcludeSpecifiedOrganizationalUnits" Data..=)
              Prelude.<$> excludeSpecifiedOrganizationalUnits,
            ("OrganizationalUnits" Data..=)
              Prelude.<$> organizationalUnits
          ]
      )
