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
-- Module      : Amazonka.FMS.Types.PolicyTypeScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.PolicyTypeScope where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.SecurityServiceType
import qualified Amazonka.Prelude as Prelude

-- | Defines the policy types that the specified Firewall Manager
-- administrator can manage.
--
-- /See:/ 'newPolicyTypeScope' smart constructor.
data PolicyTypeScope = PolicyTypeScope'
  { -- | Allows the specified Firewall Manager administrator to manage all
    -- Firewall Manager policy types, except for third-party policy types.
    -- Third-party policy types can only be managed by the Firewall Manager
    -- default administrator.
    allPolicyTypesEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The list of policy types that the specified Firewall Manager
    -- administrator can manage.
    policyTypes :: Prelude.Maybe [SecurityServiceType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyTypeScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allPolicyTypesEnabled', 'policyTypeScope_allPolicyTypesEnabled' - Allows the specified Firewall Manager administrator to manage all
-- Firewall Manager policy types, except for third-party policy types.
-- Third-party policy types can only be managed by the Firewall Manager
-- default administrator.
--
-- 'policyTypes', 'policyTypeScope_policyTypes' - The list of policy types that the specified Firewall Manager
-- administrator can manage.
newPolicyTypeScope ::
  PolicyTypeScope
newPolicyTypeScope =
  PolicyTypeScope'
    { allPolicyTypesEnabled =
        Prelude.Nothing,
      policyTypes = Prelude.Nothing
    }

-- | Allows the specified Firewall Manager administrator to manage all
-- Firewall Manager policy types, except for third-party policy types.
-- Third-party policy types can only be managed by the Firewall Manager
-- default administrator.
policyTypeScope_allPolicyTypesEnabled :: Lens.Lens' PolicyTypeScope (Prelude.Maybe Prelude.Bool)
policyTypeScope_allPolicyTypesEnabled = Lens.lens (\PolicyTypeScope' {allPolicyTypesEnabled} -> allPolicyTypesEnabled) (\s@PolicyTypeScope' {} a -> s {allPolicyTypesEnabled = a} :: PolicyTypeScope)

-- | The list of policy types that the specified Firewall Manager
-- administrator can manage.
policyTypeScope_policyTypes :: Lens.Lens' PolicyTypeScope (Prelude.Maybe [SecurityServiceType])
policyTypeScope_policyTypes = Lens.lens (\PolicyTypeScope' {policyTypes} -> policyTypes) (\s@PolicyTypeScope' {} a -> s {policyTypes = a} :: PolicyTypeScope) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PolicyTypeScope where
  parseJSON =
    Data.withObject
      "PolicyTypeScope"
      ( \x ->
          PolicyTypeScope'
            Prelude.<$> (x Data..:? "AllPolicyTypesEnabled")
            Prelude.<*> (x Data..:? "PolicyTypes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PolicyTypeScope where
  hashWithSalt _salt PolicyTypeScope' {..} =
    _salt
      `Prelude.hashWithSalt` allPolicyTypesEnabled
      `Prelude.hashWithSalt` policyTypes

instance Prelude.NFData PolicyTypeScope where
  rnf PolicyTypeScope' {..} =
    Prelude.rnf allPolicyTypesEnabled
      `Prelude.seq` Prelude.rnf policyTypes

instance Data.ToJSON PolicyTypeScope where
  toJSON PolicyTypeScope' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllPolicyTypesEnabled" Data..=)
              Prelude.<$> allPolicyTypesEnabled,
            ("PolicyTypes" Data..=) Prelude.<$> policyTypes
          ]
      )
