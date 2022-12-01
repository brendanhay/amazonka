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
-- Module      : Amazonka.IoT.Types.PolicyVersionIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.PolicyVersionIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the version of the policy associated with the
-- resource.
--
-- /See:/ 'newPolicyVersionIdentifier' smart constructor.
data PolicyVersionIdentifier = PolicyVersionIdentifier'
  { -- | The name of the policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version of the policy associated with the resource.
    policyVersionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyVersionIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'policyVersionIdentifier_policyName' - The name of the policy.
--
-- 'policyVersionId', 'policyVersionIdentifier_policyVersionId' - The ID of the version of the policy associated with the resource.
newPolicyVersionIdentifier ::
  PolicyVersionIdentifier
newPolicyVersionIdentifier =
  PolicyVersionIdentifier'
    { policyName =
        Prelude.Nothing,
      policyVersionId = Prelude.Nothing
    }

-- | The name of the policy.
policyVersionIdentifier_policyName :: Lens.Lens' PolicyVersionIdentifier (Prelude.Maybe Prelude.Text)
policyVersionIdentifier_policyName = Lens.lens (\PolicyVersionIdentifier' {policyName} -> policyName) (\s@PolicyVersionIdentifier' {} a -> s {policyName = a} :: PolicyVersionIdentifier)

-- | The ID of the version of the policy associated with the resource.
policyVersionIdentifier_policyVersionId :: Lens.Lens' PolicyVersionIdentifier (Prelude.Maybe Prelude.Text)
policyVersionIdentifier_policyVersionId = Lens.lens (\PolicyVersionIdentifier' {policyVersionId} -> policyVersionId) (\s@PolicyVersionIdentifier' {} a -> s {policyVersionId = a} :: PolicyVersionIdentifier)

instance Core.FromJSON PolicyVersionIdentifier where
  parseJSON =
    Core.withObject
      "PolicyVersionIdentifier"
      ( \x ->
          PolicyVersionIdentifier'
            Prelude.<$> (x Core..:? "policyName")
            Prelude.<*> (x Core..:? "policyVersionId")
      )

instance Prelude.Hashable PolicyVersionIdentifier where
  hashWithSalt _salt PolicyVersionIdentifier' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyVersionId

instance Prelude.NFData PolicyVersionIdentifier where
  rnf PolicyVersionIdentifier' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyVersionId

instance Core.ToJSON PolicyVersionIdentifier where
  toJSON PolicyVersionIdentifier' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("policyName" Core..=) Prelude.<$> policyName,
            ("policyVersionId" Core..=)
              Prelude.<$> policyVersionId
          ]
      )
