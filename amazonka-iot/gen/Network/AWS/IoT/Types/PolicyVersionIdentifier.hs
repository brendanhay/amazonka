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
-- Module      : Network.AWS.IoT.Types.PolicyVersionIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PolicyVersionIdentifier where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the version of the policy associated with the
-- resource.
--
-- /See:/ 'newPolicyVersionIdentifier' smart constructor.
data PolicyVersionIdentifier = PolicyVersionIdentifier'
  { -- | The ID of the version of the policy associated with the resource.
    policyVersionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy.
    policyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PolicyVersionIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyVersionId', 'policyVersionIdentifier_policyVersionId' - The ID of the version of the policy associated with the resource.
--
-- 'policyName', 'policyVersionIdentifier_policyName' - The name of the policy.
newPolicyVersionIdentifier ::
  PolicyVersionIdentifier
newPolicyVersionIdentifier =
  PolicyVersionIdentifier'
    { policyVersionId =
        Prelude.Nothing,
      policyName = Prelude.Nothing
    }

-- | The ID of the version of the policy associated with the resource.
policyVersionIdentifier_policyVersionId :: Lens.Lens' PolicyVersionIdentifier (Prelude.Maybe Prelude.Text)
policyVersionIdentifier_policyVersionId = Lens.lens (\PolicyVersionIdentifier' {policyVersionId} -> policyVersionId) (\s@PolicyVersionIdentifier' {} a -> s {policyVersionId = a} :: PolicyVersionIdentifier)

-- | The name of the policy.
policyVersionIdentifier_policyName :: Lens.Lens' PolicyVersionIdentifier (Prelude.Maybe Prelude.Text)
policyVersionIdentifier_policyName = Lens.lens (\PolicyVersionIdentifier' {policyName} -> policyName) (\s@PolicyVersionIdentifier' {} a -> s {policyName = a} :: PolicyVersionIdentifier)

instance Prelude.FromJSON PolicyVersionIdentifier where
  parseJSON =
    Prelude.withObject
      "PolicyVersionIdentifier"
      ( \x ->
          PolicyVersionIdentifier'
            Prelude.<$> (x Prelude..:? "policyVersionId")
            Prelude.<*> (x Prelude..:? "policyName")
      )

instance Prelude.Hashable PolicyVersionIdentifier

instance Prelude.NFData PolicyVersionIdentifier

instance Prelude.ToJSON PolicyVersionIdentifier where
  toJSON PolicyVersionIdentifier' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("policyVersionId" Prelude..=)
              Prelude.<$> policyVersionId,
            ("policyName" Prelude..=) Prelude.<$> policyName
          ]
      )
