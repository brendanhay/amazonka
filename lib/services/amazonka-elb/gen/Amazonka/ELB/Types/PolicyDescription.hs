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
-- Module      : Amazonka.ELB.Types.PolicyDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.PolicyDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELB.Internal
import Amazonka.ELB.Types.PolicyAttributeDescription
import qualified Amazonka.Prelude as Prelude

-- | Information about a policy.
--
-- /See:/ 'newPolicyDescription' smart constructor.
data PolicyDescription = PolicyDescription'
  { -- | The name of the policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The policy attributes.
    policyAttributeDescriptions :: Prelude.Maybe [PolicyAttributeDescription],
    -- | The name of the policy type.
    policyTypeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'policyDescription_policyName' - The name of the policy.
--
-- 'policyAttributeDescriptions', 'policyDescription_policyAttributeDescriptions' - The policy attributes.
--
-- 'policyTypeName', 'policyDescription_policyTypeName' - The name of the policy type.
newPolicyDescription ::
  PolicyDescription
newPolicyDescription =
  PolicyDescription'
    { policyName = Prelude.Nothing,
      policyAttributeDescriptions = Prelude.Nothing,
      policyTypeName = Prelude.Nothing
    }

-- | The name of the policy.
policyDescription_policyName :: Lens.Lens' PolicyDescription (Prelude.Maybe Prelude.Text)
policyDescription_policyName = Lens.lens (\PolicyDescription' {policyName} -> policyName) (\s@PolicyDescription' {} a -> s {policyName = a} :: PolicyDescription)

-- | The policy attributes.
policyDescription_policyAttributeDescriptions :: Lens.Lens' PolicyDescription (Prelude.Maybe [PolicyAttributeDescription])
policyDescription_policyAttributeDescriptions = Lens.lens (\PolicyDescription' {policyAttributeDescriptions} -> policyAttributeDescriptions) (\s@PolicyDescription' {} a -> s {policyAttributeDescriptions = a} :: PolicyDescription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the policy type.
policyDescription_policyTypeName :: Lens.Lens' PolicyDescription (Prelude.Maybe Prelude.Text)
policyDescription_policyTypeName = Lens.lens (\PolicyDescription' {policyTypeName} -> policyTypeName) (\s@PolicyDescription' {} a -> s {policyTypeName = a} :: PolicyDescription)

instance Core.FromXML PolicyDescription where
  parseXML x =
    PolicyDescription'
      Prelude.<$> (x Core..@? "PolicyName")
      Prelude.<*> ( x Core..@? "PolicyAttributeDescriptions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "PolicyTypeName")

instance Prelude.Hashable PolicyDescription where
  hashWithSalt _salt PolicyDescription' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyAttributeDescriptions
      `Prelude.hashWithSalt` policyTypeName

instance Prelude.NFData PolicyDescription where
  rnf PolicyDescription' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyAttributeDescriptions
      `Prelude.seq` Prelude.rnf policyTypeName
