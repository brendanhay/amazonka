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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.PolicyDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Internal
import Amazonka.ELB.Types.PolicyAttributeDescription
import qualified Amazonka.Prelude as Prelude

-- | Information about a policy.
--
-- /See:/ 'newPolicyDescription' smart constructor.
data PolicyDescription = PolicyDescription'
  { -- | The policy attributes.
    policyAttributeDescriptions :: Prelude.Maybe [PolicyAttributeDescription],
    -- | The name of the policy.
    policyName :: Prelude.Maybe Prelude.Text,
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
-- 'policyAttributeDescriptions', 'policyDescription_policyAttributeDescriptions' - The policy attributes.
--
-- 'policyName', 'policyDescription_policyName' - The name of the policy.
--
-- 'policyTypeName', 'policyDescription_policyTypeName' - The name of the policy type.
newPolicyDescription ::
  PolicyDescription
newPolicyDescription =
  PolicyDescription'
    { policyAttributeDescriptions =
        Prelude.Nothing,
      policyName = Prelude.Nothing,
      policyTypeName = Prelude.Nothing
    }

-- | The policy attributes.
policyDescription_policyAttributeDescriptions :: Lens.Lens' PolicyDescription (Prelude.Maybe [PolicyAttributeDescription])
policyDescription_policyAttributeDescriptions = Lens.lens (\PolicyDescription' {policyAttributeDescriptions} -> policyAttributeDescriptions) (\s@PolicyDescription' {} a -> s {policyAttributeDescriptions = a} :: PolicyDescription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the policy.
policyDescription_policyName :: Lens.Lens' PolicyDescription (Prelude.Maybe Prelude.Text)
policyDescription_policyName = Lens.lens (\PolicyDescription' {policyName} -> policyName) (\s@PolicyDescription' {} a -> s {policyName = a} :: PolicyDescription)

-- | The name of the policy type.
policyDescription_policyTypeName :: Lens.Lens' PolicyDescription (Prelude.Maybe Prelude.Text)
policyDescription_policyTypeName = Lens.lens (\PolicyDescription' {policyTypeName} -> policyTypeName) (\s@PolicyDescription' {} a -> s {policyTypeName = a} :: PolicyDescription)

instance Data.FromXML PolicyDescription where
  parseXML x =
    PolicyDescription'
      Prelude.<$> ( x
                      Data..@? "PolicyAttributeDescriptions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "PolicyName")
      Prelude.<*> (x Data..@? "PolicyTypeName")

instance Prelude.Hashable PolicyDescription where
  hashWithSalt _salt PolicyDescription' {..} =
    _salt
      `Prelude.hashWithSalt` policyAttributeDescriptions
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyTypeName

instance Prelude.NFData PolicyDescription where
  rnf PolicyDescription' {..} =
    Prelude.rnf policyAttributeDescriptions
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyTypeName
