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
-- Module      : Network.AWS.ELB.Types.PolicyDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyDescription where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.PolicyAttributeDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a policy.
--
-- /See:/ 'newPolicyDescription' smart constructor.
data PolicyDescription = PolicyDescription'
  { -- | The name of the policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy type.
    policyTypeName :: Prelude.Maybe Prelude.Text,
    -- | The policy attributes.
    policyAttributeDescriptions :: Prelude.Maybe [PolicyAttributeDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'policyTypeName', 'policyDescription_policyTypeName' - The name of the policy type.
--
-- 'policyAttributeDescriptions', 'policyDescription_policyAttributeDescriptions' - The policy attributes.
newPolicyDescription ::
  PolicyDescription
newPolicyDescription =
  PolicyDescription'
    { policyName = Prelude.Nothing,
      policyTypeName = Prelude.Nothing,
      policyAttributeDescriptions = Prelude.Nothing
    }

-- | The name of the policy.
policyDescription_policyName :: Lens.Lens' PolicyDescription (Prelude.Maybe Prelude.Text)
policyDescription_policyName = Lens.lens (\PolicyDescription' {policyName} -> policyName) (\s@PolicyDescription' {} a -> s {policyName = a} :: PolicyDescription)

-- | The name of the policy type.
policyDescription_policyTypeName :: Lens.Lens' PolicyDescription (Prelude.Maybe Prelude.Text)
policyDescription_policyTypeName = Lens.lens (\PolicyDescription' {policyTypeName} -> policyTypeName) (\s@PolicyDescription' {} a -> s {policyTypeName = a} :: PolicyDescription)

-- | The policy attributes.
policyDescription_policyAttributeDescriptions :: Lens.Lens' PolicyDescription (Prelude.Maybe [PolicyAttributeDescription])
policyDescription_policyAttributeDescriptions = Lens.lens (\PolicyDescription' {policyAttributeDescriptions} -> policyAttributeDescriptions) (\s@PolicyDescription' {} a -> s {policyAttributeDescriptions = a} :: PolicyDescription) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML PolicyDescription where
  parseXML x =
    PolicyDescription'
      Prelude.<$> (x Prelude..@? "PolicyName")
      Prelude.<*> (x Prelude..@? "PolicyTypeName")
      Prelude.<*> ( x Prelude..@? "PolicyAttributeDescriptions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance Prelude.Hashable PolicyDescription

instance Prelude.NFData PolicyDescription
