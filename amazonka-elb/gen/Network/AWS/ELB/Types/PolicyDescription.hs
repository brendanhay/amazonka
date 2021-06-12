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

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.PolicyAttributeDescription
import qualified Network.AWS.Lens as Lens

-- | Information about a policy.
--
-- /See:/ 'newPolicyDescription' smart constructor.
data PolicyDescription = PolicyDescription'
  { -- | The name of the policy.
    policyName :: Core.Maybe Core.Text,
    -- | The name of the policy type.
    policyTypeName :: Core.Maybe Core.Text,
    -- | The policy attributes.
    policyAttributeDescriptions :: Core.Maybe [PolicyAttributeDescription]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { policyName = Core.Nothing,
      policyTypeName = Core.Nothing,
      policyAttributeDescriptions = Core.Nothing
    }

-- | The name of the policy.
policyDescription_policyName :: Lens.Lens' PolicyDescription (Core.Maybe Core.Text)
policyDescription_policyName = Lens.lens (\PolicyDescription' {policyName} -> policyName) (\s@PolicyDescription' {} a -> s {policyName = a} :: PolicyDescription)

-- | The name of the policy type.
policyDescription_policyTypeName :: Lens.Lens' PolicyDescription (Core.Maybe Core.Text)
policyDescription_policyTypeName = Lens.lens (\PolicyDescription' {policyTypeName} -> policyTypeName) (\s@PolicyDescription' {} a -> s {policyTypeName = a} :: PolicyDescription)

-- | The policy attributes.
policyDescription_policyAttributeDescriptions :: Lens.Lens' PolicyDescription (Core.Maybe [PolicyAttributeDescription])
policyDescription_policyAttributeDescriptions = Lens.lens (\PolicyDescription' {policyAttributeDescriptions} -> policyAttributeDescriptions) (\s@PolicyDescription' {} a -> s {policyAttributeDescriptions = a} :: PolicyDescription) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML PolicyDescription where
  parseXML x =
    PolicyDescription'
      Core.<$> (x Core..@? "PolicyName")
      Core.<*> (x Core..@? "PolicyTypeName")
      Core.<*> ( x Core..@? "PolicyAttributeDescriptions"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable PolicyDescription

instance Core.NFData PolicyDescription
