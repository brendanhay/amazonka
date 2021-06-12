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
-- Module      : Network.AWS.ELB.Types.PolicyTypeDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyTypeDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.PolicyAttributeTypeDescription
import qualified Network.AWS.Lens as Lens

-- | Information about a policy type.
--
-- /See:/ 'newPolicyTypeDescription' smart constructor.
data PolicyTypeDescription = PolicyTypeDescription'
  { -- | The description of the policy attributes associated with the policies
    -- defined by Elastic Load Balancing.
    policyAttributeTypeDescriptions :: Core.Maybe [PolicyAttributeTypeDescription],
    -- | The name of the policy type.
    policyTypeName :: Core.Maybe Core.Text,
    -- | A description of the policy type.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PolicyTypeDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyAttributeTypeDescriptions', 'policyTypeDescription_policyAttributeTypeDescriptions' - The description of the policy attributes associated with the policies
-- defined by Elastic Load Balancing.
--
-- 'policyTypeName', 'policyTypeDescription_policyTypeName' - The name of the policy type.
--
-- 'description', 'policyTypeDescription_description' - A description of the policy type.
newPolicyTypeDescription ::
  PolicyTypeDescription
newPolicyTypeDescription =
  PolicyTypeDescription'
    { policyAttributeTypeDescriptions =
        Core.Nothing,
      policyTypeName = Core.Nothing,
      description = Core.Nothing
    }

-- | The description of the policy attributes associated with the policies
-- defined by Elastic Load Balancing.
policyTypeDescription_policyAttributeTypeDescriptions :: Lens.Lens' PolicyTypeDescription (Core.Maybe [PolicyAttributeTypeDescription])
policyTypeDescription_policyAttributeTypeDescriptions = Lens.lens (\PolicyTypeDescription' {policyAttributeTypeDescriptions} -> policyAttributeTypeDescriptions) (\s@PolicyTypeDescription' {} a -> s {policyAttributeTypeDescriptions = a} :: PolicyTypeDescription) Core.. Lens.mapping Lens._Coerce

-- | The name of the policy type.
policyTypeDescription_policyTypeName :: Lens.Lens' PolicyTypeDescription (Core.Maybe Core.Text)
policyTypeDescription_policyTypeName = Lens.lens (\PolicyTypeDescription' {policyTypeName} -> policyTypeName) (\s@PolicyTypeDescription' {} a -> s {policyTypeName = a} :: PolicyTypeDescription)

-- | A description of the policy type.
policyTypeDescription_description :: Lens.Lens' PolicyTypeDescription (Core.Maybe Core.Text)
policyTypeDescription_description = Lens.lens (\PolicyTypeDescription' {description} -> description) (\s@PolicyTypeDescription' {} a -> s {description = a} :: PolicyTypeDescription)

instance Core.FromXML PolicyTypeDescription where
  parseXML x =
    PolicyTypeDescription'
      Core.<$> ( x Core..@? "PolicyAttributeTypeDescriptions"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "PolicyTypeName")
      Core.<*> (x Core..@? "Description")

instance Core.Hashable PolicyTypeDescription

instance Core.NFData PolicyTypeDescription
