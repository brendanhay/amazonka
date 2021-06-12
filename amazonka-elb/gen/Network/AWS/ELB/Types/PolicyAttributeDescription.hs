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
-- Module      : Network.AWS.ELB.Types.PolicyAttributeDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyAttributeDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about a policy attribute.
--
-- /See:/ 'newPolicyAttributeDescription' smart constructor.
data PolicyAttributeDescription = PolicyAttributeDescription'
  { -- | The value of the attribute.
    attributeValue :: Core.Maybe Core.Text,
    -- | The name of the attribute.
    attributeName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PolicyAttributeDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValue', 'policyAttributeDescription_attributeValue' - The value of the attribute.
--
-- 'attributeName', 'policyAttributeDescription_attributeName' - The name of the attribute.
newPolicyAttributeDescription ::
  PolicyAttributeDescription
newPolicyAttributeDescription =
  PolicyAttributeDescription'
    { attributeValue =
        Core.Nothing,
      attributeName = Core.Nothing
    }

-- | The value of the attribute.
policyAttributeDescription_attributeValue :: Lens.Lens' PolicyAttributeDescription (Core.Maybe Core.Text)
policyAttributeDescription_attributeValue = Lens.lens (\PolicyAttributeDescription' {attributeValue} -> attributeValue) (\s@PolicyAttributeDescription' {} a -> s {attributeValue = a} :: PolicyAttributeDescription)

-- | The name of the attribute.
policyAttributeDescription_attributeName :: Lens.Lens' PolicyAttributeDescription (Core.Maybe Core.Text)
policyAttributeDescription_attributeName = Lens.lens (\PolicyAttributeDescription' {attributeName} -> attributeName) (\s@PolicyAttributeDescription' {} a -> s {attributeName = a} :: PolicyAttributeDescription)

instance Core.FromXML PolicyAttributeDescription where
  parseXML x =
    PolicyAttributeDescription'
      Core.<$> (x Core..@? "AttributeValue")
      Core.<*> (x Core..@? "AttributeName")

instance Core.Hashable PolicyAttributeDescription

instance Core.NFData PolicyAttributeDescription
