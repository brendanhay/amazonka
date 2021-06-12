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
-- Module      : Network.AWS.ELB.Types.PolicyAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyAttribute where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about a policy attribute.
--
-- /See:/ 'newPolicyAttribute' smart constructor.
data PolicyAttribute = PolicyAttribute'
  { -- | The value of the attribute.
    attributeValue :: Core.Maybe Core.Text,
    -- | The name of the attribute.
    attributeName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PolicyAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValue', 'policyAttribute_attributeValue' - The value of the attribute.
--
-- 'attributeName', 'policyAttribute_attributeName' - The name of the attribute.
newPolicyAttribute ::
  PolicyAttribute
newPolicyAttribute =
  PolicyAttribute'
    { attributeValue = Core.Nothing,
      attributeName = Core.Nothing
    }

-- | The value of the attribute.
policyAttribute_attributeValue :: Lens.Lens' PolicyAttribute (Core.Maybe Core.Text)
policyAttribute_attributeValue = Lens.lens (\PolicyAttribute' {attributeValue} -> attributeValue) (\s@PolicyAttribute' {} a -> s {attributeValue = a} :: PolicyAttribute)

-- | The name of the attribute.
policyAttribute_attributeName :: Lens.Lens' PolicyAttribute (Core.Maybe Core.Text)
policyAttribute_attributeName = Lens.lens (\PolicyAttribute' {attributeName} -> attributeName) (\s@PolicyAttribute' {} a -> s {attributeName = a} :: PolicyAttribute)

instance Core.Hashable PolicyAttribute

instance Core.NFData PolicyAttribute

instance Core.ToQuery PolicyAttribute where
  toQuery PolicyAttribute' {..} =
    Core.mconcat
      [ "AttributeValue" Core.=: attributeValue,
        "AttributeName" Core.=: attributeName
      ]
