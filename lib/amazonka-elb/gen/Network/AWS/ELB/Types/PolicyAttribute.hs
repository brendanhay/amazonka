{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyAttribute
  ( PolicyAttribute (..),

    -- * Smart constructor
    mkPolicyAttribute,

    -- * Lenses
    paAttributeName,
    paAttributeValue,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.AttributeName as Types
import qualified Network.AWS.ELB.Types.AttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a policy attribute.
--
-- /See:/ 'mkPolicyAttribute' smart constructor.
data PolicyAttribute = PolicyAttribute'
  { -- | The name of the attribute.
    attributeName :: Core.Maybe Types.AttributeName,
    -- | The value of the attribute.
    attributeValue :: Core.Maybe Types.AttributeValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyAttribute' value with any optional fields omitted.
mkPolicyAttribute ::
  PolicyAttribute
mkPolicyAttribute =
  PolicyAttribute'
    { attributeName = Core.Nothing,
      attributeValue = Core.Nothing
    }

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAttributeName :: Lens.Lens' PolicyAttribute (Core.Maybe Types.AttributeName)
paAttributeName = Lens.field @"attributeName"
{-# DEPRECATED paAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAttributeValue :: Lens.Lens' PolicyAttribute (Core.Maybe Types.AttributeValue)
paAttributeValue = Lens.field @"attributeValue"
{-# DEPRECATED paAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}
