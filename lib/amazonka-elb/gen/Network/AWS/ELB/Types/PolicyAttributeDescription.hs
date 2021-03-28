{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyAttributeDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types.PolicyAttributeDescription
  ( PolicyAttributeDescription (..)
  -- * Smart constructor
  , mkPolicyAttributeDescription
  -- * Lenses
  , padAttributeName
  , padAttributeValue
  ) where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.AttributeName as Types
import qualified Network.AWS.ELB.Types.AttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a policy attribute.
--
-- /See:/ 'mkPolicyAttributeDescription' smart constructor.
data PolicyAttributeDescription = PolicyAttributeDescription'
  { attributeName :: Core.Maybe Types.AttributeName
    -- ^ The name of the attribute.
  , attributeValue :: Core.Maybe Types.AttributeValue
    -- ^ The value of the attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyAttributeDescription' value with any optional fields omitted.
mkPolicyAttributeDescription
    :: PolicyAttributeDescription
mkPolicyAttributeDescription
  = PolicyAttributeDescription'{attributeName = Core.Nothing,
                                attributeValue = Core.Nothing}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padAttributeName :: Lens.Lens' PolicyAttributeDescription (Core.Maybe Types.AttributeName)
padAttributeName = Lens.field @"attributeName"
{-# INLINEABLE padAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padAttributeValue :: Lens.Lens' PolicyAttributeDescription (Core.Maybe Types.AttributeValue)
padAttributeValue = Lens.field @"attributeValue"
{-# INLINEABLE padAttributeValue #-}
{-# DEPRECATED attributeValue "Use generic-lens or generic-optics with 'attributeValue' instead"  #-}

instance Core.FromXML PolicyAttributeDescription where
        parseXML x
          = PolicyAttributeDescription' Core.<$>
              (x Core..@? "AttributeName") Core.<*> x Core..@? "AttributeValue"
