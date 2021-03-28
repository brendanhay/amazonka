{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AttributeType
  ( AttributeType (..)
  -- * Smart constructor
  , mkAttributeType
  -- * Lenses
  , atName
  , atValue
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.AttributeNameType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies whether the attribute is standard or custom.
--
-- /See:/ 'mkAttributeType' smart constructor.
data AttributeType = AttributeType'
  { name :: Types.AttributeNameType
    -- ^ The name of the attribute.
  , value :: Core.Maybe Types.Value
    -- ^ The value of the attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttributeType' value with any optional fields omitted.
mkAttributeType
    :: Types.AttributeNameType -- ^ 'name'
    -> AttributeType
mkAttributeType name = AttributeType'{name, value = Core.Nothing}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atName :: Lens.Lens' AttributeType Types.AttributeNameType
atName = Lens.field @"name"
{-# INLINEABLE atName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atValue :: Lens.Lens' AttributeType (Core.Maybe Types.Value)
atValue = Lens.field @"value"
{-# INLINEABLE atValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON AttributeType where
        toJSON AttributeType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("Value" Core..=) Core.<$> value])

instance Core.FromJSON AttributeType where
        parseJSON
          = Core.withObject "AttributeType" Core.$
              \ x ->
                AttributeType' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..:? "Value"
