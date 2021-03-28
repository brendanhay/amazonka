{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.AttributeValueTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.AttributeValueTarget
  ( AttributeValueTarget (..)
  -- * Smart constructor
  , mkAttributeValueTarget
  -- * Lenses
  , avtAttributeValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Describes an attribute value.
--
-- /See:/ 'mkAttributeValueTarget' smart constructor.
newtype AttributeValueTarget = AttributeValueTarget'
  { attributeValue :: Core.Maybe Core.Text
    -- ^ The value of the attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttributeValueTarget' value with any optional fields omitted.
mkAttributeValueTarget
    :: AttributeValueTarget
mkAttributeValueTarget
  = AttributeValueTarget'{attributeValue = Core.Nothing}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avtAttributeValue :: Lens.Lens' AttributeValueTarget (Core.Maybe Core.Text)
avtAttributeValue = Lens.field @"attributeValue"
{-# INLINEABLE avtAttributeValue #-}
{-# DEPRECATED attributeValue "Use generic-lens or generic-optics with 'attributeValue' instead"  #-}

instance Core.FromXML AttributeValueTarget where
        parseXML x
          = AttributeValueTarget' Core.<$> (x Core..@? "AttributeValue")
