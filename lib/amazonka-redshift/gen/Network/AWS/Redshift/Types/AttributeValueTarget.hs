{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.AttributeValueTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AttributeValueTarget
  ( AttributeValueTarget (..),

    -- * Smart constructor
    mkAttributeValueTarget,

    -- * Lenses
    avtAttributeValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes an attribute value.
--
-- /See:/ 'mkAttributeValueTarget' smart constructor.
newtype AttributeValueTarget = AttributeValueTarget'
  { -- | The value of the attribute.
    attributeValue :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttributeValueTarget' value with any optional fields omitted.
mkAttributeValueTarget ::
  AttributeValueTarget
mkAttributeValueTarget =
  AttributeValueTarget' {attributeValue = Core.Nothing}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avtAttributeValue :: Lens.Lens' AttributeValueTarget (Core.Maybe Types.String)
avtAttributeValue = Lens.field @"attributeValue"
{-# DEPRECATED avtAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

instance Core.FromXML AttributeValueTarget where
  parseXML x =
    AttributeValueTarget' Core.<$> (x Core..@? "AttributeValue")
