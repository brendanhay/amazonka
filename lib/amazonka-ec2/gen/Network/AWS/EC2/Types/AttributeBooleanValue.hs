{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AttributeBooleanValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AttributeBooleanValue
  ( AttributeBooleanValue (..),

    -- * Smart constructor
    mkAttributeBooleanValue,

    -- * Lenses
    abvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a value for a resource attribute that is a Boolean value.
--
-- /See:/ 'mkAttributeBooleanValue' smart constructor.
newtype AttributeBooleanValue = AttributeBooleanValue'
  { -- | The attribute value. The valid values are @true@ or @false@ .
    value :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttributeBooleanValue' value with any optional fields omitted.
mkAttributeBooleanValue ::
  AttributeBooleanValue
mkAttributeBooleanValue =
  AttributeBooleanValue' {value = Core.Nothing}

-- | The attribute value. The valid values are @true@ or @false@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abvValue :: Lens.Lens' AttributeBooleanValue (Core.Maybe Core.Bool)
abvValue = Lens.field @"value"
{-# DEPRECATED abvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML AttributeBooleanValue where
  parseXML x = AttributeBooleanValue' Core.<$> (x Core..@? "value")
