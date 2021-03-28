{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SDB.Types.Attribute
  ( Attribute (..)
  -- * Smart constructor
  , mkAttribute
  -- * Lenses
  , aName
  , aValue
  , aAlternateNameEncoding
  , aAlternateValueEncoding
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | 
--
-- /See:/ 'mkAttribute' smart constructor.
data Attribute = Attribute'
  { name :: Core.Text
    -- ^ The name of the attribute.
  , value :: Core.Text
    -- ^ The value of the attribute.
  , alternateNameEncoding :: Core.Maybe Core.Text
    -- ^ 
  , alternateValueEncoding :: Core.Maybe Core.Text
    -- ^ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Attribute' value with any optional fields omitted.
mkAttribute
    :: Core.Text -- ^ 'name'
    -> Core.Text -- ^ 'value'
    -> Attribute
mkAttribute name value
  = Attribute'{name, value, alternateNameEncoding = Core.Nothing,
               alternateValueEncoding = Core.Nothing}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Attribute Core.Text
aName = Lens.field @"name"
{-# INLINEABLE aName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValue :: Lens.Lens' Attribute Core.Text
aValue = Lens.field @"value"
{-# INLINEABLE aValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'alternateNameEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlternateNameEncoding :: Lens.Lens' Attribute (Core.Maybe Core.Text)
aAlternateNameEncoding = Lens.field @"alternateNameEncoding"
{-# INLINEABLE aAlternateNameEncoding #-}
{-# DEPRECATED alternateNameEncoding "Use generic-lens or generic-optics with 'alternateNameEncoding' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'alternateValueEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlternateValueEncoding :: Lens.Lens' Attribute (Core.Maybe Core.Text)
aAlternateValueEncoding = Lens.field @"alternateValueEncoding"
{-# INLINEABLE aAlternateValueEncoding #-}
{-# DEPRECATED alternateValueEncoding "Use generic-lens or generic-optics with 'alternateValueEncoding' instead"  #-}

instance Core.ToQuery Attribute where
        toQuery Attribute{..}
          = Core.toQueryPair "Name" name Core.<>
              Core.toQueryPair "Value" value
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AlternateNameEncoding")
                alternateNameEncoding
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AlternateValueEncoding")
                alternateValueEncoding

instance Core.FromXML Attribute where
        parseXML x
          = Attribute' Core.<$>
              (x Core..@ "Name") Core.<*> x Core..@ "Value" Core.<*>
                x Core..@? "AlternateNameEncoding"
                Core.<*> x Core..@? "AlternateValueEncoding"
