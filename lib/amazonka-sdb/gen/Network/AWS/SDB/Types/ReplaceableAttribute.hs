{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.ReplaceableAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SDB.Types.ReplaceableAttribute
  ( ReplaceableAttribute (..)
  -- * Smart constructor
  , mkReplaceableAttribute
  -- * Lenses
  , raName
  , raValue
  , raReplace
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | 
--
-- /See:/ 'mkReplaceableAttribute' smart constructor.
data ReplaceableAttribute = ReplaceableAttribute'
  { name :: Core.Text
    -- ^ The name of the replaceable attribute.
  , value :: Core.Text
    -- ^ The value of the replaceable attribute.
  , replace :: Core.Maybe Core.Bool
    -- ^ @false@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceableAttribute' value with any optional fields omitted.
mkReplaceableAttribute
    :: Core.Text -- ^ 'name'
    -> Core.Text -- ^ 'value'
    -> ReplaceableAttribute
mkReplaceableAttribute name value
  = ReplaceableAttribute'{name, value, replace = Core.Nothing}

-- | The name of the replaceable attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raName :: Lens.Lens' ReplaceableAttribute Core.Text
raName = Lens.field @"name"
{-# INLINEABLE raName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value of the replaceable attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raValue :: Lens.Lens' ReplaceableAttribute Core.Text
raValue = Lens.field @"value"
{-# INLINEABLE raValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | @false@ 
--
-- /Note:/ Consider using 'replace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raReplace :: Lens.Lens' ReplaceableAttribute (Core.Maybe Core.Bool)
raReplace = Lens.field @"replace"
{-# INLINEABLE raReplace #-}
{-# DEPRECATED replace "Use generic-lens or generic-optics with 'replace' instead"  #-}

instance Core.ToQuery ReplaceableAttribute where
        toQuery ReplaceableAttribute{..}
          = Core.toQueryPair "Name" name Core.<>
              Core.toQueryPair "Value" value
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Replace") replace
