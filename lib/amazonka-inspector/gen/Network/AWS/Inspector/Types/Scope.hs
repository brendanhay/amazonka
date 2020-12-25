{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Scope
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Scope
  ( Scope (..),

    -- * Smart constructor
    mkScope,

    -- * Lenses
    sKey,
    sValue,
  )
where

import qualified Network.AWS.Inspector.Types.ScopeType as Types
import qualified Network.AWS.Inspector.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type contains key-value pairs that identify various Amazon resources.
--
-- /See:/ 'mkScope' smart constructor.
data Scope = Scope'
  { -- | The type of the scope.
    key :: Core.Maybe Types.ScopeType,
    -- | The resource identifier for the specified scope type.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Scope' value with any optional fields omitted.
mkScope ::
  Scope
mkScope = Scope' {key = Core.Nothing, value = Core.Nothing}

-- | The type of the scope.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKey :: Lens.Lens' Scope (Core.Maybe Types.ScopeType)
sKey = Lens.field @"key"
{-# DEPRECATED sKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The resource identifier for the specified scope type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sValue :: Lens.Lens' Scope (Core.Maybe Types.Value)
sValue = Lens.field @"value"
{-# DEPRECATED sValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Scope where
  parseJSON =
    Core.withObject "Scope" Core.$
      \x ->
        Scope' Core.<$> (x Core..:? "key") Core.<*> (x Core..:? "value")
