{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.ReplaceableAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.ReplaceableAttribute
  ( ReplaceableAttribute (..),

    -- * Smart constructor
    mkReplaceableAttribute,

    -- * Lenses
    raName,
    raValue,
    raReplace,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SDB.Types.String as Types

-- |
--
-- /See:/ 'mkReplaceableAttribute' smart constructor.
data ReplaceableAttribute = ReplaceableAttribute'
  { -- | The name of the replaceable attribute.
    name :: Types.String,
    -- | The value of the replaceable attribute.
    value :: Types.String,
    -- | @false@
    replace :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceableAttribute' value with any optional fields omitted.
mkReplaceableAttribute ::
  -- | 'name'
  Types.String ->
  -- | 'value'
  Types.String ->
  ReplaceableAttribute
mkReplaceableAttribute name value =
  ReplaceableAttribute' {name, value, replace = Core.Nothing}

-- | The name of the replaceable attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raName :: Lens.Lens' ReplaceableAttribute Types.String
raName = Lens.field @"name"
{-# DEPRECATED raName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the replaceable attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raValue :: Lens.Lens' ReplaceableAttribute Types.String
raValue = Lens.field @"value"
{-# DEPRECATED raValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | @false@
--
-- /Note:/ Consider using 'replace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raReplace :: Lens.Lens' ReplaceableAttribute (Core.Maybe Core.Bool)
raReplace = Lens.field @"replace"
{-# DEPRECATED raReplace "Use generic-lens or generic-optics with 'replace' instead." #-}
