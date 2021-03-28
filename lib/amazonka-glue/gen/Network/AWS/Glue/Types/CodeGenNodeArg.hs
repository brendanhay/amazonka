{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CodeGenNodeArg
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.CodeGenNodeArg
  ( CodeGenNodeArg (..)
  -- * Smart constructor
  , mkCodeGenNodeArg
  -- * Lenses
  , cgnaName
  , cgnaValue
  , cgnaParam
  ) where

import qualified Network.AWS.Glue.Types.CodeGenArgName as Types
import qualified Network.AWS.Glue.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An argument or property of a node.
--
-- /See:/ 'mkCodeGenNodeArg' smart constructor.
data CodeGenNodeArg = CodeGenNodeArg'
  { name :: Types.CodeGenArgName
    -- ^ The name of the argument or property.
  , value :: Types.Value
    -- ^ The value of the argument or property.
  , param :: Core.Maybe Core.Bool
    -- ^ True if the value is used as a parameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CodeGenNodeArg' value with any optional fields omitted.
mkCodeGenNodeArg
    :: Types.CodeGenArgName -- ^ 'name'
    -> Types.Value -- ^ 'value'
    -> CodeGenNodeArg
mkCodeGenNodeArg name value
  = CodeGenNodeArg'{name, value, param = Core.Nothing}

-- | The name of the argument or property.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnaName :: Lens.Lens' CodeGenNodeArg Types.CodeGenArgName
cgnaName = Lens.field @"name"
{-# INLINEABLE cgnaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value of the argument or property.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnaValue :: Lens.Lens' CodeGenNodeArg Types.Value
cgnaValue = Lens.field @"value"
{-# INLINEABLE cgnaValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | True if the value is used as a parameter.
--
-- /Note:/ Consider using 'param' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnaParam :: Lens.Lens' CodeGenNodeArg (Core.Maybe Core.Bool)
cgnaParam = Lens.field @"param"
{-# INLINEABLE cgnaParam #-}
{-# DEPRECATED param "Use generic-lens or generic-optics with 'param' instead"  #-}

instance Core.FromJSON CodeGenNodeArg where
        toJSON CodeGenNodeArg{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Value" Core..= value),
                  ("Param" Core..=) Core.<$> param])

instance Core.FromJSON CodeGenNodeArg where
        parseJSON
          = Core.withObject "CodeGenNodeArg" Core.$
              \ x ->
                CodeGenNodeArg' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Value" Core.<*>
                    x Core..:? "Param"
