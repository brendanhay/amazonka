{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.Type
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.Type
  ( Type (..)
  -- * Smart constructor
  , mkType
  -- * Lenses
  , tArn
  , tDefinition
  , tDescription
  , tFormat
  , tName
  ) where

import qualified Network.AWS.AppSync.Types.ResourceName as Types
import qualified Network.AWS.AppSync.Types.TypeDefinitionFormat as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a type.
--
-- /See:/ 'mkType' smart constructor.
data Type = Type'
  { arn :: Core.Maybe Core.Text
    -- ^ The type ARN.
  , definition :: Core.Maybe Core.Text
    -- ^ The type definition.
  , description :: Core.Maybe Core.Text
    -- ^ The type description.
  , format :: Core.Maybe Types.TypeDefinitionFormat
    -- ^ The type format: SDL or JSON.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The type name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Type' value with any optional fields omitted.
mkType
    :: Type
mkType
  = Type'{arn = Core.Nothing, definition = Core.Nothing,
          description = Core.Nothing, format = Core.Nothing,
          name = Core.Nothing}

-- | The type ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tArn :: Lens.Lens' Type (Core.Maybe Core.Text)
tArn = Lens.field @"arn"
{-# INLINEABLE tArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The type definition.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDefinition :: Lens.Lens' Type (Core.Maybe Core.Text)
tDefinition = Lens.field @"definition"
{-# INLINEABLE tDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

-- | The type description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDescription :: Lens.Lens' Type (Core.Maybe Core.Text)
tDescription = Lens.field @"description"
{-# INLINEABLE tDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The type format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tFormat :: Lens.Lens' Type (Core.Maybe Types.TypeDefinitionFormat)
tFormat = Lens.field @"format"
{-# INLINEABLE tFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The type name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Type (Core.Maybe Types.ResourceName)
tName = Lens.field @"name"
{-# INLINEABLE tName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON Type where
        parseJSON
          = Core.withObject "Type" Core.$
              \ x ->
                Type' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "definition" Core.<*>
                    x Core..:? "description"
                    Core.<*> x Core..:? "format"
                    Core.<*> x Core..:? "name"
