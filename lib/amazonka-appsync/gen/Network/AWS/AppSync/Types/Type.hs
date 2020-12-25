{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.Type
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.Type
  ( Type (..),

    -- * Smart constructor
    mkType,

    -- * Lenses
    tArn,
    tDefinition,
    tDescription,
    tFormat,
    tName,
  )
where

import qualified Network.AWS.AppSync.Types.ResourceName as Types
import qualified Network.AWS.AppSync.Types.String as Types
import qualified Network.AWS.AppSync.Types.TypeDefinitionFormat as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a type.
--
-- /See:/ 'mkType' smart constructor.
data Type = Type'
  { -- | The type ARN.
    arn :: Core.Maybe Types.String,
    -- | The type definition.
    definition :: Core.Maybe Types.String,
    -- | The type description.
    description :: Core.Maybe Types.String,
    -- | The type format: SDL or JSON.
    format :: Core.Maybe Types.TypeDefinitionFormat,
    -- | The type name.
    name :: Core.Maybe Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Type' value with any optional fields omitted.
mkType ::
  Type
mkType =
  Type'
    { arn = Core.Nothing,
      definition = Core.Nothing,
      description = Core.Nothing,
      format = Core.Nothing,
      name = Core.Nothing
    }

-- | The type ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tArn :: Lens.Lens' Type (Core.Maybe Types.String)
tArn = Lens.field @"arn"
{-# DEPRECATED tArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The type definition.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDefinition :: Lens.Lens' Type (Core.Maybe Types.String)
tDefinition = Lens.field @"definition"
{-# DEPRECATED tDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The type description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDescription :: Lens.Lens' Type (Core.Maybe Types.String)
tDescription = Lens.field @"description"
{-# DEPRECATED tDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The type format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tFormat :: Lens.Lens' Type (Core.Maybe Types.TypeDefinitionFormat)
tFormat = Lens.field @"format"
{-# DEPRECATED tFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The type name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Type (Core.Maybe Types.ResourceName)
tName = Lens.field @"name"
{-# DEPRECATED tName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON Type where
  parseJSON =
    Core.withObject "Type" Core.$
      \x ->
        Type'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "definition")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "format")
          Core.<*> (x Core..:? "name")
