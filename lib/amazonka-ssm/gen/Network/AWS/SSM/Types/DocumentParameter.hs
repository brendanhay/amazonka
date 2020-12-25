{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentParameter
  ( DocumentParameter (..),

    -- * Smart constructor
    mkDocumentParameter,

    -- * Lenses
    dpDefaultValue,
    dpDescription,
    dpName,
    dpType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.DefaultValue as Types
import qualified Network.AWS.SSM.Types.Description as Types
import qualified Network.AWS.SSM.Types.DocumentParameterType as Types
import qualified Network.AWS.SSM.Types.Name as Types

-- | Parameters specified in a System Manager document that run on the server when the command is run.
--
-- /See:/ 'mkDocumentParameter' smart constructor.
data DocumentParameter = DocumentParameter'
  { -- | If specified, the default values for the parameters. Parameters without a default value are required. Parameters with a default value are optional.
    defaultValue :: Core.Maybe Types.DefaultValue,
    -- | A description of what the parameter does, how to use it, the default value, and whether or not the parameter is optional.
    description :: Core.Maybe Types.Description,
    -- | The name of the parameter.
    name :: Core.Maybe Types.Name,
    -- | The type of parameter. The type can be either String or StringList.
    type' :: Core.Maybe Types.DocumentParameterType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentParameter' value with any optional fields omitted.
mkDocumentParameter ::
  DocumentParameter
mkDocumentParameter =
  DocumentParameter'
    { defaultValue = Core.Nothing,
      description = Core.Nothing,
      name = Core.Nothing,
      type' = Core.Nothing
    }

-- | If specified, the default values for the parameters. Parameters without a default value are required. Parameters with a default value are optional.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDefaultValue :: Lens.Lens' DocumentParameter (Core.Maybe Types.DefaultValue)
dpDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED dpDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | A description of what the parameter does, how to use it, the default value, and whether or not the parameter is optional.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDescription :: Lens.Lens' DocumentParameter (Core.Maybe Types.Description)
dpDescription = Lens.field @"description"
{-# DEPRECATED dpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpName :: Lens.Lens' DocumentParameter (Core.Maybe Types.Name)
dpName = Lens.field @"name"
{-# DEPRECATED dpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of parameter. The type can be either String or StringList.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpType :: Lens.Lens' DocumentParameter (Core.Maybe Types.DocumentParameterType)
dpType = Lens.field @"type'"
{-# DEPRECATED dpType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON DocumentParameter where
  parseJSON =
    Core.withObject "DocumentParameter" Core.$
      \x ->
        DocumentParameter'
          Core.<$> (x Core..:? "DefaultValue")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Type")
