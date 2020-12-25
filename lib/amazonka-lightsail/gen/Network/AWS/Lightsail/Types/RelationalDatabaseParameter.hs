{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseParameter
  ( RelationalDatabaseParameter (..),

    -- * Smart constructor
    mkRelationalDatabaseParameter,

    -- * Lenses
    rdpAllowedValues,
    rdpApplyMethod,
    rdpApplyType,
    rdpDataType,
    rdpDescription,
    rdpIsModifiable,
    rdpParameterName,
    rdpParameterValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the parameters of a database.
--
-- /See:/ 'mkRelationalDatabaseParameter' smart constructor.
data RelationalDatabaseParameter = RelationalDatabaseParameter'
  { -- | Specifies the valid range of values for the parameter.
    allowedValues :: Core.Maybe Types.String,
    -- | Indicates when parameter updates are applied.
    --
    -- Can be @immediate@ or @pending-reboot@ .
    applyMethod :: Core.Maybe Types.String,
    -- | Specifies the engine-specific parameter type.
    applyType :: Core.Maybe Types.String,
    -- | Specifies the valid data type for the parameter.
    dataType :: Core.Maybe Types.String,
    -- | Provides a description of the parameter.
    description :: Core.Maybe Types.String,
    -- | A Boolean value indicating whether the parameter can be modified.
    isModifiable :: Core.Maybe Core.Bool,
    -- | Specifies the name of the parameter.
    parameterName :: Core.Maybe Types.String,
    -- | Specifies the value of the parameter.
    parameterValue :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RelationalDatabaseParameter' value with any optional fields omitted.
mkRelationalDatabaseParameter ::
  RelationalDatabaseParameter
mkRelationalDatabaseParameter =
  RelationalDatabaseParameter'
    { allowedValues = Core.Nothing,
      applyMethod = Core.Nothing,
      applyType = Core.Nothing,
      dataType = Core.Nothing,
      description = Core.Nothing,
      isModifiable = Core.Nothing,
      parameterName = Core.Nothing,
      parameterValue = Core.Nothing
    }

-- | Specifies the valid range of values for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpAllowedValues :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Types.String)
rdpAllowedValues = Lens.field @"allowedValues"
{-# DEPRECATED rdpAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | Indicates when parameter updates are applied.
--
-- Can be @immediate@ or @pending-reboot@ .
--
-- /Note:/ Consider using 'applyMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpApplyMethod :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Types.String)
rdpApplyMethod = Lens.field @"applyMethod"
{-# DEPRECATED rdpApplyMethod "Use generic-lens or generic-optics with 'applyMethod' instead." #-}

-- | Specifies the engine-specific parameter type.
--
-- /Note:/ Consider using 'applyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpApplyType :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Types.String)
rdpApplyType = Lens.field @"applyType"
{-# DEPRECATED rdpApplyType "Use generic-lens or generic-optics with 'applyType' instead." #-}

-- | Specifies the valid data type for the parameter.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpDataType :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Types.String)
rdpDataType = Lens.field @"dataType"
{-# DEPRECATED rdpDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | Provides a description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpDescription :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Types.String)
rdpDescription = Lens.field @"description"
{-# DEPRECATED rdpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A Boolean value indicating whether the parameter can be modified.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpIsModifiable :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Core.Bool)
rdpIsModifiable = Lens.field @"isModifiable"
{-# DEPRECATED rdpIsModifiable "Use generic-lens or generic-optics with 'isModifiable' instead." #-}

-- | Specifies the name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpParameterName :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Types.String)
rdpParameterName = Lens.field @"parameterName"
{-# DEPRECATED rdpParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | Specifies the value of the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpParameterValue :: Lens.Lens' RelationalDatabaseParameter (Core.Maybe Types.String)
rdpParameterValue = Lens.field @"parameterValue"
{-# DEPRECATED rdpParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

instance Core.FromJSON RelationalDatabaseParameter where
  toJSON RelationalDatabaseParameter {..} =
    Core.object
      ( Core.catMaybes
          [ ("allowedValues" Core..=) Core.<$> allowedValues,
            ("applyMethod" Core..=) Core.<$> applyMethod,
            ("applyType" Core..=) Core.<$> applyType,
            ("dataType" Core..=) Core.<$> dataType,
            ("description" Core..=) Core.<$> description,
            ("isModifiable" Core..=) Core.<$> isModifiable,
            ("parameterName" Core..=) Core.<$> parameterName,
            ("parameterValue" Core..=) Core.<$> parameterValue
          ]
      )

instance Core.FromJSON RelationalDatabaseParameter where
  parseJSON =
    Core.withObject "RelationalDatabaseParameter" Core.$
      \x ->
        RelationalDatabaseParameter'
          Core.<$> (x Core..:? "allowedValues")
          Core.<*> (x Core..:? "applyMethod")
          Core.<*> (x Core..:? "applyType")
          Core.<*> (x Core..:? "dataType")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "isModifiable")
          Core.<*> (x Core..:? "parameterName")
          Core.<*> (x Core..:? "parameterValue")
