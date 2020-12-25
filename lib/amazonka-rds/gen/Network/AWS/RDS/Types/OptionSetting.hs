{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionSetting
  ( OptionSetting (..),

    -- * Smart constructor
    mkOptionSetting,

    -- * Lenses
    osAllowedValues,
    osApplyType,
    osDataType,
    osDefaultValue,
    osDescription,
    osIsCollection,
    osIsModifiable,
    osName,
    osValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | Option settings are the actual settings being applied or configured for that option. It is used when you modify an option group or describe option groups. For example, the NATIVE_NETWORK_ENCRYPTION option has a setting called SQLNET.ENCRYPTION_SERVER that can have several different values.
--
-- /See:/ 'mkOptionSetting' smart constructor.
data OptionSetting = OptionSetting'
  { -- | The allowed values of the option setting.
    allowedValues :: Core.Maybe Types.String,
    -- | The DB engine specific parameter type.
    applyType :: Core.Maybe Types.String,
    -- | The data type of the option setting.
    dataType :: Core.Maybe Types.String,
    -- | The default value of the option setting.
    defaultValue :: Core.Maybe Types.String,
    -- | The description of the option setting.
    description :: Core.Maybe Types.String,
    -- | Indicates if the option setting is part of a collection.
    isCollection :: Core.Maybe Core.Bool,
    -- | A Boolean value that, when true, indicates the option setting can be modified from the default.
    isModifiable :: Core.Maybe Core.Bool,
    -- | The name of the option that has settings that you can set.
    name :: Core.Maybe Types.String,
    -- | The current value of the option setting.
    value :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OptionSetting' value with any optional fields omitted.
mkOptionSetting ::
  OptionSetting
mkOptionSetting =
  OptionSetting'
    { allowedValues = Core.Nothing,
      applyType = Core.Nothing,
      dataType = Core.Nothing,
      defaultValue = Core.Nothing,
      description = Core.Nothing,
      isCollection = Core.Nothing,
      isModifiable = Core.Nothing,
      name = Core.Nothing,
      value = Core.Nothing
    }

-- | The allowed values of the option setting.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAllowedValues :: Lens.Lens' OptionSetting (Core.Maybe Types.String)
osAllowedValues = Lens.field @"allowedValues"
{-# DEPRECATED osAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | The DB engine specific parameter type.
--
-- /Note:/ Consider using 'applyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osApplyType :: Lens.Lens' OptionSetting (Core.Maybe Types.String)
osApplyType = Lens.field @"applyType"
{-# DEPRECATED osApplyType "Use generic-lens or generic-optics with 'applyType' instead." #-}

-- | The data type of the option setting.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDataType :: Lens.Lens' OptionSetting (Core.Maybe Types.String)
osDataType = Lens.field @"dataType"
{-# DEPRECATED osDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | The default value of the option setting.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDefaultValue :: Lens.Lens' OptionSetting (Core.Maybe Types.String)
osDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED osDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | The description of the option setting.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDescription :: Lens.Lens' OptionSetting (Core.Maybe Types.String)
osDescription = Lens.field @"description"
{-# DEPRECATED osDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates if the option setting is part of a collection.
--
-- /Note:/ Consider using 'isCollection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osIsCollection :: Lens.Lens' OptionSetting (Core.Maybe Core.Bool)
osIsCollection = Lens.field @"isCollection"
{-# DEPRECATED osIsCollection "Use generic-lens or generic-optics with 'isCollection' instead." #-}

-- | A Boolean value that, when true, indicates the option setting can be modified from the default.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osIsModifiable :: Lens.Lens' OptionSetting (Core.Maybe Core.Bool)
osIsModifiable = Lens.field @"isModifiable"
{-# DEPRECATED osIsModifiable "Use generic-lens or generic-optics with 'isModifiable' instead." #-}

-- | The name of the option that has settings that you can set.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osName :: Lens.Lens' OptionSetting (Core.Maybe Types.String)
osName = Lens.field @"name"
{-# DEPRECATED osName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The current value of the option setting.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osValue :: Lens.Lens' OptionSetting (Core.Maybe Types.String)
osValue = Lens.field @"value"
{-# DEPRECATED osValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML OptionSetting where
  parseXML x =
    OptionSetting'
      Core.<$> (x Core..@? "AllowedValues")
      Core.<*> (x Core..@? "ApplyType")
      Core.<*> (x Core..@? "DataType")
      Core.<*> (x Core..@? "DefaultValue")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "IsCollection")
      Core.<*> (x Core..@? "IsModifiable")
      Core.<*> (x Core..@? "Name")
      Core.<*> (x Core..@? "Value")
