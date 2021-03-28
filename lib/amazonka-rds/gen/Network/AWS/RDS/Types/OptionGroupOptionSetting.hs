{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionGroupOptionSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.OptionGroupOptionSetting
  ( OptionGroupOptionSetting (..)
  -- * Smart constructor
  , mkOptionGroupOptionSetting
  -- * Lenses
  , ogosAllowedValues
  , ogosApplyType
  , ogosDefaultValue
  , ogosIsModifiable
  , ogosIsRequired
  , ogosMinimumEngineVersionPerAllowedValue
  , ogosSettingDescription
  , ogosSettingName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue as Types

-- | Option group option settings are used to display settings available for each option with their default values and other information. These values are used with the DescribeOptionGroupOptions action.
--
-- /See:/ 'mkOptionGroupOptionSetting' smart constructor.
data OptionGroupOptionSetting = OptionGroupOptionSetting'
  { allowedValues :: Core.Maybe Core.Text
    -- ^ Indicates the acceptable values for the option group option.
  , applyType :: Core.Maybe Core.Text
    -- ^ The DB engine specific parameter type for the option group option.
  , defaultValue :: Core.Maybe Core.Text
    -- ^ The default value for the option group option.
  , isModifiable :: Core.Maybe Core.Bool
    -- ^ Boolean value where true indicates that this option group option can be changed from the default value.
  , isRequired :: Core.Maybe Core.Bool
    -- ^ Boolean value where true indicates that a value must be specified for this option setting of the option group option.
  , minimumEngineVersionPerAllowedValue :: Core.Maybe [Types.MinimumEngineVersionPerAllowedValue]
    -- ^ The minimum DB engine version required for the corresponding allowed value for this option setting.
  , settingDescription :: Core.Maybe Core.Text
    -- ^ The description of the option group option.
  , settingName :: Core.Maybe Core.Text
    -- ^ The name of the option group option.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OptionGroupOptionSetting' value with any optional fields omitted.
mkOptionGroupOptionSetting
    :: OptionGroupOptionSetting
mkOptionGroupOptionSetting
  = OptionGroupOptionSetting'{allowedValues = Core.Nothing,
                              applyType = Core.Nothing, defaultValue = Core.Nothing,
                              isModifiable = Core.Nothing, isRequired = Core.Nothing,
                              minimumEngineVersionPerAllowedValue = Core.Nothing,
                              settingDescription = Core.Nothing, settingName = Core.Nothing}

-- | Indicates the acceptable values for the option group option.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosAllowedValues :: Lens.Lens' OptionGroupOptionSetting (Core.Maybe Core.Text)
ogosAllowedValues = Lens.field @"allowedValues"
{-# INLINEABLE ogosAllowedValues #-}
{-# DEPRECATED allowedValues "Use generic-lens or generic-optics with 'allowedValues' instead"  #-}

-- | The DB engine specific parameter type for the option group option.
--
-- /Note:/ Consider using 'applyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosApplyType :: Lens.Lens' OptionGroupOptionSetting (Core.Maybe Core.Text)
ogosApplyType = Lens.field @"applyType"
{-# INLINEABLE ogosApplyType #-}
{-# DEPRECATED applyType "Use generic-lens or generic-optics with 'applyType' instead"  #-}

-- | The default value for the option group option.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosDefaultValue :: Lens.Lens' OptionGroupOptionSetting (Core.Maybe Core.Text)
ogosDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE ogosDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | Boolean value where true indicates that this option group option can be changed from the default value.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosIsModifiable :: Lens.Lens' OptionGroupOptionSetting (Core.Maybe Core.Bool)
ogosIsModifiable = Lens.field @"isModifiable"
{-# INLINEABLE ogosIsModifiable #-}
{-# DEPRECATED isModifiable "Use generic-lens or generic-optics with 'isModifiable' instead"  #-}

-- | Boolean value where true indicates that a value must be specified for this option setting of the option group option.
--
-- /Note:/ Consider using 'isRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosIsRequired :: Lens.Lens' OptionGroupOptionSetting (Core.Maybe Core.Bool)
ogosIsRequired = Lens.field @"isRequired"
{-# INLINEABLE ogosIsRequired #-}
{-# DEPRECATED isRequired "Use generic-lens or generic-optics with 'isRequired' instead"  #-}

-- | The minimum DB engine version required for the corresponding allowed value for this option setting.
--
-- /Note:/ Consider using 'minimumEngineVersionPerAllowedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosMinimumEngineVersionPerAllowedValue :: Lens.Lens' OptionGroupOptionSetting (Core.Maybe [Types.MinimumEngineVersionPerAllowedValue])
ogosMinimumEngineVersionPerAllowedValue = Lens.field @"minimumEngineVersionPerAllowedValue"
{-# INLINEABLE ogosMinimumEngineVersionPerAllowedValue #-}
{-# DEPRECATED minimumEngineVersionPerAllowedValue "Use generic-lens or generic-optics with 'minimumEngineVersionPerAllowedValue' instead"  #-}

-- | The description of the option group option.
--
-- /Note:/ Consider using 'settingDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosSettingDescription :: Lens.Lens' OptionGroupOptionSetting (Core.Maybe Core.Text)
ogosSettingDescription = Lens.field @"settingDescription"
{-# INLINEABLE ogosSettingDescription #-}
{-# DEPRECATED settingDescription "Use generic-lens or generic-optics with 'settingDescription' instead"  #-}

-- | The name of the option group option.
--
-- /Note:/ Consider using 'settingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosSettingName :: Lens.Lens' OptionGroupOptionSetting (Core.Maybe Core.Text)
ogosSettingName = Lens.field @"settingName"
{-# INLINEABLE ogosSettingName #-}
{-# DEPRECATED settingName "Use generic-lens or generic-optics with 'settingName' instead"  #-}

instance Core.FromXML OptionGroupOptionSetting where
        parseXML x
          = OptionGroupOptionSetting' Core.<$>
              (x Core..@? "AllowedValues") Core.<*> x Core..@? "ApplyType"
                Core.<*> x Core..@? "DefaultValue"
                Core.<*> x Core..@? "IsModifiable"
                Core.<*> x Core..@? "IsRequired"
                Core.<*>
                x Core..@? "MinimumEngineVersionPerAllowedValue" Core..<@>
                  Core.parseXMLList "MinimumEngineVersionPerAllowedValue"
                Core.<*> x Core..@? "SettingDescription"
                Core.<*> x Core..@? "SettingName"
