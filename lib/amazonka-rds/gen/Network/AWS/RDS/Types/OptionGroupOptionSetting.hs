-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionGroupOptionSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionGroupOptionSetting
  ( OptionGroupOptionSetting (..),

    -- * Smart constructor
    mkOptionGroupOptionSetting,

    -- * Lenses
    ogosApplyType,
    ogosMinimumEngineVersionPerAllowedValue,
    ogosSettingName,
    ogosDefaultValue,
    ogosIsModifiable,
    ogosSettingDescription,
    ogosAllowedValues,
    ogosIsRequired,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue

-- | Option group option settings are used to display settings available for each option with their default values and other information. These values are used with the DescribeOptionGroupOptions action.
--
-- /See:/ 'mkOptionGroupOptionSetting' smart constructor.
data OptionGroupOptionSetting = OptionGroupOptionSetting'
  { applyType ::
      Lude.Maybe Lude.Text,
    minimumEngineVersionPerAllowedValue ::
      Lude.Maybe
        [MinimumEngineVersionPerAllowedValue],
    settingName :: Lude.Maybe Lude.Text,
    defaultValue :: Lude.Maybe Lude.Text,
    isModifiable :: Lude.Maybe Lude.Bool,
    settingDescription ::
      Lude.Maybe Lude.Text,
    allowedValues :: Lude.Maybe Lude.Text,
    isRequired :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OptionGroupOptionSetting' with the minimum fields required to make a request.
--
-- * 'allowedValues' - Indicates the acceptable values for the option group option.
-- * 'applyType' - The DB engine specific parameter type for the option group option.
-- * 'defaultValue' - The default value for the option group option.
-- * 'isModifiable' - Boolean value where true indicates that this option group option can be changed from the default value.
-- * 'isRequired' - Boolean value where true indicates that a value must be specified for this option setting of the option group option.
-- * 'minimumEngineVersionPerAllowedValue' - The minimum DB engine version required for the corresponding allowed value for this option setting.
-- * 'settingDescription' - The description of the option group option.
-- * 'settingName' - The name of the option group option.
mkOptionGroupOptionSetting ::
  OptionGroupOptionSetting
mkOptionGroupOptionSetting =
  OptionGroupOptionSetting'
    { applyType = Lude.Nothing,
      minimumEngineVersionPerAllowedValue = Lude.Nothing,
      settingName = Lude.Nothing,
      defaultValue = Lude.Nothing,
      isModifiable = Lude.Nothing,
      settingDescription = Lude.Nothing,
      allowedValues = Lude.Nothing,
      isRequired = Lude.Nothing
    }

-- | The DB engine specific parameter type for the option group option.
--
-- /Note:/ Consider using 'applyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosApplyType :: Lens.Lens' OptionGroupOptionSetting (Lude.Maybe Lude.Text)
ogosApplyType = Lens.lens (applyType :: OptionGroupOptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {applyType = a} :: OptionGroupOptionSetting)
{-# DEPRECATED ogosApplyType "Use generic-lens or generic-optics with 'applyType' instead." #-}

-- | The minimum DB engine version required for the corresponding allowed value for this option setting.
--
-- /Note:/ Consider using 'minimumEngineVersionPerAllowedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosMinimumEngineVersionPerAllowedValue :: Lens.Lens' OptionGroupOptionSetting (Lude.Maybe [MinimumEngineVersionPerAllowedValue])
ogosMinimumEngineVersionPerAllowedValue = Lens.lens (minimumEngineVersionPerAllowedValue :: OptionGroupOptionSetting -> Lude.Maybe [MinimumEngineVersionPerAllowedValue]) (\s a -> s {minimumEngineVersionPerAllowedValue = a} :: OptionGroupOptionSetting)
{-# DEPRECATED ogosMinimumEngineVersionPerAllowedValue "Use generic-lens or generic-optics with 'minimumEngineVersionPerAllowedValue' instead." #-}

-- | The name of the option group option.
--
-- /Note:/ Consider using 'settingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosSettingName :: Lens.Lens' OptionGroupOptionSetting (Lude.Maybe Lude.Text)
ogosSettingName = Lens.lens (settingName :: OptionGroupOptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {settingName = a} :: OptionGroupOptionSetting)
{-# DEPRECATED ogosSettingName "Use generic-lens or generic-optics with 'settingName' instead." #-}

-- | The default value for the option group option.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosDefaultValue :: Lens.Lens' OptionGroupOptionSetting (Lude.Maybe Lude.Text)
ogosDefaultValue = Lens.lens (defaultValue :: OptionGroupOptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: OptionGroupOptionSetting)
{-# DEPRECATED ogosDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Boolean value where true indicates that this option group option can be changed from the default value.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosIsModifiable :: Lens.Lens' OptionGroupOptionSetting (Lude.Maybe Lude.Bool)
ogosIsModifiable = Lens.lens (isModifiable :: OptionGroupOptionSetting -> Lude.Maybe Lude.Bool) (\s a -> s {isModifiable = a} :: OptionGroupOptionSetting)
{-# DEPRECATED ogosIsModifiable "Use generic-lens or generic-optics with 'isModifiable' instead." #-}

-- | The description of the option group option.
--
-- /Note:/ Consider using 'settingDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosSettingDescription :: Lens.Lens' OptionGroupOptionSetting (Lude.Maybe Lude.Text)
ogosSettingDescription = Lens.lens (settingDescription :: OptionGroupOptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {settingDescription = a} :: OptionGroupOptionSetting)
{-# DEPRECATED ogosSettingDescription "Use generic-lens or generic-optics with 'settingDescription' instead." #-}

-- | Indicates the acceptable values for the option group option.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosAllowedValues :: Lens.Lens' OptionGroupOptionSetting (Lude.Maybe Lude.Text)
ogosAllowedValues = Lens.lens (allowedValues :: OptionGroupOptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {allowedValues = a} :: OptionGroupOptionSetting)
{-# DEPRECATED ogosAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | Boolean value where true indicates that a value must be specified for this option setting of the option group option.
--
-- /Note:/ Consider using 'isRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogosIsRequired :: Lens.Lens' OptionGroupOptionSetting (Lude.Maybe Lude.Bool)
ogosIsRequired = Lens.lens (isRequired :: OptionGroupOptionSetting -> Lude.Maybe Lude.Bool) (\s a -> s {isRequired = a} :: OptionGroupOptionSetting)
{-# DEPRECATED ogosIsRequired "Use generic-lens or generic-optics with 'isRequired' instead." #-}

instance Lude.FromXML OptionGroupOptionSetting where
  parseXML x =
    OptionGroupOptionSetting'
      Lude.<$> (x Lude..@? "ApplyType")
      Lude.<*> ( x Lude..@? "MinimumEngineVersionPerAllowedValue"
                   Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "MinimumEngineVersionPerAllowedValue")
               )
      Lude.<*> (x Lude..@? "SettingName")
      Lude.<*> (x Lude..@? "DefaultValue")
      Lude.<*> (x Lude..@? "IsModifiable")
      Lude.<*> (x Lude..@? "SettingDescription")
      Lude.<*> (x Lude..@? "AllowedValues")
      Lude.<*> (x Lude..@? "IsRequired")
