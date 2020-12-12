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
    osIsCollection,
    osApplyType,
    osValue,
    osName,
    osDefaultValue,
    osIsModifiable,
    osDataType,
    osAllowedValues,
    osDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Option settings are the actual settings being applied or configured for that option. It is used when you modify an option group or describe option groups. For example, the NATIVE_NETWORK_ENCRYPTION option has a setting called SQLNET.ENCRYPTION_SERVER that can have several different values.
--
-- /See:/ 'mkOptionSetting' smart constructor.
data OptionSetting = OptionSetting'
  { isCollection ::
      Lude.Maybe Lude.Bool,
    applyType :: Lude.Maybe Lude.Text,
    value :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    defaultValue :: Lude.Maybe Lude.Text,
    isModifiable :: Lude.Maybe Lude.Bool,
    dataType :: Lude.Maybe Lude.Text,
    allowedValues :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OptionSetting' with the minimum fields required to make a request.
--
-- * 'allowedValues' - The allowed values of the option setting.
-- * 'applyType' - The DB engine specific parameter type.
-- * 'dataType' - The data type of the option setting.
-- * 'defaultValue' - The default value of the option setting.
-- * 'description' - The description of the option setting.
-- * 'isCollection' - Indicates if the option setting is part of a collection.
-- * 'isModifiable' - A Boolean value that, when true, indicates the option setting can be modified from the default.
-- * 'name' - The name of the option that has settings that you can set.
-- * 'value' - The current value of the option setting.
mkOptionSetting ::
  OptionSetting
mkOptionSetting =
  OptionSetting'
    { isCollection = Lude.Nothing,
      applyType = Lude.Nothing,
      value = Lude.Nothing,
      name = Lude.Nothing,
      defaultValue = Lude.Nothing,
      isModifiable = Lude.Nothing,
      dataType = Lude.Nothing,
      allowedValues = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Indicates if the option setting is part of a collection.
--
-- /Note:/ Consider using 'isCollection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osIsCollection :: Lens.Lens' OptionSetting (Lude.Maybe Lude.Bool)
osIsCollection = Lens.lens (isCollection :: OptionSetting -> Lude.Maybe Lude.Bool) (\s a -> s {isCollection = a} :: OptionSetting)
{-# DEPRECATED osIsCollection "Use generic-lens or generic-optics with 'isCollection' instead." #-}

-- | The DB engine specific parameter type.
--
-- /Note:/ Consider using 'applyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osApplyType :: Lens.Lens' OptionSetting (Lude.Maybe Lude.Text)
osApplyType = Lens.lens (applyType :: OptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {applyType = a} :: OptionSetting)
{-# DEPRECATED osApplyType "Use generic-lens or generic-optics with 'applyType' instead." #-}

-- | The current value of the option setting.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osValue :: Lens.Lens' OptionSetting (Lude.Maybe Lude.Text)
osValue = Lens.lens (value :: OptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: OptionSetting)
{-# DEPRECATED osValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the option that has settings that you can set.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osName :: Lens.Lens' OptionSetting (Lude.Maybe Lude.Text)
osName = Lens.lens (name :: OptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: OptionSetting)
{-# DEPRECATED osName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The default value of the option setting.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDefaultValue :: Lens.Lens' OptionSetting (Lude.Maybe Lude.Text)
osDefaultValue = Lens.lens (defaultValue :: OptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: OptionSetting)
{-# DEPRECATED osDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | A Boolean value that, when true, indicates the option setting can be modified from the default.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osIsModifiable :: Lens.Lens' OptionSetting (Lude.Maybe Lude.Bool)
osIsModifiable = Lens.lens (isModifiable :: OptionSetting -> Lude.Maybe Lude.Bool) (\s a -> s {isModifiable = a} :: OptionSetting)
{-# DEPRECATED osIsModifiable "Use generic-lens or generic-optics with 'isModifiable' instead." #-}

-- | The data type of the option setting.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDataType :: Lens.Lens' OptionSetting (Lude.Maybe Lude.Text)
osDataType = Lens.lens (dataType :: OptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {dataType = a} :: OptionSetting)
{-# DEPRECATED osDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | The allowed values of the option setting.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAllowedValues :: Lens.Lens' OptionSetting (Lude.Maybe Lude.Text)
osAllowedValues = Lens.lens (allowedValues :: OptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {allowedValues = a} :: OptionSetting)
{-# DEPRECATED osAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | The description of the option setting.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDescription :: Lens.Lens' OptionSetting (Lude.Maybe Lude.Text)
osDescription = Lens.lens (description :: OptionSetting -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: OptionSetting)
{-# DEPRECATED osDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML OptionSetting where
  parseXML x =
    OptionSetting'
      Lude.<$> (x Lude..@? "IsCollection")
      Lude.<*> (x Lude..@? "ApplyType")
      Lude.<*> (x Lude..@? "Value")
      Lude.<*> (x Lude..@? "Name")
      Lude.<*> (x Lude..@? "DefaultValue")
      Lude.<*> (x Lude..@? "IsModifiable")
      Lude.<*> (x Lude..@? "DataType")
      Lude.<*> (x Lude..@? "AllowedValues")
      Lude.<*> (x Lude..@? "Description")

instance Lude.ToQuery OptionSetting where
  toQuery OptionSetting' {..} =
    Lude.mconcat
      [ "IsCollection" Lude.=: isCollection,
        "ApplyType" Lude.=: applyType,
        "Value" Lude.=: value,
        "Name" Lude.=: name,
        "DefaultValue" Lude.=: defaultValue,
        "IsModifiable" Lude.=: isModifiable,
        "DataType" Lude.=: dataType,
        "AllowedValues" Lude.=: allowedValues,
        "Description" Lude.=: description
      ]
