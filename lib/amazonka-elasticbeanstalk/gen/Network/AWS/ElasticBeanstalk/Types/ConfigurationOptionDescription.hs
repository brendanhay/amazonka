-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionDescription
  ( ConfigurationOptionDescription (..),

    -- * Smart constructor
    mkConfigurationOptionDescription,

    -- * Lenses
    codMaxValue,
    codRegex,
    codMaxLength,
    codUserDefined,
    codNamespace,
    codValueOptions,
    codName,
    codChangeSeverity,
    codDefaultValue,
    codValueType,
    codMinValue,
  )
where

import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionValueType
import Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the possible values for a configuration option.
--
-- /See:/ 'mkConfigurationOptionDescription' smart constructor.
data ConfigurationOptionDescription = ConfigurationOptionDescription'
  { maxValue ::
      Lude.Maybe Lude.Int,
    regex ::
      Lude.Maybe
        OptionRestrictionRegex,
    maxLength ::
      Lude.Maybe Lude.Int,
    userDefined ::
      Lude.Maybe Lude.Bool,
    namespace ::
      Lude.Maybe Lude.Text,
    valueOptions ::
      Lude.Maybe [Lude.Text],
    name :: Lude.Maybe Lude.Text,
    changeSeverity ::
      Lude.Maybe Lude.Text,
    defaultValue ::
      Lude.Maybe Lude.Text,
    valueType ::
      Lude.Maybe
        ConfigurationOptionValueType,
    minValue ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigurationOptionDescription' with the minimum fields required to make a request.
--
-- * 'changeSeverity' - An indication of which action is required if the value for this configuration option changes:
--
--
--     * @NoInterruption@ : There is no interruption to the environment or application availability.
--
--
--     * @RestartEnvironment@ : The environment is entirely restarted, all AWS resources are deleted and recreated, and the environment is unavailable during the process.
--
--
--     * @RestartApplicationServer@ : The environment is available the entire time. However, a short application outage occurs when the application servers on the running Amazon EC2 instances are restarted.
--
--
-- * 'defaultValue' - The default value for this configuration option.
-- * 'maxLength' - If specified, the configuration option must be a string value no longer than this value.
-- * 'maxValue' - If specified, the configuration option must be a numeric value less than this value.
-- * 'minValue' - If specified, the configuration option must be a numeric value greater than this value.
-- * 'name' - The name of the configuration option.
-- * 'namespace' - A unique namespace identifying the option's associated AWS resource.
-- * 'regex' - If specified, the configuration option must be a string value that satisfies this regular expression.
-- * 'userDefined' - An indication of whether the user defined this configuration option:
--
--
--     * @true@ : This configuration option was defined by the user. It is a valid choice for specifying if this as an @Option to Remove@ when updating configuration settings.
--
--
--     * @false@ : This configuration was not defined by the user.
--
--
-- Constraint: You can remove only @UserDefined@ options from a configuration.
-- Valid Values: @true@ | @false@
-- * 'valueOptions' - If specified, values for the configuration option are selected from this list.
-- * 'valueType' - An indication of which type of values this option has and whether it is allowable to select one or more than one of the possible values:
--
--
--     * @Scalar@ : Values for this option are a single selection from the possible values, or an unformatted string, or numeric value governed by the @MIN/MAX/Regex@ constraints.
--
--
--     * @List@ : Values for this option are multiple selections from the possible values.
--
--
--     * @Boolean@ : Values for this option are either @true@ or @false@ .
--
--
--     * @Json@ : Values for this option are a JSON representation of a @ConfigDocument@ .
mkConfigurationOptionDescription ::
  ConfigurationOptionDescription
mkConfigurationOptionDescription =
  ConfigurationOptionDescription'
    { maxValue = Lude.Nothing,
      regex = Lude.Nothing,
      maxLength = Lude.Nothing,
      userDefined = Lude.Nothing,
      namespace = Lude.Nothing,
      valueOptions = Lude.Nothing,
      name = Lude.Nothing,
      changeSeverity = Lude.Nothing,
      defaultValue = Lude.Nothing,
      valueType = Lude.Nothing,
      minValue = Lude.Nothing
    }

-- | If specified, the configuration option must be a numeric value less than this value.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codMaxValue :: Lens.Lens' ConfigurationOptionDescription (Lude.Maybe Lude.Int)
codMaxValue = Lens.lens (maxValue :: ConfigurationOptionDescription -> Lude.Maybe Lude.Int) (\s a -> s {maxValue = a} :: ConfigurationOptionDescription)
{-# DEPRECATED codMaxValue "Use generic-lens or generic-optics with 'maxValue' instead." #-}

-- | If specified, the configuration option must be a string value that satisfies this regular expression.
--
-- /Note:/ Consider using 'regex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codRegex :: Lens.Lens' ConfigurationOptionDescription (Lude.Maybe OptionRestrictionRegex)
codRegex = Lens.lens (regex :: ConfigurationOptionDescription -> Lude.Maybe OptionRestrictionRegex) (\s a -> s {regex = a} :: ConfigurationOptionDescription)
{-# DEPRECATED codRegex "Use generic-lens or generic-optics with 'regex' instead." #-}

-- | If specified, the configuration option must be a string value no longer than this value.
--
-- /Note:/ Consider using 'maxLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codMaxLength :: Lens.Lens' ConfigurationOptionDescription (Lude.Maybe Lude.Int)
codMaxLength = Lens.lens (maxLength :: ConfigurationOptionDescription -> Lude.Maybe Lude.Int) (\s a -> s {maxLength = a} :: ConfigurationOptionDescription)
{-# DEPRECATED codMaxLength "Use generic-lens or generic-optics with 'maxLength' instead." #-}

-- | An indication of whether the user defined this configuration option:
--
--
--     * @true@ : This configuration option was defined by the user. It is a valid choice for specifying if this as an @Option to Remove@ when updating configuration settings.
--
--
--     * @false@ : This configuration was not defined by the user.
--
--
-- Constraint: You can remove only @UserDefined@ options from a configuration.
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'userDefined' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codUserDefined :: Lens.Lens' ConfigurationOptionDescription (Lude.Maybe Lude.Bool)
codUserDefined = Lens.lens (userDefined :: ConfigurationOptionDescription -> Lude.Maybe Lude.Bool) (\s a -> s {userDefined = a} :: ConfigurationOptionDescription)
{-# DEPRECATED codUserDefined "Use generic-lens or generic-optics with 'userDefined' instead." #-}

-- | A unique namespace identifying the option's associated AWS resource.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codNamespace :: Lens.Lens' ConfigurationOptionDescription (Lude.Maybe Lude.Text)
codNamespace = Lens.lens (namespace :: ConfigurationOptionDescription -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: ConfigurationOptionDescription)
{-# DEPRECATED codNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | If specified, values for the configuration option are selected from this list.
--
-- /Note:/ Consider using 'valueOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codValueOptions :: Lens.Lens' ConfigurationOptionDescription (Lude.Maybe [Lude.Text])
codValueOptions = Lens.lens (valueOptions :: ConfigurationOptionDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {valueOptions = a} :: ConfigurationOptionDescription)
{-# DEPRECATED codValueOptions "Use generic-lens or generic-optics with 'valueOptions' instead." #-}

-- | The name of the configuration option.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codName :: Lens.Lens' ConfigurationOptionDescription (Lude.Maybe Lude.Text)
codName = Lens.lens (name :: ConfigurationOptionDescription -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ConfigurationOptionDescription)
{-# DEPRECATED codName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An indication of which action is required if the value for this configuration option changes:
--
--
--     * @NoInterruption@ : There is no interruption to the environment or application availability.
--
--
--     * @RestartEnvironment@ : The environment is entirely restarted, all AWS resources are deleted and recreated, and the environment is unavailable during the process.
--
--
--     * @RestartApplicationServer@ : The environment is available the entire time. However, a short application outage occurs when the application servers on the running Amazon EC2 instances are restarted.
--
--
--
-- /Note:/ Consider using 'changeSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codChangeSeverity :: Lens.Lens' ConfigurationOptionDescription (Lude.Maybe Lude.Text)
codChangeSeverity = Lens.lens (changeSeverity :: ConfigurationOptionDescription -> Lude.Maybe Lude.Text) (\s a -> s {changeSeverity = a} :: ConfigurationOptionDescription)
{-# DEPRECATED codChangeSeverity "Use generic-lens or generic-optics with 'changeSeverity' instead." #-}

-- | The default value for this configuration option.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codDefaultValue :: Lens.Lens' ConfigurationOptionDescription (Lude.Maybe Lude.Text)
codDefaultValue = Lens.lens (defaultValue :: ConfigurationOptionDescription -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: ConfigurationOptionDescription)
{-# DEPRECATED codDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | An indication of which type of values this option has and whether it is allowable to select one or more than one of the possible values:
--
--
--     * @Scalar@ : Values for this option are a single selection from the possible values, or an unformatted string, or numeric value governed by the @MIN/MAX/Regex@ constraints.
--
--
--     * @List@ : Values for this option are multiple selections from the possible values.
--
--
--     * @Boolean@ : Values for this option are either @true@ or @false@ .
--
--
--     * @Json@ : Values for this option are a JSON representation of a @ConfigDocument@ .
--
--
--
-- /Note:/ Consider using 'valueType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codValueType :: Lens.Lens' ConfigurationOptionDescription (Lude.Maybe ConfigurationOptionValueType)
codValueType = Lens.lens (valueType :: ConfigurationOptionDescription -> Lude.Maybe ConfigurationOptionValueType) (\s a -> s {valueType = a} :: ConfigurationOptionDescription)
{-# DEPRECATED codValueType "Use generic-lens or generic-optics with 'valueType' instead." #-}

-- | If specified, the configuration option must be a numeric value greater than this value.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codMinValue :: Lens.Lens' ConfigurationOptionDescription (Lude.Maybe Lude.Int)
codMinValue = Lens.lens (minValue :: ConfigurationOptionDescription -> Lude.Maybe Lude.Int) (\s a -> s {minValue = a} :: ConfigurationOptionDescription)
{-# DEPRECATED codMinValue "Use generic-lens or generic-optics with 'minValue' instead." #-}

instance Lude.FromXML ConfigurationOptionDescription where
  parseXML x =
    ConfigurationOptionDescription'
      Lude.<$> (x Lude..@? "MaxValue")
      Lude.<*> (x Lude..@? "Regex")
      Lude.<*> (x Lude..@? "MaxLength")
      Lude.<*> (x Lude..@? "UserDefined")
      Lude.<*> (x Lude..@? "Namespace")
      Lude.<*> ( x Lude..@? "ValueOptions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Name")
      Lude.<*> (x Lude..@? "ChangeSeverity")
      Lude.<*> (x Lude..@? "DefaultValue")
      Lude.<*> (x Lude..@? "ValueType")
      Lude.<*> (x Lude..@? "MinValue")
