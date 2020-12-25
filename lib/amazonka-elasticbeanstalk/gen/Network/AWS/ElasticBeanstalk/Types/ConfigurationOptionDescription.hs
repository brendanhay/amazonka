{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    codChangeSeverity,
    codDefaultValue,
    codMaxLength,
    codMaxValue,
    codMinValue,
    codName,
    codNamespace,
    codRegex,
    codUserDefined,
    codValueOptions,
    codValueType,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionPossibleValue as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSeverity as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionValueType as Types
import qualified Network.AWS.ElasticBeanstalk.Types.DefaultValue as Types
import qualified Network.AWS.ElasticBeanstalk.Types.OptionNamespace as Types
import qualified Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the possible values for a configuration option.
--
-- /See:/ 'mkConfigurationOptionDescription' smart constructor.
data ConfigurationOptionDescription = ConfigurationOptionDescription'
  { -- | An indication of which action is required if the value for this configuration option changes:
    --
    --
    --     * @NoInterruption@ : There is no interruption to the environment or application availability.
    --
    --
    --     * @RestartEnvironment@ : The environment is entirely restarted, all AWS resources are deleted and recreated, and the environment is unavailable during the process.
    --
    --
    --     * @RestartApplicationServer@ : The environment is available the entire time. However, a short application outage occurs when the application servers on the running Amazon EC2 instances are restarted.
    changeSeverity :: Core.Maybe Types.ConfigurationOptionSeverity,
    -- | The default value for this configuration option.
    defaultValue :: Core.Maybe Types.DefaultValue,
    -- | If specified, the configuration option must be a string value no longer than this value.
    maxLength :: Core.Maybe Core.Int,
    -- | If specified, the configuration option must be a numeric value less than this value.
    maxValue :: Core.Maybe Core.Int,
    -- | If specified, the configuration option must be a numeric value greater than this value.
    minValue :: Core.Maybe Core.Int,
    -- | The name of the configuration option.
    name :: Core.Maybe Types.ConfigurationOptionName,
    -- | A unique namespace identifying the option's associated AWS resource.
    namespace :: Core.Maybe Types.OptionNamespace,
    -- | If specified, the configuration option must be a string value that satisfies this regular expression.
    regex :: Core.Maybe Types.OptionRestrictionRegex,
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
    userDefined :: Core.Maybe Core.Bool,
    -- | If specified, values for the configuration option are selected from this list.
    valueOptions :: Core.Maybe [Types.ConfigurationOptionPossibleValue],
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
    valueType :: Core.Maybe Types.ConfigurationOptionValueType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigurationOptionDescription' value with any optional fields omitted.
mkConfigurationOptionDescription ::
  ConfigurationOptionDescription
mkConfigurationOptionDescription =
  ConfigurationOptionDescription'
    { changeSeverity = Core.Nothing,
      defaultValue = Core.Nothing,
      maxLength = Core.Nothing,
      maxValue = Core.Nothing,
      minValue = Core.Nothing,
      name = Core.Nothing,
      namespace = Core.Nothing,
      regex = Core.Nothing,
      userDefined = Core.Nothing,
      valueOptions = Core.Nothing,
      valueType = Core.Nothing
    }

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
codChangeSeverity :: Lens.Lens' ConfigurationOptionDescription (Core.Maybe Types.ConfigurationOptionSeverity)
codChangeSeverity = Lens.field @"changeSeverity"
{-# DEPRECATED codChangeSeverity "Use generic-lens or generic-optics with 'changeSeverity' instead." #-}

-- | The default value for this configuration option.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codDefaultValue :: Lens.Lens' ConfigurationOptionDescription (Core.Maybe Types.DefaultValue)
codDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED codDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | If specified, the configuration option must be a string value no longer than this value.
--
-- /Note:/ Consider using 'maxLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codMaxLength :: Lens.Lens' ConfigurationOptionDescription (Core.Maybe Core.Int)
codMaxLength = Lens.field @"maxLength"
{-# DEPRECATED codMaxLength "Use generic-lens or generic-optics with 'maxLength' instead." #-}

-- | If specified, the configuration option must be a numeric value less than this value.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codMaxValue :: Lens.Lens' ConfigurationOptionDescription (Core.Maybe Core.Int)
codMaxValue = Lens.field @"maxValue"
{-# DEPRECATED codMaxValue "Use generic-lens or generic-optics with 'maxValue' instead." #-}

-- | If specified, the configuration option must be a numeric value greater than this value.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codMinValue :: Lens.Lens' ConfigurationOptionDescription (Core.Maybe Core.Int)
codMinValue = Lens.field @"minValue"
{-# DEPRECATED codMinValue "Use generic-lens or generic-optics with 'minValue' instead." #-}

-- | The name of the configuration option.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codName :: Lens.Lens' ConfigurationOptionDescription (Core.Maybe Types.ConfigurationOptionName)
codName = Lens.field @"name"
{-# DEPRECATED codName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique namespace identifying the option's associated AWS resource.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codNamespace :: Lens.Lens' ConfigurationOptionDescription (Core.Maybe Types.OptionNamespace)
codNamespace = Lens.field @"namespace"
{-# DEPRECATED codNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | If specified, the configuration option must be a string value that satisfies this regular expression.
--
-- /Note:/ Consider using 'regex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codRegex :: Lens.Lens' ConfigurationOptionDescription (Core.Maybe Types.OptionRestrictionRegex)
codRegex = Lens.field @"regex"
{-# DEPRECATED codRegex "Use generic-lens or generic-optics with 'regex' instead." #-}

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
codUserDefined :: Lens.Lens' ConfigurationOptionDescription (Core.Maybe Core.Bool)
codUserDefined = Lens.field @"userDefined"
{-# DEPRECATED codUserDefined "Use generic-lens or generic-optics with 'userDefined' instead." #-}

-- | If specified, values for the configuration option are selected from this list.
--
-- /Note:/ Consider using 'valueOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
codValueOptions :: Lens.Lens' ConfigurationOptionDescription (Core.Maybe [Types.ConfigurationOptionPossibleValue])
codValueOptions = Lens.field @"valueOptions"
{-# DEPRECATED codValueOptions "Use generic-lens or generic-optics with 'valueOptions' instead." #-}

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
codValueType :: Lens.Lens' ConfigurationOptionDescription (Core.Maybe Types.ConfigurationOptionValueType)
codValueType = Lens.field @"valueType"
{-# DEPRECATED codValueType "Use generic-lens or generic-optics with 'valueType' instead." #-}

instance Core.FromXML ConfigurationOptionDescription where
  parseXML x =
    ConfigurationOptionDescription'
      Core.<$> (x Core..@? "ChangeSeverity")
      Core.<*> (x Core..@? "DefaultValue")
      Core.<*> (x Core..@? "MaxLength")
      Core.<*> (x Core..@? "MaxValue")
      Core.<*> (x Core..@? "MinValue")
      Core.<*> (x Core..@? "Name")
      Core.<*> (x Core..@? "Namespace")
      Core.<*> (x Core..@? "Regex")
      Core.<*> (x Core..@? "UserDefined")
      Core.<*> (x Core..@? "ValueOptions" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "ValueType")
