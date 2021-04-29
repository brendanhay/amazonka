{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionDescription where

import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionValueType
import Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the possible values for a configuration option.
--
-- /See:/ 'newConfigurationOptionDescription' smart constructor.
data ConfigurationOptionDescription = ConfigurationOptionDescription'
  { -- | If specified, the configuration option must be a numeric value less than
    -- this value.
    maxValue :: Prelude.Maybe Prelude.Int,
    -- | If specified, values for the configuration option are selected from this
    -- list.
    valueOptions :: Prelude.Maybe [Prelude.Text],
    -- | An indication of which type of values this option has and whether it is
    -- allowable to select one or more than one of the possible values:
    --
    -- -   @Scalar@ : Values for this option are a single selection from the
    --     possible values, or an unformatted string, or numeric value governed
    --     by the @MIN\/MAX\/Regex@ constraints.
    --
    -- -   @List@ : Values for this option are multiple selections from the
    --     possible values.
    --
    -- -   @Boolean@ : Values for this option are either @true@ or @false@ .
    --
    -- -   @Json@ : Values for this option are a JSON representation of a
    --     @ConfigDocument@.
    valueType :: Prelude.Maybe ConfigurationOptionValueType,
    -- | An indication of which action is required if the value for this
    -- configuration option changes:
    --
    -- -   @NoInterruption@ : There is no interruption to the environment or
    --     application availability.
    --
    -- -   @RestartEnvironment@ : The environment is entirely restarted, all
    --     AWS resources are deleted and recreated, and the environment is
    --     unavailable during the process.
    --
    -- -   @RestartApplicationServer@ : The environment is available the entire
    --     time. However, a short application outage occurs when the
    --     application servers on the running Amazon EC2 instances are
    --     restarted.
    changeSeverity :: Prelude.Maybe Prelude.Text,
    -- | If specified, the configuration option must be a string value that
    -- satisfies this regular expression.
    regex :: Prelude.Maybe OptionRestrictionRegex,
    -- | The name of the configuration option.
    name :: Prelude.Maybe Prelude.Text,
    -- | If specified, the configuration option must be a numeric value greater
    -- than this value.
    minValue :: Prelude.Maybe Prelude.Int,
    -- | A unique namespace identifying the option\'s associated AWS resource.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | An indication of whether the user defined this configuration option:
    --
    -- -   @true@ : This configuration option was defined by the user. It is a
    --     valid choice for specifying if this as an @Option to Remove@ when
    --     updating configuration settings.
    --
    -- -   @false@ : This configuration was not defined by the user.
    --
    -- Constraint: You can remove only @UserDefined@ options from a
    -- configuration.
    --
    -- Valid Values: @true@ | @false@
    userDefined :: Prelude.Maybe Prelude.Bool,
    -- | If specified, the configuration option must be a string value no longer
    -- than this value.
    maxLength :: Prelude.Maybe Prelude.Int,
    -- | The default value for this configuration option.
    defaultValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationOptionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxValue', 'configurationOptionDescription_maxValue' - If specified, the configuration option must be a numeric value less than
-- this value.
--
-- 'valueOptions', 'configurationOptionDescription_valueOptions' - If specified, values for the configuration option are selected from this
-- list.
--
-- 'valueType', 'configurationOptionDescription_valueType' - An indication of which type of values this option has and whether it is
-- allowable to select one or more than one of the possible values:
--
-- -   @Scalar@ : Values for this option are a single selection from the
--     possible values, or an unformatted string, or numeric value governed
--     by the @MIN\/MAX\/Regex@ constraints.
--
-- -   @List@ : Values for this option are multiple selections from the
--     possible values.
--
-- -   @Boolean@ : Values for this option are either @true@ or @false@ .
--
-- -   @Json@ : Values for this option are a JSON representation of a
--     @ConfigDocument@.
--
-- 'changeSeverity', 'configurationOptionDescription_changeSeverity' - An indication of which action is required if the value for this
-- configuration option changes:
--
-- -   @NoInterruption@ : There is no interruption to the environment or
--     application availability.
--
-- -   @RestartEnvironment@ : The environment is entirely restarted, all
--     AWS resources are deleted and recreated, and the environment is
--     unavailable during the process.
--
-- -   @RestartApplicationServer@ : The environment is available the entire
--     time. However, a short application outage occurs when the
--     application servers on the running Amazon EC2 instances are
--     restarted.
--
-- 'regex', 'configurationOptionDescription_regex' - If specified, the configuration option must be a string value that
-- satisfies this regular expression.
--
-- 'name', 'configurationOptionDescription_name' - The name of the configuration option.
--
-- 'minValue', 'configurationOptionDescription_minValue' - If specified, the configuration option must be a numeric value greater
-- than this value.
--
-- 'namespace', 'configurationOptionDescription_namespace' - A unique namespace identifying the option\'s associated AWS resource.
--
-- 'userDefined', 'configurationOptionDescription_userDefined' - An indication of whether the user defined this configuration option:
--
-- -   @true@ : This configuration option was defined by the user. It is a
--     valid choice for specifying if this as an @Option to Remove@ when
--     updating configuration settings.
--
-- -   @false@ : This configuration was not defined by the user.
--
-- Constraint: You can remove only @UserDefined@ options from a
-- configuration.
--
-- Valid Values: @true@ | @false@
--
-- 'maxLength', 'configurationOptionDescription_maxLength' - If specified, the configuration option must be a string value no longer
-- than this value.
--
-- 'defaultValue', 'configurationOptionDescription_defaultValue' - The default value for this configuration option.
newConfigurationOptionDescription ::
  ConfigurationOptionDescription
newConfigurationOptionDescription =
  ConfigurationOptionDescription'
    { maxValue =
        Prelude.Nothing,
      valueOptions = Prelude.Nothing,
      valueType = Prelude.Nothing,
      changeSeverity = Prelude.Nothing,
      regex = Prelude.Nothing,
      name = Prelude.Nothing,
      minValue = Prelude.Nothing,
      namespace = Prelude.Nothing,
      userDefined = Prelude.Nothing,
      maxLength = Prelude.Nothing,
      defaultValue = Prelude.Nothing
    }

-- | If specified, the configuration option must be a numeric value less than
-- this value.
configurationOptionDescription_maxValue :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Int)
configurationOptionDescription_maxValue = Lens.lens (\ConfigurationOptionDescription' {maxValue} -> maxValue) (\s@ConfigurationOptionDescription' {} a -> s {maxValue = a} :: ConfigurationOptionDescription)

-- | If specified, values for the configuration option are selected from this
-- list.
configurationOptionDescription_valueOptions :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe [Prelude.Text])
configurationOptionDescription_valueOptions = Lens.lens (\ConfigurationOptionDescription' {valueOptions} -> valueOptions) (\s@ConfigurationOptionDescription' {} a -> s {valueOptions = a} :: ConfigurationOptionDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | An indication of which type of values this option has and whether it is
-- allowable to select one or more than one of the possible values:
--
-- -   @Scalar@ : Values for this option are a single selection from the
--     possible values, or an unformatted string, or numeric value governed
--     by the @MIN\/MAX\/Regex@ constraints.
--
-- -   @List@ : Values for this option are multiple selections from the
--     possible values.
--
-- -   @Boolean@ : Values for this option are either @true@ or @false@ .
--
-- -   @Json@ : Values for this option are a JSON representation of a
--     @ConfigDocument@.
configurationOptionDescription_valueType :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe ConfigurationOptionValueType)
configurationOptionDescription_valueType = Lens.lens (\ConfigurationOptionDescription' {valueType} -> valueType) (\s@ConfigurationOptionDescription' {} a -> s {valueType = a} :: ConfigurationOptionDescription)

-- | An indication of which action is required if the value for this
-- configuration option changes:
--
-- -   @NoInterruption@ : There is no interruption to the environment or
--     application availability.
--
-- -   @RestartEnvironment@ : The environment is entirely restarted, all
--     AWS resources are deleted and recreated, and the environment is
--     unavailable during the process.
--
-- -   @RestartApplicationServer@ : The environment is available the entire
--     time. However, a short application outage occurs when the
--     application servers on the running Amazon EC2 instances are
--     restarted.
configurationOptionDescription_changeSeverity :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Text)
configurationOptionDescription_changeSeverity = Lens.lens (\ConfigurationOptionDescription' {changeSeverity} -> changeSeverity) (\s@ConfigurationOptionDescription' {} a -> s {changeSeverity = a} :: ConfigurationOptionDescription)

-- | If specified, the configuration option must be a string value that
-- satisfies this regular expression.
configurationOptionDescription_regex :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe OptionRestrictionRegex)
configurationOptionDescription_regex = Lens.lens (\ConfigurationOptionDescription' {regex} -> regex) (\s@ConfigurationOptionDescription' {} a -> s {regex = a} :: ConfigurationOptionDescription)

-- | The name of the configuration option.
configurationOptionDescription_name :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Text)
configurationOptionDescription_name = Lens.lens (\ConfigurationOptionDescription' {name} -> name) (\s@ConfigurationOptionDescription' {} a -> s {name = a} :: ConfigurationOptionDescription)

-- | If specified, the configuration option must be a numeric value greater
-- than this value.
configurationOptionDescription_minValue :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Int)
configurationOptionDescription_minValue = Lens.lens (\ConfigurationOptionDescription' {minValue} -> minValue) (\s@ConfigurationOptionDescription' {} a -> s {minValue = a} :: ConfigurationOptionDescription)

-- | A unique namespace identifying the option\'s associated AWS resource.
configurationOptionDescription_namespace :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Text)
configurationOptionDescription_namespace = Lens.lens (\ConfigurationOptionDescription' {namespace} -> namespace) (\s@ConfigurationOptionDescription' {} a -> s {namespace = a} :: ConfigurationOptionDescription)

-- | An indication of whether the user defined this configuration option:
--
-- -   @true@ : This configuration option was defined by the user. It is a
--     valid choice for specifying if this as an @Option to Remove@ when
--     updating configuration settings.
--
-- -   @false@ : This configuration was not defined by the user.
--
-- Constraint: You can remove only @UserDefined@ options from a
-- configuration.
--
-- Valid Values: @true@ | @false@
configurationOptionDescription_userDefined :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Bool)
configurationOptionDescription_userDefined = Lens.lens (\ConfigurationOptionDescription' {userDefined} -> userDefined) (\s@ConfigurationOptionDescription' {} a -> s {userDefined = a} :: ConfigurationOptionDescription)

-- | If specified, the configuration option must be a string value no longer
-- than this value.
configurationOptionDescription_maxLength :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Int)
configurationOptionDescription_maxLength = Lens.lens (\ConfigurationOptionDescription' {maxLength} -> maxLength) (\s@ConfigurationOptionDescription' {} a -> s {maxLength = a} :: ConfigurationOptionDescription)

-- | The default value for this configuration option.
configurationOptionDescription_defaultValue :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Text)
configurationOptionDescription_defaultValue = Lens.lens (\ConfigurationOptionDescription' {defaultValue} -> defaultValue) (\s@ConfigurationOptionDescription' {} a -> s {defaultValue = a} :: ConfigurationOptionDescription)

instance
  Prelude.FromXML
    ConfigurationOptionDescription
  where
  parseXML x =
    ConfigurationOptionDescription'
      Prelude.<$> (x Prelude..@? "MaxValue")
      Prelude.<*> ( x Prelude..@? "ValueOptions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "ValueType")
      Prelude.<*> (x Prelude..@? "ChangeSeverity")
      Prelude.<*> (x Prelude..@? "Regex")
      Prelude.<*> (x Prelude..@? "Name")
      Prelude.<*> (x Prelude..@? "MinValue")
      Prelude.<*> (x Prelude..@? "Namespace")
      Prelude.<*> (x Prelude..@? "UserDefined")
      Prelude.<*> (x Prelude..@? "MaxLength")
      Prelude.<*> (x Prelude..@? "DefaultValue")

instance
  Prelude.Hashable
    ConfigurationOptionDescription

instance
  Prelude.NFData
    ConfigurationOptionDescription
