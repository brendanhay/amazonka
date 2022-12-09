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
-- Module      : Amazonka.ElasticBeanstalk.Types.ConfigurationOptionDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ConfigurationOptionDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.ConfigurationOptionValueType
import Amazonka.ElasticBeanstalk.Types.OptionRestrictionRegex
import qualified Amazonka.Prelude as Prelude

-- | Describes the possible values for a configuration option.
--
-- /See:/ 'newConfigurationOptionDescription' smart constructor.
data ConfigurationOptionDescription = ConfigurationOptionDescription'
  { -- | An indication of which action is required if the value for this
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
    -- | The default value for this configuration option.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | If specified, the configuration option must be a string value no longer
    -- than this value.
    maxLength :: Prelude.Maybe Prelude.Int,
    -- | If specified, the configuration option must be a numeric value less than
    -- this value.
    maxValue :: Prelude.Maybe Prelude.Int,
    -- | If specified, the configuration option must be a numeric value greater
    -- than this value.
    minValue :: Prelude.Maybe Prelude.Int,
    -- | The name of the configuration option.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique namespace identifying the option\'s associated AWS resource.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | If specified, the configuration option must be a string value that
    -- satisfies this regular expression.
    regex :: Prelude.Maybe OptionRestrictionRegex,
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
    valueType :: Prelude.Maybe ConfigurationOptionValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationOptionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'defaultValue', 'configurationOptionDescription_defaultValue' - The default value for this configuration option.
--
-- 'maxLength', 'configurationOptionDescription_maxLength' - If specified, the configuration option must be a string value no longer
-- than this value.
--
-- 'maxValue', 'configurationOptionDescription_maxValue' - If specified, the configuration option must be a numeric value less than
-- this value.
--
-- 'minValue', 'configurationOptionDescription_minValue' - If specified, the configuration option must be a numeric value greater
-- than this value.
--
-- 'name', 'configurationOptionDescription_name' - The name of the configuration option.
--
-- 'namespace', 'configurationOptionDescription_namespace' - A unique namespace identifying the option\'s associated AWS resource.
--
-- 'regex', 'configurationOptionDescription_regex' - If specified, the configuration option must be a string value that
-- satisfies this regular expression.
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
newConfigurationOptionDescription ::
  ConfigurationOptionDescription
newConfigurationOptionDescription =
  ConfigurationOptionDescription'
    { changeSeverity =
        Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      maxLength = Prelude.Nothing,
      maxValue = Prelude.Nothing,
      minValue = Prelude.Nothing,
      name = Prelude.Nothing,
      namespace = Prelude.Nothing,
      regex = Prelude.Nothing,
      userDefined = Prelude.Nothing,
      valueOptions = Prelude.Nothing,
      valueType = Prelude.Nothing
    }

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

-- | The default value for this configuration option.
configurationOptionDescription_defaultValue :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Text)
configurationOptionDescription_defaultValue = Lens.lens (\ConfigurationOptionDescription' {defaultValue} -> defaultValue) (\s@ConfigurationOptionDescription' {} a -> s {defaultValue = a} :: ConfigurationOptionDescription)

-- | If specified, the configuration option must be a string value no longer
-- than this value.
configurationOptionDescription_maxLength :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Int)
configurationOptionDescription_maxLength = Lens.lens (\ConfigurationOptionDescription' {maxLength} -> maxLength) (\s@ConfigurationOptionDescription' {} a -> s {maxLength = a} :: ConfigurationOptionDescription)

-- | If specified, the configuration option must be a numeric value less than
-- this value.
configurationOptionDescription_maxValue :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Int)
configurationOptionDescription_maxValue = Lens.lens (\ConfigurationOptionDescription' {maxValue} -> maxValue) (\s@ConfigurationOptionDescription' {} a -> s {maxValue = a} :: ConfigurationOptionDescription)

-- | If specified, the configuration option must be a numeric value greater
-- than this value.
configurationOptionDescription_minValue :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Int)
configurationOptionDescription_minValue = Lens.lens (\ConfigurationOptionDescription' {minValue} -> minValue) (\s@ConfigurationOptionDescription' {} a -> s {minValue = a} :: ConfigurationOptionDescription)

-- | The name of the configuration option.
configurationOptionDescription_name :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Text)
configurationOptionDescription_name = Lens.lens (\ConfigurationOptionDescription' {name} -> name) (\s@ConfigurationOptionDescription' {} a -> s {name = a} :: ConfigurationOptionDescription)

-- | A unique namespace identifying the option\'s associated AWS resource.
configurationOptionDescription_namespace :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe Prelude.Text)
configurationOptionDescription_namespace = Lens.lens (\ConfigurationOptionDescription' {namespace} -> namespace) (\s@ConfigurationOptionDescription' {} a -> s {namespace = a} :: ConfigurationOptionDescription)

-- | If specified, the configuration option must be a string value that
-- satisfies this regular expression.
configurationOptionDescription_regex :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe OptionRestrictionRegex)
configurationOptionDescription_regex = Lens.lens (\ConfigurationOptionDescription' {regex} -> regex) (\s@ConfigurationOptionDescription' {} a -> s {regex = a} :: ConfigurationOptionDescription)

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

-- | If specified, values for the configuration option are selected from this
-- list.
configurationOptionDescription_valueOptions :: Lens.Lens' ConfigurationOptionDescription (Prelude.Maybe [Prelude.Text])
configurationOptionDescription_valueOptions = Lens.lens (\ConfigurationOptionDescription' {valueOptions} -> valueOptions) (\s@ConfigurationOptionDescription' {} a -> s {valueOptions = a} :: ConfigurationOptionDescription) Prelude.. Lens.mapping Lens.coerced

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

instance Data.FromXML ConfigurationOptionDescription where
  parseXML x =
    ConfigurationOptionDescription'
      Prelude.<$> (x Data..@? "ChangeSeverity")
      Prelude.<*> (x Data..@? "DefaultValue")
      Prelude.<*> (x Data..@? "MaxLength")
      Prelude.<*> (x Data..@? "MaxValue")
      Prelude.<*> (x Data..@? "MinValue")
      Prelude.<*> (x Data..@? "Name")
      Prelude.<*> (x Data..@? "Namespace")
      Prelude.<*> (x Data..@? "Regex")
      Prelude.<*> (x Data..@? "UserDefined")
      Prelude.<*> ( x Data..@? "ValueOptions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "ValueType")

instance
  Prelude.Hashable
    ConfigurationOptionDescription
  where
  hashWithSalt
    _salt
    ConfigurationOptionDescription' {..} =
      _salt `Prelude.hashWithSalt` changeSeverity
        `Prelude.hashWithSalt` defaultValue
        `Prelude.hashWithSalt` maxLength
        `Prelude.hashWithSalt` maxValue
        `Prelude.hashWithSalt` minValue
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` namespace
        `Prelude.hashWithSalt` regex
        `Prelude.hashWithSalt` userDefined
        `Prelude.hashWithSalt` valueOptions
        `Prelude.hashWithSalt` valueType

instance
  Prelude.NFData
    ConfigurationOptionDescription
  where
  rnf ConfigurationOptionDescription' {..} =
    Prelude.rnf changeSeverity
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf maxLength
      `Prelude.seq` Prelude.rnf maxValue
      `Prelude.seq` Prelude.rnf minValue
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf regex
      `Prelude.seq` Prelude.rnf userDefined
      `Prelude.seq` Prelude.rnf valueOptions
      `Prelude.seq` Prelude.rnf valueType
