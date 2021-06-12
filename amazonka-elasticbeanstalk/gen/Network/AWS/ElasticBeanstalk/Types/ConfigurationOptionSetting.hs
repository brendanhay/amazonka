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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A specification identifying an individual configuration option along
-- with its current value. For a list of possible namespaces and option
-- values, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- /See:/ 'newConfigurationOptionSetting' smart constructor.
data ConfigurationOptionSetting = ConfigurationOptionSetting'
  { -- | The name of the configuration option.
    optionName :: Core.Maybe Core.Text,
    -- | The current value for the configuration option.
    value :: Core.Maybe Core.Text,
    -- | A unique namespace that identifies the option\'s associated AWS
    -- resource.
    namespace :: Core.Maybe Core.Text,
    -- | A unique resource name for the option setting. Use it for a time–based
    -- scaling configuration option.
    resourceName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfigurationOptionSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionName', 'configurationOptionSetting_optionName' - The name of the configuration option.
--
-- 'value', 'configurationOptionSetting_value' - The current value for the configuration option.
--
-- 'namespace', 'configurationOptionSetting_namespace' - A unique namespace that identifies the option\'s associated AWS
-- resource.
--
-- 'resourceName', 'configurationOptionSetting_resourceName' - A unique resource name for the option setting. Use it for a time–based
-- scaling configuration option.
newConfigurationOptionSetting ::
  ConfigurationOptionSetting
newConfigurationOptionSetting =
  ConfigurationOptionSetting'
    { optionName =
        Core.Nothing,
      value = Core.Nothing,
      namespace = Core.Nothing,
      resourceName = Core.Nothing
    }

-- | The name of the configuration option.
configurationOptionSetting_optionName :: Lens.Lens' ConfigurationOptionSetting (Core.Maybe Core.Text)
configurationOptionSetting_optionName = Lens.lens (\ConfigurationOptionSetting' {optionName} -> optionName) (\s@ConfigurationOptionSetting' {} a -> s {optionName = a} :: ConfigurationOptionSetting)

-- | The current value for the configuration option.
configurationOptionSetting_value :: Lens.Lens' ConfigurationOptionSetting (Core.Maybe Core.Text)
configurationOptionSetting_value = Lens.lens (\ConfigurationOptionSetting' {value} -> value) (\s@ConfigurationOptionSetting' {} a -> s {value = a} :: ConfigurationOptionSetting)

-- | A unique namespace that identifies the option\'s associated AWS
-- resource.
configurationOptionSetting_namespace :: Lens.Lens' ConfigurationOptionSetting (Core.Maybe Core.Text)
configurationOptionSetting_namespace = Lens.lens (\ConfigurationOptionSetting' {namespace} -> namespace) (\s@ConfigurationOptionSetting' {} a -> s {namespace = a} :: ConfigurationOptionSetting)

-- | A unique resource name for the option setting. Use it for a time–based
-- scaling configuration option.
configurationOptionSetting_resourceName :: Lens.Lens' ConfigurationOptionSetting (Core.Maybe Core.Text)
configurationOptionSetting_resourceName = Lens.lens (\ConfigurationOptionSetting' {resourceName} -> resourceName) (\s@ConfigurationOptionSetting' {} a -> s {resourceName = a} :: ConfigurationOptionSetting)

instance Core.FromXML ConfigurationOptionSetting where
  parseXML x =
    ConfigurationOptionSetting'
      Core.<$> (x Core..@? "OptionName")
      Core.<*> (x Core..@? "Value")
      Core.<*> (x Core..@? "Namespace")
      Core.<*> (x Core..@? "ResourceName")

instance Core.Hashable ConfigurationOptionSetting

instance Core.NFData ConfigurationOptionSetting

instance Core.ToQuery ConfigurationOptionSetting where
  toQuery ConfigurationOptionSetting' {..} =
    Core.mconcat
      [ "OptionName" Core.=: optionName,
        "Value" Core.=: value,
        "Namespace" Core.=: namespace,
        "ResourceName" Core.=: resourceName
      ]
