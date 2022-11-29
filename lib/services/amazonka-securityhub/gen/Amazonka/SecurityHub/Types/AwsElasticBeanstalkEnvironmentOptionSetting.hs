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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentOptionSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentOptionSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A configuration option setting for the environment.
--
-- /See:/ 'newAwsElasticBeanstalkEnvironmentOptionSetting' smart constructor.
data AwsElasticBeanstalkEnvironmentOptionSetting = AwsElasticBeanstalkEnvironmentOptionSetting'
  { -- | The name of the resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the option.
    optionName :: Prelude.Maybe Prelude.Text,
    -- | The type of resource that the configuration option is associated with.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The value of the configuration setting.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElasticBeanstalkEnvironmentOptionSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'awsElasticBeanstalkEnvironmentOptionSetting_resourceName' - The name of the resource.
--
-- 'optionName', 'awsElasticBeanstalkEnvironmentOptionSetting_optionName' - The name of the option.
--
-- 'namespace', 'awsElasticBeanstalkEnvironmentOptionSetting_namespace' - The type of resource that the configuration option is associated with.
--
-- 'value', 'awsElasticBeanstalkEnvironmentOptionSetting_value' - The value of the configuration setting.
newAwsElasticBeanstalkEnvironmentOptionSetting ::
  AwsElasticBeanstalkEnvironmentOptionSetting
newAwsElasticBeanstalkEnvironmentOptionSetting =
  AwsElasticBeanstalkEnvironmentOptionSetting'
    { resourceName =
        Prelude.Nothing,
      optionName = Prelude.Nothing,
      namespace = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the resource.
awsElasticBeanstalkEnvironmentOptionSetting_resourceName :: Lens.Lens' AwsElasticBeanstalkEnvironmentOptionSetting (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentOptionSetting_resourceName = Lens.lens (\AwsElasticBeanstalkEnvironmentOptionSetting' {resourceName} -> resourceName) (\s@AwsElasticBeanstalkEnvironmentOptionSetting' {} a -> s {resourceName = a} :: AwsElasticBeanstalkEnvironmentOptionSetting)

-- | The name of the option.
awsElasticBeanstalkEnvironmentOptionSetting_optionName :: Lens.Lens' AwsElasticBeanstalkEnvironmentOptionSetting (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentOptionSetting_optionName = Lens.lens (\AwsElasticBeanstalkEnvironmentOptionSetting' {optionName} -> optionName) (\s@AwsElasticBeanstalkEnvironmentOptionSetting' {} a -> s {optionName = a} :: AwsElasticBeanstalkEnvironmentOptionSetting)

-- | The type of resource that the configuration option is associated with.
awsElasticBeanstalkEnvironmentOptionSetting_namespace :: Lens.Lens' AwsElasticBeanstalkEnvironmentOptionSetting (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentOptionSetting_namespace = Lens.lens (\AwsElasticBeanstalkEnvironmentOptionSetting' {namespace} -> namespace) (\s@AwsElasticBeanstalkEnvironmentOptionSetting' {} a -> s {namespace = a} :: AwsElasticBeanstalkEnvironmentOptionSetting)

-- | The value of the configuration setting.
awsElasticBeanstalkEnvironmentOptionSetting_value :: Lens.Lens' AwsElasticBeanstalkEnvironmentOptionSetting (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentOptionSetting_value = Lens.lens (\AwsElasticBeanstalkEnvironmentOptionSetting' {value} -> value) (\s@AwsElasticBeanstalkEnvironmentOptionSetting' {} a -> s {value = a} :: AwsElasticBeanstalkEnvironmentOptionSetting)

instance
  Core.FromJSON
    AwsElasticBeanstalkEnvironmentOptionSetting
  where
  parseJSON =
    Core.withObject
      "AwsElasticBeanstalkEnvironmentOptionSetting"
      ( \x ->
          AwsElasticBeanstalkEnvironmentOptionSetting'
            Prelude.<$> (x Core..:? "ResourceName")
              Prelude.<*> (x Core..:? "OptionName")
              Prelude.<*> (x Core..:? "Namespace")
              Prelude.<*> (x Core..:? "Value")
      )

instance
  Prelude.Hashable
    AwsElasticBeanstalkEnvironmentOptionSetting
  where
  hashWithSalt
    _salt
    AwsElasticBeanstalkEnvironmentOptionSetting' {..} =
      _salt `Prelude.hashWithSalt` resourceName
        `Prelude.hashWithSalt` optionName
        `Prelude.hashWithSalt` namespace
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsElasticBeanstalkEnvironmentOptionSetting
  where
  rnf AwsElasticBeanstalkEnvironmentOptionSetting' {..} =
    Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf optionName
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf value

instance
  Core.ToJSON
    AwsElasticBeanstalkEnvironmentOptionSetting
  where
  toJSON
    AwsElasticBeanstalkEnvironmentOptionSetting' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ResourceName" Core..=) Prelude.<$> resourceName,
              ("OptionName" Core..=) Prelude.<$> optionName,
              ("Namespace" Core..=) Prelude.<$> namespace,
              ("Value" Core..=) Prelude.<$> value
            ]
        )
