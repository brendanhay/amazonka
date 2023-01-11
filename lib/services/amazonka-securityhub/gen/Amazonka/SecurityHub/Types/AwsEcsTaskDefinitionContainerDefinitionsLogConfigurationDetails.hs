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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails

-- | The log configuration specification for the container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails = AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails'
  { -- | The log driver to use for the container.
    --
    -- Valid values on Fargate are as follows:
    --
    -- -   @awsfirelens@
    --
    -- -   @awslogs@
    --
    -- -   @splunk@
    --
    -- Valid values on Amazon EC2 are as follows:
    --
    -- -   @awsfirelens@
    --
    -- -   @awslogs@
    --
    -- -   @fluentd@
    --
    -- -   @gelf@
    --
    -- -   @journald@
    --
    -- -   @json-file@
    --
    -- -   @logentries@
    --
    -- -   @splunk@
    --
    -- -   @syslog@
    logDriver :: Prelude.Maybe Prelude.Text,
    -- | The configuration options to send to the log driver. Requires version
    -- 1.19 of the Docker Remote API or greater on your container instance.
    options :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The secrets to pass to the log configuration.
    secretOptions :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logDriver', 'awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_logDriver' - The log driver to use for the container.
--
-- Valid values on Fargate are as follows:
--
-- -   @awsfirelens@
--
-- -   @awslogs@
--
-- -   @splunk@
--
-- Valid values on Amazon EC2 are as follows:
--
-- -   @awsfirelens@
--
-- -   @awslogs@
--
-- -   @fluentd@
--
-- -   @gelf@
--
-- -   @journald@
--
-- -   @json-file@
--
-- -   @logentries@
--
-- -   @splunk@
--
-- -   @syslog@
--
-- 'options', 'awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_options' - The configuration options to send to the log driver. Requires version
-- 1.19 of the Docker Remote API or greater on your container instance.
--
-- 'secretOptions', 'awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_secretOptions' - The secrets to pass to the log configuration.
newAwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
newAwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails =
  AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails'
    { logDriver =
        Prelude.Nothing,
      options =
        Prelude.Nothing,
      secretOptions =
        Prelude.Nothing
    }

-- | The log driver to use for the container.
--
-- Valid values on Fargate are as follows:
--
-- -   @awsfirelens@
--
-- -   @awslogs@
--
-- -   @splunk@
--
-- Valid values on Amazon EC2 are as follows:
--
-- -   @awsfirelens@
--
-- -   @awslogs@
--
-- -   @fluentd@
--
-- -   @gelf@
--
-- -   @journald@
--
-- -   @json-file@
--
-- -   @logentries@
--
-- -   @splunk@
--
-- -   @syslog@
awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_logDriver :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_logDriver = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails' {logDriver} -> logDriver) (\s@AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails' {} a -> s {logDriver = a} :: AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails)

-- | The configuration options to send to the log driver. Requires version
-- 1.19 of the Docker Remote API or greater on your container instance.
awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_options :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_options = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails' {options} -> options) (\s@AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails' {} a -> s {options = a} :: AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

-- | The secrets to pass to the log configuration.
awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_secretOptions :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails])
awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_secretOptions = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails' {secretOptions} -> secretOptions) (\s@AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails' {} a -> s {secretOptions = a} :: AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails'
            Prelude.<$> (x Data..:? "LogDriver")
              Prelude.<*> (x Data..:? "Options" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "SecretOptions" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails' {..} =
      _salt `Prelude.hashWithSalt` logDriver
        `Prelude.hashWithSalt` options
        `Prelude.hashWithSalt` secretOptions

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails' {..} =
      Prelude.rnf logDriver
        `Prelude.seq` Prelude.rnf options
        `Prelude.seq` Prelude.rnf secretOptions

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("LogDriver" Data..=) Prelude.<$> logDriver,
              ("Options" Data..=) Prelude.<$> options,
              ("SecretOptions" Data..=) Prelude.<$> secretOptions
            ]
        )
