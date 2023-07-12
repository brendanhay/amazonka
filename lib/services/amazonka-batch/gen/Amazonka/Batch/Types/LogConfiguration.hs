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
-- Module      : Amazonka.Batch.Types.LogConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.LogConfiguration where

import Amazonka.Batch.Types.LogDriver
import Amazonka.Batch.Types.Secret
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Log configuration options to send to a custom log driver for the
-- container.
--
-- /See:/ 'newLogConfiguration' smart constructor.
data LogConfiguration = LogConfiguration'
  { -- | The configuration options to send to the log driver. This parameter
    -- requires version 1.19 of the Docker Remote API or greater on your
    -- container instance. To check the Docker Remote API version on your
    -- container instance, log in to your container instance and run the
    -- following command: @sudo docker version | grep \"Server API version\"@
    options :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The secrets to pass to the log configuration. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
    -- in the /Batch User Guide/.
    secretOptions :: Prelude.Maybe [Secret],
    -- | The log driver to use for the container. The valid values that are
    -- listed for this parameter are log drivers that the Amazon ECS container
    -- agent can communicate with by default.
    --
    -- The supported log drivers are @awslogs@, @fluentd@, @gelf@, @json-file@,
    -- @journald@, @logentries@, @syslog@, and @splunk@.
    --
    -- Jobs that are running on Fargate resources are restricted to the
    -- @awslogs@ and @splunk@ log drivers.
    --
    -- [awslogs]
    --     Specifies the Amazon CloudWatch Logs logging driver. For more
    --     information, see
    --     <https://docs.aws.amazon.com/batch/latest/userguide/using_awslogs.html Using the awslogs log driver>
    --     in the /Batch User Guide/ and
    --     <https://docs.docker.com/config/containers/logging/awslogs/ Amazon CloudWatch Logs logging driver>
    --     in the Docker documentation.
    --
    -- [fluentd]
    --     Specifies the Fluentd logging driver. For more information including
    --     usage and options, see
    --     <https://docs.docker.com/config/containers/logging/fluentd/ Fluentd logging driver>
    --     in the /Docker documentation/.
    --
    -- [gelf]
    --     Specifies the Graylog Extended Format (GELF) logging driver. For
    --     more information including usage and options, see
    --     <https://docs.docker.com/config/containers/logging/gelf/ Graylog Extended Format logging driver>
    --     in the /Docker documentation/.
    --
    -- [journald]
    --     Specifies the journald logging driver. For more information
    --     including usage and options, see
    --     <https://docs.docker.com/config/containers/logging/journald/ Journald logging driver>
    --     in the /Docker documentation/.
    --
    -- [json-file]
    --     Specifies the JSON file logging driver. For more information
    --     including usage and options, see
    --     <https://docs.docker.com/config/containers/logging/json-file/ JSON File logging driver>
    --     in the /Docker documentation/.
    --
    -- [splunk]
    --     Specifies the Splunk logging driver. For more information including
    --     usage and options, see
    --     <https://docs.docker.com/config/containers/logging/splunk/ Splunk logging driver>
    --     in the /Docker documentation/.
    --
    -- [syslog]
    --     Specifies the syslog logging driver. For more information including
    --     usage and options, see
    --     <https://docs.docker.com/config/containers/logging/syslog/ Syslog logging driver>
    --     in the /Docker documentation/.
    --
    -- If you have a custom driver that\'s not listed earlier that you want to
    -- work with the Amazon ECS container agent, you can fork the Amazon ECS
    -- container agent project that\'s
    -- <https://github.com/aws/amazon-ecs-agent available on GitHub> and
    -- customize it to work with that driver. We encourage you to submit pull
    -- requests for changes that you want to have included. However, Amazon Web
    -- Services doesn\'t currently support running modified copies of this
    -- software.
    --
    -- This parameter requires version 1.18 of the Docker Remote API or greater
    -- on your container instance. To check the Docker Remote API version on
    -- your container instance, log in to your container instance and run the
    -- following command: @sudo docker version | grep \"Server API version\"@
    logDriver :: LogDriver
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'logConfiguration_options' - The configuration options to send to the log driver. This parameter
-- requires version 1.19 of the Docker Remote API or greater on your
-- container instance. To check the Docker Remote API version on your
-- container instance, log in to your container instance and run the
-- following command: @sudo docker version | grep \"Server API version\"@
--
-- 'secretOptions', 'logConfiguration_secretOptions' - The secrets to pass to the log configuration. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /Batch User Guide/.
--
-- 'logDriver', 'logConfiguration_logDriver' - The log driver to use for the container. The valid values that are
-- listed for this parameter are log drivers that the Amazon ECS container
-- agent can communicate with by default.
--
-- The supported log drivers are @awslogs@, @fluentd@, @gelf@, @json-file@,
-- @journald@, @logentries@, @syslog@, and @splunk@.
--
-- Jobs that are running on Fargate resources are restricted to the
-- @awslogs@ and @splunk@ log drivers.
--
-- [awslogs]
--     Specifies the Amazon CloudWatch Logs logging driver. For more
--     information, see
--     <https://docs.aws.amazon.com/batch/latest/userguide/using_awslogs.html Using the awslogs log driver>
--     in the /Batch User Guide/ and
--     <https://docs.docker.com/config/containers/logging/awslogs/ Amazon CloudWatch Logs logging driver>
--     in the Docker documentation.
--
-- [fluentd]
--     Specifies the Fluentd logging driver. For more information including
--     usage and options, see
--     <https://docs.docker.com/config/containers/logging/fluentd/ Fluentd logging driver>
--     in the /Docker documentation/.
--
-- [gelf]
--     Specifies the Graylog Extended Format (GELF) logging driver. For
--     more information including usage and options, see
--     <https://docs.docker.com/config/containers/logging/gelf/ Graylog Extended Format logging driver>
--     in the /Docker documentation/.
--
-- [journald]
--     Specifies the journald logging driver. For more information
--     including usage and options, see
--     <https://docs.docker.com/config/containers/logging/journald/ Journald logging driver>
--     in the /Docker documentation/.
--
-- [json-file]
--     Specifies the JSON file logging driver. For more information
--     including usage and options, see
--     <https://docs.docker.com/config/containers/logging/json-file/ JSON File logging driver>
--     in the /Docker documentation/.
--
-- [splunk]
--     Specifies the Splunk logging driver. For more information including
--     usage and options, see
--     <https://docs.docker.com/config/containers/logging/splunk/ Splunk logging driver>
--     in the /Docker documentation/.
--
-- [syslog]
--     Specifies the syslog logging driver. For more information including
--     usage and options, see
--     <https://docs.docker.com/config/containers/logging/syslog/ Syslog logging driver>
--     in the /Docker documentation/.
--
-- If you have a custom driver that\'s not listed earlier that you want to
-- work with the Amazon ECS container agent, you can fork the Amazon ECS
-- container agent project that\'s
-- <https://github.com/aws/amazon-ecs-agent available on GitHub> and
-- customize it to work with that driver. We encourage you to submit pull
-- requests for changes that you want to have included. However, Amazon Web
-- Services doesn\'t currently support running modified copies of this
-- software.
--
-- This parameter requires version 1.18 of the Docker Remote API or greater
-- on your container instance. To check the Docker Remote API version on
-- your container instance, log in to your container instance and run the
-- following command: @sudo docker version | grep \"Server API version\"@
newLogConfiguration ::
  -- | 'logDriver'
  LogDriver ->
  LogConfiguration
newLogConfiguration pLogDriver_ =
  LogConfiguration'
    { options = Prelude.Nothing,
      secretOptions = Prelude.Nothing,
      logDriver = pLogDriver_
    }

-- | The configuration options to send to the log driver. This parameter
-- requires version 1.19 of the Docker Remote API or greater on your
-- container instance. To check the Docker Remote API version on your
-- container instance, log in to your container instance and run the
-- following command: @sudo docker version | grep \"Server API version\"@
logConfiguration_options :: Lens.Lens' LogConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
logConfiguration_options = Lens.lens (\LogConfiguration' {options} -> options) (\s@LogConfiguration' {} a -> s {options = a} :: LogConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The secrets to pass to the log configuration. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /Batch User Guide/.
logConfiguration_secretOptions :: Lens.Lens' LogConfiguration (Prelude.Maybe [Secret])
logConfiguration_secretOptions = Lens.lens (\LogConfiguration' {secretOptions} -> secretOptions) (\s@LogConfiguration' {} a -> s {secretOptions = a} :: LogConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The log driver to use for the container. The valid values that are
-- listed for this parameter are log drivers that the Amazon ECS container
-- agent can communicate with by default.
--
-- The supported log drivers are @awslogs@, @fluentd@, @gelf@, @json-file@,
-- @journald@, @logentries@, @syslog@, and @splunk@.
--
-- Jobs that are running on Fargate resources are restricted to the
-- @awslogs@ and @splunk@ log drivers.
--
-- [awslogs]
--     Specifies the Amazon CloudWatch Logs logging driver. For more
--     information, see
--     <https://docs.aws.amazon.com/batch/latest/userguide/using_awslogs.html Using the awslogs log driver>
--     in the /Batch User Guide/ and
--     <https://docs.docker.com/config/containers/logging/awslogs/ Amazon CloudWatch Logs logging driver>
--     in the Docker documentation.
--
-- [fluentd]
--     Specifies the Fluentd logging driver. For more information including
--     usage and options, see
--     <https://docs.docker.com/config/containers/logging/fluentd/ Fluentd logging driver>
--     in the /Docker documentation/.
--
-- [gelf]
--     Specifies the Graylog Extended Format (GELF) logging driver. For
--     more information including usage and options, see
--     <https://docs.docker.com/config/containers/logging/gelf/ Graylog Extended Format logging driver>
--     in the /Docker documentation/.
--
-- [journald]
--     Specifies the journald logging driver. For more information
--     including usage and options, see
--     <https://docs.docker.com/config/containers/logging/journald/ Journald logging driver>
--     in the /Docker documentation/.
--
-- [json-file]
--     Specifies the JSON file logging driver. For more information
--     including usage and options, see
--     <https://docs.docker.com/config/containers/logging/json-file/ JSON File logging driver>
--     in the /Docker documentation/.
--
-- [splunk]
--     Specifies the Splunk logging driver. For more information including
--     usage and options, see
--     <https://docs.docker.com/config/containers/logging/splunk/ Splunk logging driver>
--     in the /Docker documentation/.
--
-- [syslog]
--     Specifies the syslog logging driver. For more information including
--     usage and options, see
--     <https://docs.docker.com/config/containers/logging/syslog/ Syslog logging driver>
--     in the /Docker documentation/.
--
-- If you have a custom driver that\'s not listed earlier that you want to
-- work with the Amazon ECS container agent, you can fork the Amazon ECS
-- container agent project that\'s
-- <https://github.com/aws/amazon-ecs-agent available on GitHub> and
-- customize it to work with that driver. We encourage you to submit pull
-- requests for changes that you want to have included. However, Amazon Web
-- Services doesn\'t currently support running modified copies of this
-- software.
--
-- This parameter requires version 1.18 of the Docker Remote API or greater
-- on your container instance. To check the Docker Remote API version on
-- your container instance, log in to your container instance and run the
-- following command: @sudo docker version | grep \"Server API version\"@
logConfiguration_logDriver :: Lens.Lens' LogConfiguration LogDriver
logConfiguration_logDriver = Lens.lens (\LogConfiguration' {logDriver} -> logDriver) (\s@LogConfiguration' {} a -> s {logDriver = a} :: LogConfiguration)

instance Data.FromJSON LogConfiguration where
  parseJSON =
    Data.withObject
      "LogConfiguration"
      ( \x ->
          LogConfiguration'
            Prelude.<$> (x Data..:? "options" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "secretOptions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "logDriver")
      )

instance Prelude.Hashable LogConfiguration where
  hashWithSalt _salt LogConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` secretOptions
      `Prelude.hashWithSalt` logDriver

instance Prelude.NFData LogConfiguration where
  rnf LogConfiguration' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf secretOptions
      `Prelude.seq` Prelude.rnf logDriver

instance Data.ToJSON LogConfiguration where
  toJSON LogConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("options" Data..=) Prelude.<$> options,
            ("secretOptions" Data..=) Prelude.<$> secretOptions,
            Prelude.Just ("logDriver" Data..= logDriver)
          ]
      )
