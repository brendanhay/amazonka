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
-- Module      : Amazonka.MQ.Types.Configuration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.Configuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MQ.Types.AuthenticationStrategy
import Amazonka.MQ.Types.ConfigurationRevision
import Amazonka.MQ.Types.EngineType
import qualified Amazonka.Prelude as Prelude

-- | Returns information about all configurations.
--
-- /See:/ 'newConfiguration' smart constructor.
data Configuration = Configuration'
  { -- | The list of all tags associated with this configuration.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Required. The description of the configuration.
    description :: Prelude.Text,
    -- | Required. The broker engine\'s version. For a list of supported engine
    -- versions, see,
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
    engineVersion :: Prelude.Text,
    -- | Required. The latest revision of the configuration.
    latestRevision :: ConfigurationRevision,
    -- | Optional. The authentication strategy associated with the configuration.
    -- The default is SIMPLE.
    authenticationStrategy :: AuthenticationStrategy,
    -- | Required. The type of broker engine. Currently, Amazon MQ supports
    -- ACTIVEMQ and RABBITMQ.
    engineType :: EngineType,
    -- | Required. The unique ID that Amazon MQ generates for the configuration.
    id :: Prelude.Text,
    -- | Required. The ARN of the configuration.
    arn :: Prelude.Text,
    -- | Required. The name of the configuration. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 1-150 characters long.
    name :: Prelude.Text,
    -- | Required. The date and time of the configuration revision.
    created :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'configuration_tags' - The list of all tags associated with this configuration.
--
-- 'description', 'configuration_description' - Required. The description of the configuration.
--
-- 'engineVersion', 'configuration_engineVersion' - Required. The broker engine\'s version. For a list of supported engine
-- versions, see,
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
--
-- 'latestRevision', 'configuration_latestRevision' - Required. The latest revision of the configuration.
--
-- 'authenticationStrategy', 'configuration_authenticationStrategy' - Optional. The authentication strategy associated with the configuration.
-- The default is SIMPLE.
--
-- 'engineType', 'configuration_engineType' - Required. The type of broker engine. Currently, Amazon MQ supports
-- ACTIVEMQ and RABBITMQ.
--
-- 'id', 'configuration_id' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- 'arn', 'configuration_arn' - Required. The ARN of the configuration.
--
-- 'name', 'configuration_name' - Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
--
-- 'created', 'configuration_created' - Required. The date and time of the configuration revision.
newConfiguration ::
  -- | 'description'
  Prelude.Text ->
  -- | 'engineVersion'
  Prelude.Text ->
  -- | 'latestRevision'
  ConfigurationRevision ->
  -- | 'authenticationStrategy'
  AuthenticationStrategy ->
  -- | 'engineType'
  EngineType ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'created'
  Prelude.UTCTime ->
  Configuration
newConfiguration
  pDescription_
  pEngineVersion_
  pLatestRevision_
  pAuthenticationStrategy_
  pEngineType_
  pId_
  pArn_
  pName_
  pCreated_ =
    Configuration'
      { tags = Prelude.Nothing,
        description = pDescription_,
        engineVersion = pEngineVersion_,
        latestRevision = pLatestRevision_,
        authenticationStrategy = pAuthenticationStrategy_,
        engineType = pEngineType_,
        id = pId_,
        arn = pArn_,
        name = pName_,
        created = Core._Time Lens.# pCreated_
      }

-- | The list of all tags associated with this configuration.
configuration_tags :: Lens.Lens' Configuration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
configuration_tags = Lens.lens (\Configuration' {tags} -> tags) (\s@Configuration' {} a -> s {tags = a} :: Configuration) Prelude.. Lens.mapping Lens.coerced

-- | Required. The description of the configuration.
configuration_description :: Lens.Lens' Configuration Prelude.Text
configuration_description = Lens.lens (\Configuration' {description} -> description) (\s@Configuration' {} a -> s {description = a} :: Configuration)

-- | Required. The broker engine\'s version. For a list of supported engine
-- versions, see,
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
configuration_engineVersion :: Lens.Lens' Configuration Prelude.Text
configuration_engineVersion = Lens.lens (\Configuration' {engineVersion} -> engineVersion) (\s@Configuration' {} a -> s {engineVersion = a} :: Configuration)

-- | Required. The latest revision of the configuration.
configuration_latestRevision :: Lens.Lens' Configuration ConfigurationRevision
configuration_latestRevision = Lens.lens (\Configuration' {latestRevision} -> latestRevision) (\s@Configuration' {} a -> s {latestRevision = a} :: Configuration)

-- | Optional. The authentication strategy associated with the configuration.
-- The default is SIMPLE.
configuration_authenticationStrategy :: Lens.Lens' Configuration AuthenticationStrategy
configuration_authenticationStrategy = Lens.lens (\Configuration' {authenticationStrategy} -> authenticationStrategy) (\s@Configuration' {} a -> s {authenticationStrategy = a} :: Configuration)

-- | Required. The type of broker engine. Currently, Amazon MQ supports
-- ACTIVEMQ and RABBITMQ.
configuration_engineType :: Lens.Lens' Configuration EngineType
configuration_engineType = Lens.lens (\Configuration' {engineType} -> engineType) (\s@Configuration' {} a -> s {engineType = a} :: Configuration)

-- | Required. The unique ID that Amazon MQ generates for the configuration.
configuration_id :: Lens.Lens' Configuration Prelude.Text
configuration_id = Lens.lens (\Configuration' {id} -> id) (\s@Configuration' {} a -> s {id = a} :: Configuration)

-- | Required. The ARN of the configuration.
configuration_arn :: Lens.Lens' Configuration Prelude.Text
configuration_arn = Lens.lens (\Configuration' {arn} -> arn) (\s@Configuration' {} a -> s {arn = a} :: Configuration)

-- | Required. The name of the configuration. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 1-150 characters long.
configuration_name :: Lens.Lens' Configuration Prelude.Text
configuration_name = Lens.lens (\Configuration' {name} -> name) (\s@Configuration' {} a -> s {name = a} :: Configuration)

-- | Required. The date and time of the configuration revision.
configuration_created :: Lens.Lens' Configuration Prelude.UTCTime
configuration_created = Lens.lens (\Configuration' {created} -> created) (\s@Configuration' {} a -> s {created = a} :: Configuration) Prelude.. Core._Time

instance Core.FromJSON Configuration where
  parseJSON =
    Core.withObject
      "Configuration"
      ( \x ->
          Configuration'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "description")
            Prelude.<*> (x Core..: "engineVersion")
            Prelude.<*> (x Core..: "latestRevision")
            Prelude.<*> (x Core..: "authenticationStrategy")
            Prelude.<*> (x Core..: "engineType")
            Prelude.<*> (x Core..: "id")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "created")
      )

instance Prelude.Hashable Configuration where
  hashWithSalt _salt Configuration' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` latestRevision
      `Prelude.hashWithSalt` authenticationStrategy
      `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` created

instance Prelude.NFData Configuration where
  rnf Configuration' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf latestRevision
      `Prelude.seq` Prelude.rnf authenticationStrategy
      `Prelude.seq` Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf created
