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
-- Module      : Amazonka.Kafka.Types.Configuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.Configuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.ConfigurationRevision
import Amazonka.Kafka.Types.ConfigurationState
import qualified Amazonka.Prelude as Prelude

-- | Represents an MSK Configuration.
--
-- /See:/ 'newConfiguration' smart constructor.
data Configuration = Configuration'
  { -- | The description of the configuration.
    description :: Prelude.Text,
    -- | Latest revision of the configuration.
    latestRevision :: ConfigurationRevision,
    -- | The time when the configuration was created.
    creationTime :: Data.ISO8601,
    -- | An array of the versions of Apache Kafka with which you can use this MSK
    -- configuration. You can use this configuration for an MSK cluster only if
    -- the Apache Kafka version specified for the cluster appears in this
    -- array.
    kafkaVersions :: [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the configuration.
    arn :: Prelude.Text,
    -- | The name of the configuration.
    name :: Prelude.Text,
    -- | The state of the configuration. The possible states are ACTIVE,
    -- DELETING, and DELETE_FAILED.
    state :: ConfigurationState
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
-- 'description', 'configuration_description' - The description of the configuration.
--
-- 'latestRevision', 'configuration_latestRevision' - Latest revision of the configuration.
--
-- 'creationTime', 'configuration_creationTime' - The time when the configuration was created.
--
-- 'kafkaVersions', 'configuration_kafkaVersions' - An array of the versions of Apache Kafka with which you can use this MSK
-- configuration. You can use this configuration for an MSK cluster only if
-- the Apache Kafka version specified for the cluster appears in this
-- array.
--
-- 'arn', 'configuration_arn' - The Amazon Resource Name (ARN) of the configuration.
--
-- 'name', 'configuration_name' - The name of the configuration.
--
-- 'state', 'configuration_state' - The state of the configuration. The possible states are ACTIVE,
-- DELETING, and DELETE_FAILED.
newConfiguration ::
  -- | 'description'
  Prelude.Text ->
  -- | 'latestRevision'
  ConfigurationRevision ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'state'
  ConfigurationState ->
  Configuration
newConfiguration
  pDescription_
  pLatestRevision_
  pCreationTime_
  pArn_
  pName_
  pState_ =
    Configuration'
      { description = pDescription_,
        latestRevision = pLatestRevision_,
        creationTime = Data._Time Lens.# pCreationTime_,
        kafkaVersions = Prelude.mempty,
        arn = pArn_,
        name = pName_,
        state = pState_
      }

-- | The description of the configuration.
configuration_description :: Lens.Lens' Configuration Prelude.Text
configuration_description = Lens.lens (\Configuration' {description} -> description) (\s@Configuration' {} a -> s {description = a} :: Configuration)

-- | Latest revision of the configuration.
configuration_latestRevision :: Lens.Lens' Configuration ConfigurationRevision
configuration_latestRevision = Lens.lens (\Configuration' {latestRevision} -> latestRevision) (\s@Configuration' {} a -> s {latestRevision = a} :: Configuration)

-- | The time when the configuration was created.
configuration_creationTime :: Lens.Lens' Configuration Prelude.UTCTime
configuration_creationTime = Lens.lens (\Configuration' {creationTime} -> creationTime) (\s@Configuration' {} a -> s {creationTime = a} :: Configuration) Prelude.. Data._Time

-- | An array of the versions of Apache Kafka with which you can use this MSK
-- configuration. You can use this configuration for an MSK cluster only if
-- the Apache Kafka version specified for the cluster appears in this
-- array.
configuration_kafkaVersions :: Lens.Lens' Configuration [Prelude.Text]
configuration_kafkaVersions = Lens.lens (\Configuration' {kafkaVersions} -> kafkaVersions) (\s@Configuration' {} a -> s {kafkaVersions = a} :: Configuration) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the configuration.
configuration_arn :: Lens.Lens' Configuration Prelude.Text
configuration_arn = Lens.lens (\Configuration' {arn} -> arn) (\s@Configuration' {} a -> s {arn = a} :: Configuration)

-- | The name of the configuration.
configuration_name :: Lens.Lens' Configuration Prelude.Text
configuration_name = Lens.lens (\Configuration' {name} -> name) (\s@Configuration' {} a -> s {name = a} :: Configuration)

-- | The state of the configuration. The possible states are ACTIVE,
-- DELETING, and DELETE_FAILED.
configuration_state :: Lens.Lens' Configuration ConfigurationState
configuration_state = Lens.lens (\Configuration' {state} -> state) (\s@Configuration' {} a -> s {state = a} :: Configuration)

instance Data.FromJSON Configuration where
  parseJSON =
    Data.withObject
      "Configuration"
      ( \x ->
          Configuration'
            Prelude.<$> (x Data..: "description")
            Prelude.<*> (x Data..: "latestRevision")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..:? "kafkaVersions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "state")
      )

instance Prelude.Hashable Configuration where
  hashWithSalt _salt Configuration' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` latestRevision
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` kafkaVersions
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state

instance Prelude.NFData Configuration where
  rnf Configuration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf latestRevision
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf kafkaVersions
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
