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
-- Module      : Amazonka.EMRServerless.Types.Application
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.Application where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRServerless.Types.ApplicationState
import Amazonka.EMRServerless.Types.Architecture
import Amazonka.EMRServerless.Types.AutoStartConfig
import Amazonka.EMRServerless.Types.AutoStopConfig
import Amazonka.EMRServerless.Types.InitialCapacityConfig
import Amazonka.EMRServerless.Types.MaximumAllowedResources
import Amazonka.EMRServerless.Types.NetworkConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Information about an application. EMR Serverless uses applications to
-- run jobs.
--
-- /See:/ 'newApplication' smart constructor.
data Application = Application'
  { -- | The CPU architecture of an application.
    architecture :: Prelude.Maybe Architecture,
    -- | The configuration for an application to automatically start on job
    -- submission.
    autoStartConfiguration :: Prelude.Maybe AutoStartConfig,
    -- | The configuration for an application to automatically stop after a
    -- certain amount of time being idle.
    autoStopConfiguration :: Prelude.Maybe AutoStopConfig,
    -- | The initial capacity of the application.
    initialCapacity :: Prelude.Maybe (Prelude.HashMap Prelude.Text InitialCapacityConfig),
    -- | The maximum capacity of the application. This is cumulative across all
    -- workers at any given point in time during the lifespan of the
    -- application is created. No new resources will be created once any one of
    -- the defined limits is hit.
    maximumCapacity :: Prelude.Maybe MaximumAllowedResources,
    -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The network configuration for customer VPC connectivity for the
    -- application.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The state details of the application.
    stateDetails :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the application.
    applicationId :: Prelude.Text,
    -- | The ARN of the application.
    arn :: Prelude.Text,
    -- | The EMR release version associated with the application.
    releaseLabel :: Prelude.Text,
    -- | The type of application, such as Spark or Hive.
    type' :: Prelude.Text,
    -- | The state of the application.
    state :: ApplicationState,
    -- | The date and time when the application run was created.
    createdAt :: Data.POSIX,
    -- | The date and time when the application run was last updated.
    updatedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Application' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architecture', 'application_architecture' - The CPU architecture of an application.
--
-- 'autoStartConfiguration', 'application_autoStartConfiguration' - The configuration for an application to automatically start on job
-- submission.
--
-- 'autoStopConfiguration', 'application_autoStopConfiguration' - The configuration for an application to automatically stop after a
-- certain amount of time being idle.
--
-- 'initialCapacity', 'application_initialCapacity' - The initial capacity of the application.
--
-- 'maximumCapacity', 'application_maximumCapacity' - The maximum capacity of the application. This is cumulative across all
-- workers at any given point in time during the lifespan of the
-- application is created. No new resources will be created once any one of
-- the defined limits is hit.
--
-- 'name', 'application_name' - The name of the application.
--
-- 'networkConfiguration', 'application_networkConfiguration' - The network configuration for customer VPC connectivity for the
-- application.
--
-- 'stateDetails', 'application_stateDetails' - The state details of the application.
--
-- 'tags', 'application_tags' - The tags assigned to the application.
--
-- 'applicationId', 'application_applicationId' - The ID of the application.
--
-- 'arn', 'application_arn' - The ARN of the application.
--
-- 'releaseLabel', 'application_releaseLabel' - The EMR release version associated with the application.
--
-- 'type'', 'application_type' - The type of application, such as Spark or Hive.
--
-- 'state', 'application_state' - The state of the application.
--
-- 'createdAt', 'application_createdAt' - The date and time when the application run was created.
--
-- 'updatedAt', 'application_updatedAt' - The date and time when the application run was last updated.
newApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'releaseLabel'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  -- | 'state'
  ApplicationState ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  Application
newApplication
  pApplicationId_
  pArn_
  pReleaseLabel_
  pType_
  pState_
  pCreatedAt_
  pUpdatedAt_ =
    Application'
      { architecture = Prelude.Nothing,
        autoStartConfiguration = Prelude.Nothing,
        autoStopConfiguration = Prelude.Nothing,
        initialCapacity = Prelude.Nothing,
        maximumCapacity = Prelude.Nothing,
        name = Prelude.Nothing,
        networkConfiguration = Prelude.Nothing,
        stateDetails = Prelude.Nothing,
        tags = Prelude.Nothing,
        applicationId = pApplicationId_,
        arn = pArn_,
        releaseLabel = pReleaseLabel_,
        type' = pType_,
        state = pState_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_
      }

-- | The CPU architecture of an application.
application_architecture :: Lens.Lens' Application (Prelude.Maybe Architecture)
application_architecture = Lens.lens (\Application' {architecture} -> architecture) (\s@Application' {} a -> s {architecture = a} :: Application)

-- | The configuration for an application to automatically start on job
-- submission.
application_autoStartConfiguration :: Lens.Lens' Application (Prelude.Maybe AutoStartConfig)
application_autoStartConfiguration = Lens.lens (\Application' {autoStartConfiguration} -> autoStartConfiguration) (\s@Application' {} a -> s {autoStartConfiguration = a} :: Application)

-- | The configuration for an application to automatically stop after a
-- certain amount of time being idle.
application_autoStopConfiguration :: Lens.Lens' Application (Prelude.Maybe AutoStopConfig)
application_autoStopConfiguration = Lens.lens (\Application' {autoStopConfiguration} -> autoStopConfiguration) (\s@Application' {} a -> s {autoStopConfiguration = a} :: Application)

-- | The initial capacity of the application.
application_initialCapacity :: Lens.Lens' Application (Prelude.Maybe (Prelude.HashMap Prelude.Text InitialCapacityConfig))
application_initialCapacity = Lens.lens (\Application' {initialCapacity} -> initialCapacity) (\s@Application' {} a -> s {initialCapacity = a} :: Application) Prelude.. Lens.mapping Lens.coerced

-- | The maximum capacity of the application. This is cumulative across all
-- workers at any given point in time during the lifespan of the
-- application is created. No new resources will be created once any one of
-- the defined limits is hit.
application_maximumCapacity :: Lens.Lens' Application (Prelude.Maybe MaximumAllowedResources)
application_maximumCapacity = Lens.lens (\Application' {maximumCapacity} -> maximumCapacity) (\s@Application' {} a -> s {maximumCapacity = a} :: Application)

-- | The name of the application.
application_name :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_name = Lens.lens (\Application' {name} -> name) (\s@Application' {} a -> s {name = a} :: Application)

-- | The network configuration for customer VPC connectivity for the
-- application.
application_networkConfiguration :: Lens.Lens' Application (Prelude.Maybe NetworkConfiguration)
application_networkConfiguration = Lens.lens (\Application' {networkConfiguration} -> networkConfiguration) (\s@Application' {} a -> s {networkConfiguration = a} :: Application)

-- | The state details of the application.
application_stateDetails :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_stateDetails = Lens.lens (\Application' {stateDetails} -> stateDetails) (\s@Application' {} a -> s {stateDetails = a} :: Application)

-- | The tags assigned to the application.
application_tags :: Lens.Lens' Application (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
application_tags = Lens.lens (\Application' {tags} -> tags) (\s@Application' {} a -> s {tags = a} :: Application) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the application.
application_applicationId :: Lens.Lens' Application Prelude.Text
application_applicationId = Lens.lens (\Application' {applicationId} -> applicationId) (\s@Application' {} a -> s {applicationId = a} :: Application)

-- | The ARN of the application.
application_arn :: Lens.Lens' Application Prelude.Text
application_arn = Lens.lens (\Application' {arn} -> arn) (\s@Application' {} a -> s {arn = a} :: Application)

-- | The EMR release version associated with the application.
application_releaseLabel :: Lens.Lens' Application Prelude.Text
application_releaseLabel = Lens.lens (\Application' {releaseLabel} -> releaseLabel) (\s@Application' {} a -> s {releaseLabel = a} :: Application)

-- | The type of application, such as Spark or Hive.
application_type :: Lens.Lens' Application Prelude.Text
application_type = Lens.lens (\Application' {type'} -> type') (\s@Application' {} a -> s {type' = a} :: Application)

-- | The state of the application.
application_state :: Lens.Lens' Application ApplicationState
application_state = Lens.lens (\Application' {state} -> state) (\s@Application' {} a -> s {state = a} :: Application)

-- | The date and time when the application run was created.
application_createdAt :: Lens.Lens' Application Prelude.UTCTime
application_createdAt = Lens.lens (\Application' {createdAt} -> createdAt) (\s@Application' {} a -> s {createdAt = a} :: Application) Prelude.. Data._Time

-- | The date and time when the application run was last updated.
application_updatedAt :: Lens.Lens' Application Prelude.UTCTime
application_updatedAt = Lens.lens (\Application' {updatedAt} -> updatedAt) (\s@Application' {} a -> s {updatedAt = a} :: Application) Prelude.. Data._Time

instance Data.FromJSON Application where
  parseJSON =
    Data.withObject
      "Application"
      ( \x ->
          Application'
            Prelude.<$> (x Data..:? "architecture")
            Prelude.<*> (x Data..:? "autoStartConfiguration")
            Prelude.<*> (x Data..:? "autoStopConfiguration")
            Prelude.<*> ( x Data..:? "initialCapacity"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "maximumCapacity")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "networkConfiguration")
            Prelude.<*> (x Data..:? "stateDetails")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "applicationId")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "releaseLabel")
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "state")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "updatedAt")
      )

instance Prelude.Hashable Application where
  hashWithSalt _salt Application' {..} =
    _salt `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` autoStartConfiguration
      `Prelude.hashWithSalt` autoStopConfiguration
      `Prelude.hashWithSalt` initialCapacity
      `Prelude.hashWithSalt` maximumCapacity
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` stateDetails
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` releaseLabel
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Application where
  rnf Application' {..} =
    Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf autoStartConfiguration
      `Prelude.seq` Prelude.rnf autoStopConfiguration
      `Prelude.seq` Prelude.rnf initialCapacity
      `Prelude.seq` Prelude.rnf maximumCapacity
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf stateDetails
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf releaseLabel
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
