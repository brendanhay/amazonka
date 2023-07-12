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
-- Module      : Amazonka.Amplify.Types.BackendEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.BackendEnvironment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the backend environment for an Amplify app.
--
-- /See:/ 'newBackendEnvironment' smart constructor.
data BackendEnvironment = BackendEnvironment'
  { -- | The name of deployment artifacts.
    deploymentArtifacts :: Prelude.Maybe Prelude.Text,
    -- | The AWS CloudFormation stack name of a backend environment.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for a backend environment that is part of
    -- an Amplify app.
    backendEnvironmentArn :: Prelude.Text,
    -- | The name for a backend environment that is part of an Amplify app.
    environmentName :: Prelude.Text,
    -- | The creation date and time for a backend environment that is part of an
    -- Amplify app.
    createTime :: Data.POSIX,
    -- | The last updated date and time for a backend environment that is part of
    -- an Amplify app.
    updateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackendEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentArtifacts', 'backendEnvironment_deploymentArtifacts' - The name of deployment artifacts.
--
-- 'stackName', 'backendEnvironment_stackName' - The AWS CloudFormation stack name of a backend environment.
--
-- 'backendEnvironmentArn', 'backendEnvironment_backendEnvironmentArn' - The Amazon Resource Name (ARN) for a backend environment that is part of
-- an Amplify app.
--
-- 'environmentName', 'backendEnvironment_environmentName' - The name for a backend environment that is part of an Amplify app.
--
-- 'createTime', 'backendEnvironment_createTime' - The creation date and time for a backend environment that is part of an
-- Amplify app.
--
-- 'updateTime', 'backendEnvironment_updateTime' - The last updated date and time for a backend environment that is part of
-- an Amplify app.
newBackendEnvironment ::
  -- | 'backendEnvironmentArn'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  BackendEnvironment
newBackendEnvironment
  pBackendEnvironmentArn_
  pEnvironmentName_
  pCreateTime_
  pUpdateTime_ =
    BackendEnvironment'
      { deploymentArtifacts =
          Prelude.Nothing,
        stackName = Prelude.Nothing,
        backendEnvironmentArn = pBackendEnvironmentArn_,
        environmentName = pEnvironmentName_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The name of deployment artifacts.
backendEnvironment_deploymentArtifacts :: Lens.Lens' BackendEnvironment (Prelude.Maybe Prelude.Text)
backendEnvironment_deploymentArtifacts = Lens.lens (\BackendEnvironment' {deploymentArtifacts} -> deploymentArtifacts) (\s@BackendEnvironment' {} a -> s {deploymentArtifacts = a} :: BackendEnvironment)

-- | The AWS CloudFormation stack name of a backend environment.
backendEnvironment_stackName :: Lens.Lens' BackendEnvironment (Prelude.Maybe Prelude.Text)
backendEnvironment_stackName = Lens.lens (\BackendEnvironment' {stackName} -> stackName) (\s@BackendEnvironment' {} a -> s {stackName = a} :: BackendEnvironment)

-- | The Amazon Resource Name (ARN) for a backend environment that is part of
-- an Amplify app.
backendEnvironment_backendEnvironmentArn :: Lens.Lens' BackendEnvironment Prelude.Text
backendEnvironment_backendEnvironmentArn = Lens.lens (\BackendEnvironment' {backendEnvironmentArn} -> backendEnvironmentArn) (\s@BackendEnvironment' {} a -> s {backendEnvironmentArn = a} :: BackendEnvironment)

-- | The name for a backend environment that is part of an Amplify app.
backendEnvironment_environmentName :: Lens.Lens' BackendEnvironment Prelude.Text
backendEnvironment_environmentName = Lens.lens (\BackendEnvironment' {environmentName} -> environmentName) (\s@BackendEnvironment' {} a -> s {environmentName = a} :: BackendEnvironment)

-- | The creation date and time for a backend environment that is part of an
-- Amplify app.
backendEnvironment_createTime :: Lens.Lens' BackendEnvironment Prelude.UTCTime
backendEnvironment_createTime = Lens.lens (\BackendEnvironment' {createTime} -> createTime) (\s@BackendEnvironment' {} a -> s {createTime = a} :: BackendEnvironment) Prelude.. Data._Time

-- | The last updated date and time for a backend environment that is part of
-- an Amplify app.
backendEnvironment_updateTime :: Lens.Lens' BackendEnvironment Prelude.UTCTime
backendEnvironment_updateTime = Lens.lens (\BackendEnvironment' {updateTime} -> updateTime) (\s@BackendEnvironment' {} a -> s {updateTime = a} :: BackendEnvironment) Prelude.. Data._Time

instance Data.FromJSON BackendEnvironment where
  parseJSON =
    Data.withObject
      "BackendEnvironment"
      ( \x ->
          BackendEnvironment'
            Prelude.<$> (x Data..:? "deploymentArtifacts")
            Prelude.<*> (x Data..:? "stackName")
            Prelude.<*> (x Data..: "backendEnvironmentArn")
            Prelude.<*> (x Data..: "environmentName")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
      )

instance Prelude.Hashable BackendEnvironment where
  hashWithSalt _salt BackendEnvironment' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentArtifacts
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` backendEnvironmentArn
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData BackendEnvironment where
  rnf BackendEnvironment' {..} =
    Prelude.rnf deploymentArtifacts
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf backendEnvironmentArn
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
