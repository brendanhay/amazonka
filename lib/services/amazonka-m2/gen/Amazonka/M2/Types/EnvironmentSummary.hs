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
-- Module      : Amazonka.M2.Types.EnvironmentSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.EnvironmentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.EngineType
import Amazonka.M2.Types.EnvironmentLifecycle
import qualified Amazonka.Prelude as Prelude

-- | Contains a subset of the possible runtime environment attributes. Used
-- in the environment list.
--
-- /See:/ 'newEnvironmentSummary' smart constructor.
data EnvironmentSummary = EnvironmentSummary'
  { -- | The timestamp when the runtime environment was created.
    creationTime :: Data.POSIX,
    -- | The target platform for the runtime environment.
    engineType :: EngineType,
    -- | The version of the runtime engine.
    engineVersion :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a particular runtime environment.
    environmentArn :: Prelude.Text,
    -- | The unique identifier of a particular runtime environment.
    environmentId :: Prelude.Text,
    -- | The instance type of the runtime environment.
    instanceType :: Prelude.Text,
    -- | The name of the runtime environment.
    name :: Prelude.Text,
    -- | The status of the runtime environment
    status :: EnvironmentLifecycle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'environmentSummary_creationTime' - The timestamp when the runtime environment was created.
--
-- 'engineType', 'environmentSummary_engineType' - The target platform for the runtime environment.
--
-- 'engineVersion', 'environmentSummary_engineVersion' - The version of the runtime engine.
--
-- 'environmentArn', 'environmentSummary_environmentArn' - The Amazon Resource Name (ARN) of a particular runtime environment.
--
-- 'environmentId', 'environmentSummary_environmentId' - The unique identifier of a particular runtime environment.
--
-- 'instanceType', 'environmentSummary_instanceType' - The instance type of the runtime environment.
--
-- 'name', 'environmentSummary_name' - The name of the runtime environment.
--
-- 'status', 'environmentSummary_status' - The status of the runtime environment
newEnvironmentSummary ::
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'engineType'
  EngineType ->
  -- | 'engineVersion'
  Prelude.Text ->
  -- | 'environmentArn'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'instanceType'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  EnvironmentLifecycle ->
  EnvironmentSummary
newEnvironmentSummary
  pCreationTime_
  pEngineType_
  pEngineVersion_
  pEnvironmentArn_
  pEnvironmentId_
  pInstanceType_
  pName_
  pStatus_ =
    EnvironmentSummary'
      { creationTime =
          Data._Time Lens.# pCreationTime_,
        engineType = pEngineType_,
        engineVersion = pEngineVersion_,
        environmentArn = pEnvironmentArn_,
        environmentId = pEnvironmentId_,
        instanceType = pInstanceType_,
        name = pName_,
        status = pStatus_
      }

-- | The timestamp when the runtime environment was created.
environmentSummary_creationTime :: Lens.Lens' EnvironmentSummary Prelude.UTCTime
environmentSummary_creationTime = Lens.lens (\EnvironmentSummary' {creationTime} -> creationTime) (\s@EnvironmentSummary' {} a -> s {creationTime = a} :: EnvironmentSummary) Prelude.. Data._Time

-- | The target platform for the runtime environment.
environmentSummary_engineType :: Lens.Lens' EnvironmentSummary EngineType
environmentSummary_engineType = Lens.lens (\EnvironmentSummary' {engineType} -> engineType) (\s@EnvironmentSummary' {} a -> s {engineType = a} :: EnvironmentSummary)

-- | The version of the runtime engine.
environmentSummary_engineVersion :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_engineVersion = Lens.lens (\EnvironmentSummary' {engineVersion} -> engineVersion) (\s@EnvironmentSummary' {} a -> s {engineVersion = a} :: EnvironmentSummary)

-- | The Amazon Resource Name (ARN) of a particular runtime environment.
environmentSummary_environmentArn :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_environmentArn = Lens.lens (\EnvironmentSummary' {environmentArn} -> environmentArn) (\s@EnvironmentSummary' {} a -> s {environmentArn = a} :: EnvironmentSummary)

-- | The unique identifier of a particular runtime environment.
environmentSummary_environmentId :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_environmentId = Lens.lens (\EnvironmentSummary' {environmentId} -> environmentId) (\s@EnvironmentSummary' {} a -> s {environmentId = a} :: EnvironmentSummary)

-- | The instance type of the runtime environment.
environmentSummary_instanceType :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_instanceType = Lens.lens (\EnvironmentSummary' {instanceType} -> instanceType) (\s@EnvironmentSummary' {} a -> s {instanceType = a} :: EnvironmentSummary)

-- | The name of the runtime environment.
environmentSummary_name :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_name = Lens.lens (\EnvironmentSummary' {name} -> name) (\s@EnvironmentSummary' {} a -> s {name = a} :: EnvironmentSummary)

-- | The status of the runtime environment
environmentSummary_status :: Lens.Lens' EnvironmentSummary EnvironmentLifecycle
environmentSummary_status = Lens.lens (\EnvironmentSummary' {status} -> status) (\s@EnvironmentSummary' {} a -> s {status = a} :: EnvironmentSummary)

instance Data.FromJSON EnvironmentSummary where
  parseJSON =
    Data.withObject
      "EnvironmentSummary"
      ( \x ->
          EnvironmentSummary'
            Prelude.<$> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "engineType")
            Prelude.<*> (x Data..: "engineVersion")
            Prelude.<*> (x Data..: "environmentArn")
            Prelude.<*> (x Data..: "environmentId")
            Prelude.<*> (x Data..: "instanceType")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable EnvironmentSummary where
  hashWithSalt _salt EnvironmentSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` environmentArn
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData EnvironmentSummary where
  rnf EnvironmentSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf environmentArn
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
