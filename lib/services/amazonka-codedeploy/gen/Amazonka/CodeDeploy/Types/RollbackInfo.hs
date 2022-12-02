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
-- Module      : Amazonka.CodeDeploy.Types.RollbackInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.RollbackInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a deployment rollback.
--
-- /See:/ 'newRollbackInfo' smart constructor.
data RollbackInfo = RollbackInfo'
  { -- | Information that describes the status of a deployment rollback (for
    -- example, whether the deployment can\'t be rolled back, is in progress,
    -- failed, or succeeded).
    rollbackMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the deployment rollback.
    rollbackDeploymentId :: Prelude.Maybe Prelude.Text,
    -- | The deployment ID of the deployment that was underway and triggered a
    -- rollback deployment because it failed or was stopped.
    rollbackTriggeringDeploymentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rollbackMessage', 'rollbackInfo_rollbackMessage' - Information that describes the status of a deployment rollback (for
-- example, whether the deployment can\'t be rolled back, is in progress,
-- failed, or succeeded).
--
-- 'rollbackDeploymentId', 'rollbackInfo_rollbackDeploymentId' - The ID of the deployment rollback.
--
-- 'rollbackTriggeringDeploymentId', 'rollbackInfo_rollbackTriggeringDeploymentId' - The deployment ID of the deployment that was underway and triggered a
-- rollback deployment because it failed or was stopped.
newRollbackInfo ::
  RollbackInfo
newRollbackInfo =
  RollbackInfo'
    { rollbackMessage = Prelude.Nothing,
      rollbackDeploymentId = Prelude.Nothing,
      rollbackTriggeringDeploymentId = Prelude.Nothing
    }

-- | Information that describes the status of a deployment rollback (for
-- example, whether the deployment can\'t be rolled back, is in progress,
-- failed, or succeeded).
rollbackInfo_rollbackMessage :: Lens.Lens' RollbackInfo (Prelude.Maybe Prelude.Text)
rollbackInfo_rollbackMessage = Lens.lens (\RollbackInfo' {rollbackMessage} -> rollbackMessage) (\s@RollbackInfo' {} a -> s {rollbackMessage = a} :: RollbackInfo)

-- | The ID of the deployment rollback.
rollbackInfo_rollbackDeploymentId :: Lens.Lens' RollbackInfo (Prelude.Maybe Prelude.Text)
rollbackInfo_rollbackDeploymentId = Lens.lens (\RollbackInfo' {rollbackDeploymentId} -> rollbackDeploymentId) (\s@RollbackInfo' {} a -> s {rollbackDeploymentId = a} :: RollbackInfo)

-- | The deployment ID of the deployment that was underway and triggered a
-- rollback deployment because it failed or was stopped.
rollbackInfo_rollbackTriggeringDeploymentId :: Lens.Lens' RollbackInfo (Prelude.Maybe Prelude.Text)
rollbackInfo_rollbackTriggeringDeploymentId = Lens.lens (\RollbackInfo' {rollbackTriggeringDeploymentId} -> rollbackTriggeringDeploymentId) (\s@RollbackInfo' {} a -> s {rollbackTriggeringDeploymentId = a} :: RollbackInfo)

instance Data.FromJSON RollbackInfo where
  parseJSON =
    Data.withObject
      "RollbackInfo"
      ( \x ->
          RollbackInfo'
            Prelude.<$> (x Data..:? "rollbackMessage")
            Prelude.<*> (x Data..:? "rollbackDeploymentId")
            Prelude.<*> (x Data..:? "rollbackTriggeringDeploymentId")
      )

instance Prelude.Hashable RollbackInfo where
  hashWithSalt _salt RollbackInfo' {..} =
    _salt `Prelude.hashWithSalt` rollbackMessage
      `Prelude.hashWithSalt` rollbackDeploymentId
      `Prelude.hashWithSalt` rollbackTriggeringDeploymentId

instance Prelude.NFData RollbackInfo where
  rnf RollbackInfo' {..} =
    Prelude.rnf rollbackMessage
      `Prelude.seq` Prelude.rnf rollbackDeploymentId
      `Prelude.seq` Prelude.rnf rollbackTriggeringDeploymentId
