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
-- Module      : Amazonka.CodeDeploy.Types.LastDeploymentInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.LastDeploymentInfo where

import Amazonka.CodeDeploy.Types.DeploymentStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the most recent attempted or successful deployment to
-- a deployment group.
--
-- /See:/ 'newLastDeploymentInfo' smart constructor.
data LastDeploymentInfo = LastDeploymentInfo'
  { -- | A timestamp that indicates when the most recent deployment to the
    -- deployment group started.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | The unique ID of a deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the most recent deployment to the
    -- deployment group was complete.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the most recent deployment.
    status :: Prelude.Maybe DeploymentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LastDeploymentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTime', 'lastDeploymentInfo_createTime' - A timestamp that indicates when the most recent deployment to the
-- deployment group started.
--
-- 'deploymentId', 'lastDeploymentInfo_deploymentId' - The unique ID of a deployment.
--
-- 'endTime', 'lastDeploymentInfo_endTime' - A timestamp that indicates when the most recent deployment to the
-- deployment group was complete.
--
-- 'status', 'lastDeploymentInfo_status' - The status of the most recent deployment.
newLastDeploymentInfo ::
  LastDeploymentInfo
newLastDeploymentInfo =
  LastDeploymentInfo'
    { createTime = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      endTime = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | A timestamp that indicates when the most recent deployment to the
-- deployment group started.
lastDeploymentInfo_createTime :: Lens.Lens' LastDeploymentInfo (Prelude.Maybe Prelude.UTCTime)
lastDeploymentInfo_createTime = Lens.lens (\LastDeploymentInfo' {createTime} -> createTime) (\s@LastDeploymentInfo' {} a -> s {createTime = a} :: LastDeploymentInfo) Prelude.. Lens.mapping Data._Time

-- | The unique ID of a deployment.
lastDeploymentInfo_deploymentId :: Lens.Lens' LastDeploymentInfo (Prelude.Maybe Prelude.Text)
lastDeploymentInfo_deploymentId = Lens.lens (\LastDeploymentInfo' {deploymentId} -> deploymentId) (\s@LastDeploymentInfo' {} a -> s {deploymentId = a} :: LastDeploymentInfo)

-- | A timestamp that indicates when the most recent deployment to the
-- deployment group was complete.
lastDeploymentInfo_endTime :: Lens.Lens' LastDeploymentInfo (Prelude.Maybe Prelude.UTCTime)
lastDeploymentInfo_endTime = Lens.lens (\LastDeploymentInfo' {endTime} -> endTime) (\s@LastDeploymentInfo' {} a -> s {endTime = a} :: LastDeploymentInfo) Prelude.. Lens.mapping Data._Time

-- | The status of the most recent deployment.
lastDeploymentInfo_status :: Lens.Lens' LastDeploymentInfo (Prelude.Maybe DeploymentStatus)
lastDeploymentInfo_status = Lens.lens (\LastDeploymentInfo' {status} -> status) (\s@LastDeploymentInfo' {} a -> s {status = a} :: LastDeploymentInfo)

instance Data.FromJSON LastDeploymentInfo where
  parseJSON =
    Data.withObject
      "LastDeploymentInfo"
      ( \x ->
          LastDeploymentInfo'
            Prelude.<$> (x Data..:? "createTime")
            Prelude.<*> (x Data..:? "deploymentId")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable LastDeploymentInfo where
  hashWithSalt _salt LastDeploymentInfo' {..} =
    _salt
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData LastDeploymentInfo where
  rnf LastDeploymentInfo' {..} =
    Prelude.rnf createTime `Prelude.seq`
      Prelude.rnf deploymentId `Prelude.seq`
        Prelude.rnf endTime `Prelude.seq`
          Prelude.rnf status
