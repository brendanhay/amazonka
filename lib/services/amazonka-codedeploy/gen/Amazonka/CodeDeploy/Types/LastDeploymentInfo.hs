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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The unique ID of a deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The status of the most recent deployment.
    status :: Prelude.Maybe DeploymentStatus,
    -- | A timestamp that indicates when the most recent deployment to the
    -- deployment group was complete.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | A timestamp that indicates when the most recent deployment to the
    -- deployment group started.
    createTime :: Prelude.Maybe Data.POSIX
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
-- 'deploymentId', 'lastDeploymentInfo_deploymentId' - The unique ID of a deployment.
--
-- 'status', 'lastDeploymentInfo_status' - The status of the most recent deployment.
--
-- 'endTime', 'lastDeploymentInfo_endTime' - A timestamp that indicates when the most recent deployment to the
-- deployment group was complete.
--
-- 'createTime', 'lastDeploymentInfo_createTime' - A timestamp that indicates when the most recent deployment to the
-- deployment group started.
newLastDeploymentInfo ::
  LastDeploymentInfo
newLastDeploymentInfo =
  LastDeploymentInfo'
    { deploymentId = Prelude.Nothing,
      status = Prelude.Nothing,
      endTime = Prelude.Nothing,
      createTime = Prelude.Nothing
    }

-- | The unique ID of a deployment.
lastDeploymentInfo_deploymentId :: Lens.Lens' LastDeploymentInfo (Prelude.Maybe Prelude.Text)
lastDeploymentInfo_deploymentId = Lens.lens (\LastDeploymentInfo' {deploymentId} -> deploymentId) (\s@LastDeploymentInfo' {} a -> s {deploymentId = a} :: LastDeploymentInfo)

-- | The status of the most recent deployment.
lastDeploymentInfo_status :: Lens.Lens' LastDeploymentInfo (Prelude.Maybe DeploymentStatus)
lastDeploymentInfo_status = Lens.lens (\LastDeploymentInfo' {status} -> status) (\s@LastDeploymentInfo' {} a -> s {status = a} :: LastDeploymentInfo)

-- | A timestamp that indicates when the most recent deployment to the
-- deployment group was complete.
lastDeploymentInfo_endTime :: Lens.Lens' LastDeploymentInfo (Prelude.Maybe Prelude.UTCTime)
lastDeploymentInfo_endTime = Lens.lens (\LastDeploymentInfo' {endTime} -> endTime) (\s@LastDeploymentInfo' {} a -> s {endTime = a} :: LastDeploymentInfo) Prelude.. Lens.mapping Data._Time

-- | A timestamp that indicates when the most recent deployment to the
-- deployment group started.
lastDeploymentInfo_createTime :: Lens.Lens' LastDeploymentInfo (Prelude.Maybe Prelude.UTCTime)
lastDeploymentInfo_createTime = Lens.lens (\LastDeploymentInfo' {createTime} -> createTime) (\s@LastDeploymentInfo' {} a -> s {createTime = a} :: LastDeploymentInfo) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON LastDeploymentInfo where
  parseJSON =
    Data.withObject
      "LastDeploymentInfo"
      ( \x ->
          LastDeploymentInfo'
            Prelude.<$> (x Data..:? "deploymentId")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "createTime")
      )

instance Prelude.Hashable LastDeploymentInfo where
  hashWithSalt _salt LastDeploymentInfo' {..} =
    _salt `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` createTime

instance Prelude.NFData LastDeploymentInfo where
  rnf LastDeploymentInfo' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf createTime
