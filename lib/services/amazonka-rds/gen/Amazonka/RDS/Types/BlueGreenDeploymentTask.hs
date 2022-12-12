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
-- Module      : Amazonka.RDS.Types.BlueGreenDeploymentTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.BlueGreenDeploymentTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the details about a task for a blue\/green deployment.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon Aurora User Guide/.
--
-- /See:/ 'newBlueGreenDeploymentTask' smart constructor.
data BlueGreenDeploymentTask = BlueGreenDeploymentTask'
  { -- | The name of the blue\/green deployment task.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the blue\/green deployment task.
    --
    -- Values:
    --
    -- -   @PENDING@ - The resources are being prepared for deployment.
    --
    -- -   @IN_PROGRESS@ - The resource is being deployed.
    --
    -- -   @COMPLETED@ - The resource has been deployed.
    --
    -- -   @FAILED@ - Deployment of the resource failed.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlueGreenDeploymentTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'blueGreenDeploymentTask_name' - The name of the blue\/green deployment task.
--
-- 'status', 'blueGreenDeploymentTask_status' - The status of the blue\/green deployment task.
--
-- Values:
--
-- -   @PENDING@ - The resources are being prepared for deployment.
--
-- -   @IN_PROGRESS@ - The resource is being deployed.
--
-- -   @COMPLETED@ - The resource has been deployed.
--
-- -   @FAILED@ - Deployment of the resource failed.
newBlueGreenDeploymentTask ::
  BlueGreenDeploymentTask
newBlueGreenDeploymentTask =
  BlueGreenDeploymentTask'
    { name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the blue\/green deployment task.
blueGreenDeploymentTask_name :: Lens.Lens' BlueGreenDeploymentTask (Prelude.Maybe Prelude.Text)
blueGreenDeploymentTask_name = Lens.lens (\BlueGreenDeploymentTask' {name} -> name) (\s@BlueGreenDeploymentTask' {} a -> s {name = a} :: BlueGreenDeploymentTask)

-- | The status of the blue\/green deployment task.
--
-- Values:
--
-- -   @PENDING@ - The resources are being prepared for deployment.
--
-- -   @IN_PROGRESS@ - The resource is being deployed.
--
-- -   @COMPLETED@ - The resource has been deployed.
--
-- -   @FAILED@ - Deployment of the resource failed.
blueGreenDeploymentTask_status :: Lens.Lens' BlueGreenDeploymentTask (Prelude.Maybe Prelude.Text)
blueGreenDeploymentTask_status = Lens.lens (\BlueGreenDeploymentTask' {status} -> status) (\s@BlueGreenDeploymentTask' {} a -> s {status = a} :: BlueGreenDeploymentTask)

instance Data.FromXML BlueGreenDeploymentTask where
  parseXML x =
    BlueGreenDeploymentTask'
      Prelude.<$> (x Data..@? "Name") Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable BlueGreenDeploymentTask where
  hashWithSalt _salt BlueGreenDeploymentTask' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData BlueGreenDeploymentTask where
  rnf BlueGreenDeploymentTask' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf status
