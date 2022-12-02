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
-- Module      : Amazonka.Greengrass.Types.BulkDeploymentResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.BulkDeploymentResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.DeploymentType
import Amazonka.Greengrass.Types.ErrorDetail
import qualified Amazonka.Prelude as Prelude

-- | Information about an individual group deployment in a bulk deployment
-- operation.
--
-- /See:/ 'newBulkDeploymentResult' smart constructor.
data BulkDeploymentResult = BulkDeploymentResult'
  { -- | The current status of the group deployment: \'\'InProgress\'\',
    -- \'\'Building\'\', \'\'Success\'\', or \'\'Failure\'\'.
    deploymentStatus :: Prelude.Maybe Prelude.Text,
    -- | The error message for a failed deployment
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the group deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | Details about the error.
    errorDetails :: Prelude.Maybe [ErrorDetail],
    -- | The type of the deployment.
    deploymentType :: Prelude.Maybe DeploymentType,
    -- | The ARN of the group deployment.
    deploymentArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Greengrass group.
    groupArn :: Prelude.Maybe Prelude.Text,
    -- | The time, in ISO format, when the deployment was created.
    createdAt :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BulkDeploymentResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentStatus', 'bulkDeploymentResult_deploymentStatus' - The current status of the group deployment: \'\'InProgress\'\',
-- \'\'Building\'\', \'\'Success\'\', or \'\'Failure\'\'.
--
-- 'errorMessage', 'bulkDeploymentResult_errorMessage' - The error message for a failed deployment
--
-- 'deploymentId', 'bulkDeploymentResult_deploymentId' - The ID of the group deployment.
--
-- 'errorDetails', 'bulkDeploymentResult_errorDetails' - Details about the error.
--
-- 'deploymentType', 'bulkDeploymentResult_deploymentType' - The type of the deployment.
--
-- 'deploymentArn', 'bulkDeploymentResult_deploymentArn' - The ARN of the group deployment.
--
-- 'groupArn', 'bulkDeploymentResult_groupArn' - The ARN of the Greengrass group.
--
-- 'createdAt', 'bulkDeploymentResult_createdAt' - The time, in ISO format, when the deployment was created.
newBulkDeploymentResult ::
  BulkDeploymentResult
newBulkDeploymentResult =
  BulkDeploymentResult'
    { deploymentStatus =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      deploymentArn = Prelude.Nothing,
      groupArn = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The current status of the group deployment: \'\'InProgress\'\',
-- \'\'Building\'\', \'\'Success\'\', or \'\'Failure\'\'.
bulkDeploymentResult_deploymentStatus :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe Prelude.Text)
bulkDeploymentResult_deploymentStatus = Lens.lens (\BulkDeploymentResult' {deploymentStatus} -> deploymentStatus) (\s@BulkDeploymentResult' {} a -> s {deploymentStatus = a} :: BulkDeploymentResult)

-- | The error message for a failed deployment
bulkDeploymentResult_errorMessage :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe Prelude.Text)
bulkDeploymentResult_errorMessage = Lens.lens (\BulkDeploymentResult' {errorMessage} -> errorMessage) (\s@BulkDeploymentResult' {} a -> s {errorMessage = a} :: BulkDeploymentResult)

-- | The ID of the group deployment.
bulkDeploymentResult_deploymentId :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe Prelude.Text)
bulkDeploymentResult_deploymentId = Lens.lens (\BulkDeploymentResult' {deploymentId} -> deploymentId) (\s@BulkDeploymentResult' {} a -> s {deploymentId = a} :: BulkDeploymentResult)

-- | Details about the error.
bulkDeploymentResult_errorDetails :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe [ErrorDetail])
bulkDeploymentResult_errorDetails = Lens.lens (\BulkDeploymentResult' {errorDetails} -> errorDetails) (\s@BulkDeploymentResult' {} a -> s {errorDetails = a} :: BulkDeploymentResult) Prelude.. Lens.mapping Lens.coerced

-- | The type of the deployment.
bulkDeploymentResult_deploymentType :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe DeploymentType)
bulkDeploymentResult_deploymentType = Lens.lens (\BulkDeploymentResult' {deploymentType} -> deploymentType) (\s@BulkDeploymentResult' {} a -> s {deploymentType = a} :: BulkDeploymentResult)

-- | The ARN of the group deployment.
bulkDeploymentResult_deploymentArn :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe Prelude.Text)
bulkDeploymentResult_deploymentArn = Lens.lens (\BulkDeploymentResult' {deploymentArn} -> deploymentArn) (\s@BulkDeploymentResult' {} a -> s {deploymentArn = a} :: BulkDeploymentResult)

-- | The ARN of the Greengrass group.
bulkDeploymentResult_groupArn :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe Prelude.Text)
bulkDeploymentResult_groupArn = Lens.lens (\BulkDeploymentResult' {groupArn} -> groupArn) (\s@BulkDeploymentResult' {} a -> s {groupArn = a} :: BulkDeploymentResult)

-- | The time, in ISO format, when the deployment was created.
bulkDeploymentResult_createdAt :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe Prelude.Text)
bulkDeploymentResult_createdAt = Lens.lens (\BulkDeploymentResult' {createdAt} -> createdAt) (\s@BulkDeploymentResult' {} a -> s {createdAt = a} :: BulkDeploymentResult)

instance Data.FromJSON BulkDeploymentResult where
  parseJSON =
    Data.withObject
      "BulkDeploymentResult"
      ( \x ->
          BulkDeploymentResult'
            Prelude.<$> (x Data..:? "DeploymentStatus")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "DeploymentId")
            Prelude.<*> (x Data..:? "ErrorDetails" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "DeploymentType")
            Prelude.<*> (x Data..:? "DeploymentArn")
            Prelude.<*> (x Data..:? "GroupArn")
            Prelude.<*> (x Data..:? "CreatedAt")
      )

instance Prelude.Hashable BulkDeploymentResult where
  hashWithSalt _salt BulkDeploymentResult' {..} =
    _salt `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` errorDetails
      `Prelude.hashWithSalt` deploymentType
      `Prelude.hashWithSalt` deploymentArn
      `Prelude.hashWithSalt` groupArn
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData BulkDeploymentResult where
  rnf BulkDeploymentResult' {..} =
    Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf deploymentArn
      `Prelude.seq` Prelude.rnf groupArn
      `Prelude.seq` Prelude.rnf createdAt
