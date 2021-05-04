{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Greengrass.Types.BulkDeploymentResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.BulkDeploymentResult where

import Network.AWS.Greengrass.Types.DeploymentType
import Network.AWS.Greengrass.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an individual group deployment in a bulk deployment
-- operation.
--
-- /See:/ 'newBulkDeploymentResult' smart constructor.
data BulkDeploymentResult = BulkDeploymentResult'
  { -- | The ID of the group deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The type of the deployment.
    deploymentType :: Prelude.Maybe DeploymentType,
    -- | The current status of the group deployment: \'\'InProgress\'\',
    -- \'\'Building\'\', \'\'Success\'\', or \'\'Failure\'\'.
    deploymentStatus :: Prelude.Maybe Prelude.Text,
    -- | The time, in ISO format, when the deployment was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the group deployment.
    deploymentArn :: Prelude.Maybe Prelude.Text,
    -- | The error message for a failed deployment
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Greengrass group.
    groupArn :: Prelude.Maybe Prelude.Text,
    -- | Details about the error.
    errorDetails :: Prelude.Maybe [ErrorDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BulkDeploymentResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'bulkDeploymentResult_deploymentId' - The ID of the group deployment.
--
-- 'deploymentType', 'bulkDeploymentResult_deploymentType' - The type of the deployment.
--
-- 'deploymentStatus', 'bulkDeploymentResult_deploymentStatus' - The current status of the group deployment: \'\'InProgress\'\',
-- \'\'Building\'\', \'\'Success\'\', or \'\'Failure\'\'.
--
-- 'createdAt', 'bulkDeploymentResult_createdAt' - The time, in ISO format, when the deployment was created.
--
-- 'deploymentArn', 'bulkDeploymentResult_deploymentArn' - The ARN of the group deployment.
--
-- 'errorMessage', 'bulkDeploymentResult_errorMessage' - The error message for a failed deployment
--
-- 'groupArn', 'bulkDeploymentResult_groupArn' - The ARN of the Greengrass group.
--
-- 'errorDetails', 'bulkDeploymentResult_errorDetails' - Details about the error.
newBulkDeploymentResult ::
  BulkDeploymentResult
newBulkDeploymentResult =
  BulkDeploymentResult'
    { deploymentId =
        Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      deploymentArn = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      groupArn = Prelude.Nothing,
      errorDetails = Prelude.Nothing
    }

-- | The ID of the group deployment.
bulkDeploymentResult_deploymentId :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe Prelude.Text)
bulkDeploymentResult_deploymentId = Lens.lens (\BulkDeploymentResult' {deploymentId} -> deploymentId) (\s@BulkDeploymentResult' {} a -> s {deploymentId = a} :: BulkDeploymentResult)

-- | The type of the deployment.
bulkDeploymentResult_deploymentType :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe DeploymentType)
bulkDeploymentResult_deploymentType = Lens.lens (\BulkDeploymentResult' {deploymentType} -> deploymentType) (\s@BulkDeploymentResult' {} a -> s {deploymentType = a} :: BulkDeploymentResult)

-- | The current status of the group deployment: \'\'InProgress\'\',
-- \'\'Building\'\', \'\'Success\'\', or \'\'Failure\'\'.
bulkDeploymentResult_deploymentStatus :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe Prelude.Text)
bulkDeploymentResult_deploymentStatus = Lens.lens (\BulkDeploymentResult' {deploymentStatus} -> deploymentStatus) (\s@BulkDeploymentResult' {} a -> s {deploymentStatus = a} :: BulkDeploymentResult)

-- | The time, in ISO format, when the deployment was created.
bulkDeploymentResult_createdAt :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe Prelude.Text)
bulkDeploymentResult_createdAt = Lens.lens (\BulkDeploymentResult' {createdAt} -> createdAt) (\s@BulkDeploymentResult' {} a -> s {createdAt = a} :: BulkDeploymentResult)

-- | The ARN of the group deployment.
bulkDeploymentResult_deploymentArn :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe Prelude.Text)
bulkDeploymentResult_deploymentArn = Lens.lens (\BulkDeploymentResult' {deploymentArn} -> deploymentArn) (\s@BulkDeploymentResult' {} a -> s {deploymentArn = a} :: BulkDeploymentResult)

-- | The error message for a failed deployment
bulkDeploymentResult_errorMessage :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe Prelude.Text)
bulkDeploymentResult_errorMessage = Lens.lens (\BulkDeploymentResult' {errorMessage} -> errorMessage) (\s@BulkDeploymentResult' {} a -> s {errorMessage = a} :: BulkDeploymentResult)

-- | The ARN of the Greengrass group.
bulkDeploymentResult_groupArn :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe Prelude.Text)
bulkDeploymentResult_groupArn = Lens.lens (\BulkDeploymentResult' {groupArn} -> groupArn) (\s@BulkDeploymentResult' {} a -> s {groupArn = a} :: BulkDeploymentResult)

-- | Details about the error.
bulkDeploymentResult_errorDetails :: Lens.Lens' BulkDeploymentResult (Prelude.Maybe [ErrorDetail])
bulkDeploymentResult_errorDetails = Lens.lens (\BulkDeploymentResult' {errorDetails} -> errorDetails) (\s@BulkDeploymentResult' {} a -> s {errorDetails = a} :: BulkDeploymentResult) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON BulkDeploymentResult where
  parseJSON =
    Prelude.withObject
      "BulkDeploymentResult"
      ( \x ->
          BulkDeploymentResult'
            Prelude.<$> (x Prelude..:? "DeploymentId")
            Prelude.<*> (x Prelude..:? "DeploymentType")
            Prelude.<*> (x Prelude..:? "DeploymentStatus")
            Prelude.<*> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "DeploymentArn")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "GroupArn")
            Prelude.<*> ( x Prelude..:? "ErrorDetails"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BulkDeploymentResult

instance Prelude.NFData BulkDeploymentResult
