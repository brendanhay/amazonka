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
-- Module      : Amazonka.SageMakerEdge.Types.EdgeDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerEdge.Types.EdgeDeployment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerEdge.Types.Definition
import Amazonka.SageMakerEdge.Types.DeploymentType
import Amazonka.SageMakerEdge.Types.FailureHandlingPolicy

-- | Information about a deployment on an edge device that is registered with
-- SageMaker Edge Manager.
--
-- /See:/ 'newEdgeDeployment' smart constructor.
data EdgeDeployment = EdgeDeployment'
  { -- | Returns a list of Definition objects.
    definitions :: Prelude.Maybe [Definition],
    -- | The name and unique ID of the deployment.
    deploymentName :: Prelude.Maybe Prelude.Text,
    -- | Determines whether to rollback to previous configuration if deployment
    -- fails.
    failureHandlingPolicy :: Prelude.Maybe FailureHandlingPolicy,
    -- | The type of the deployment.
    type' :: Prelude.Maybe DeploymentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgeDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definitions', 'edgeDeployment_definitions' - Returns a list of Definition objects.
--
-- 'deploymentName', 'edgeDeployment_deploymentName' - The name and unique ID of the deployment.
--
-- 'failureHandlingPolicy', 'edgeDeployment_failureHandlingPolicy' - Determines whether to rollback to previous configuration if deployment
-- fails.
--
-- 'type'', 'edgeDeployment_type' - The type of the deployment.
newEdgeDeployment ::
  EdgeDeployment
newEdgeDeployment =
  EdgeDeployment'
    { definitions = Prelude.Nothing,
      deploymentName = Prelude.Nothing,
      failureHandlingPolicy = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Returns a list of Definition objects.
edgeDeployment_definitions :: Lens.Lens' EdgeDeployment (Prelude.Maybe [Definition])
edgeDeployment_definitions = Lens.lens (\EdgeDeployment' {definitions} -> definitions) (\s@EdgeDeployment' {} a -> s {definitions = a} :: EdgeDeployment) Prelude.. Lens.mapping Lens.coerced

-- | The name and unique ID of the deployment.
edgeDeployment_deploymentName :: Lens.Lens' EdgeDeployment (Prelude.Maybe Prelude.Text)
edgeDeployment_deploymentName = Lens.lens (\EdgeDeployment' {deploymentName} -> deploymentName) (\s@EdgeDeployment' {} a -> s {deploymentName = a} :: EdgeDeployment)

-- | Determines whether to rollback to previous configuration if deployment
-- fails.
edgeDeployment_failureHandlingPolicy :: Lens.Lens' EdgeDeployment (Prelude.Maybe FailureHandlingPolicy)
edgeDeployment_failureHandlingPolicy = Lens.lens (\EdgeDeployment' {failureHandlingPolicy} -> failureHandlingPolicy) (\s@EdgeDeployment' {} a -> s {failureHandlingPolicy = a} :: EdgeDeployment)

-- | The type of the deployment.
edgeDeployment_type :: Lens.Lens' EdgeDeployment (Prelude.Maybe DeploymentType)
edgeDeployment_type = Lens.lens (\EdgeDeployment' {type'} -> type') (\s@EdgeDeployment' {} a -> s {type' = a} :: EdgeDeployment)

instance Data.FromJSON EdgeDeployment where
  parseJSON =
    Data.withObject
      "EdgeDeployment"
      ( \x ->
          EdgeDeployment'
            Prelude.<$> (x Data..:? "Definitions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "DeploymentName")
            Prelude.<*> (x Data..:? "FailureHandlingPolicy")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable EdgeDeployment where
  hashWithSalt _salt EdgeDeployment' {..} =
    _salt `Prelude.hashWithSalt` definitions
      `Prelude.hashWithSalt` deploymentName
      `Prelude.hashWithSalt` failureHandlingPolicy
      `Prelude.hashWithSalt` type'

instance Prelude.NFData EdgeDeployment where
  rnf EdgeDeployment' {..} =
    Prelude.rnf definitions
      `Prelude.seq` Prelude.rnf deploymentName
      `Prelude.seq` Prelude.rnf failureHandlingPolicy
      `Prelude.seq` Prelude.rnf type'
