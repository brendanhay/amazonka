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
-- Module      : Amazonka.SageMaker.Types.EdgePresetDeploymentOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EdgePresetDeploymentOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EdgePresetDeploymentStatus
import Amazonka.SageMaker.Types.EdgePresetDeploymentType

-- | The output of a SageMaker Edge Manager deployable resource.
--
-- /See:/ 'newEdgePresetDeploymentOutput' smart constructor.
data EdgePresetDeploymentOutput = EdgePresetDeploymentOutput'
  { -- | The status of the deployable resource.
    status :: Prelude.Maybe EdgePresetDeploymentStatus,
    -- | The Amazon Resource Name (ARN) of the generated deployable resource.
    artifact :: Prelude.Maybe Prelude.Text,
    -- | Returns a message describing the status of the deployed resource.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The deployment type created by SageMaker Edge Manager. Currently only
    -- supports Amazon Web Services IoT Greengrass Version 2 components.
    type' :: EdgePresetDeploymentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgePresetDeploymentOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'edgePresetDeploymentOutput_status' - The status of the deployable resource.
--
-- 'artifact', 'edgePresetDeploymentOutput_artifact' - The Amazon Resource Name (ARN) of the generated deployable resource.
--
-- 'statusMessage', 'edgePresetDeploymentOutput_statusMessage' - Returns a message describing the status of the deployed resource.
--
-- 'type'', 'edgePresetDeploymentOutput_type' - The deployment type created by SageMaker Edge Manager. Currently only
-- supports Amazon Web Services IoT Greengrass Version 2 components.
newEdgePresetDeploymentOutput ::
  -- | 'type''
  EdgePresetDeploymentType ->
  EdgePresetDeploymentOutput
newEdgePresetDeploymentOutput pType_ =
  EdgePresetDeploymentOutput'
    { status =
        Prelude.Nothing,
      artifact = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      type' = pType_
    }

-- | The status of the deployable resource.
edgePresetDeploymentOutput_status :: Lens.Lens' EdgePresetDeploymentOutput (Prelude.Maybe EdgePresetDeploymentStatus)
edgePresetDeploymentOutput_status = Lens.lens (\EdgePresetDeploymentOutput' {status} -> status) (\s@EdgePresetDeploymentOutput' {} a -> s {status = a} :: EdgePresetDeploymentOutput)

-- | The Amazon Resource Name (ARN) of the generated deployable resource.
edgePresetDeploymentOutput_artifact :: Lens.Lens' EdgePresetDeploymentOutput (Prelude.Maybe Prelude.Text)
edgePresetDeploymentOutput_artifact = Lens.lens (\EdgePresetDeploymentOutput' {artifact} -> artifact) (\s@EdgePresetDeploymentOutput' {} a -> s {artifact = a} :: EdgePresetDeploymentOutput)

-- | Returns a message describing the status of the deployed resource.
edgePresetDeploymentOutput_statusMessage :: Lens.Lens' EdgePresetDeploymentOutput (Prelude.Maybe Prelude.Text)
edgePresetDeploymentOutput_statusMessage = Lens.lens (\EdgePresetDeploymentOutput' {statusMessage} -> statusMessage) (\s@EdgePresetDeploymentOutput' {} a -> s {statusMessage = a} :: EdgePresetDeploymentOutput)

-- | The deployment type created by SageMaker Edge Manager. Currently only
-- supports Amazon Web Services IoT Greengrass Version 2 components.
edgePresetDeploymentOutput_type :: Lens.Lens' EdgePresetDeploymentOutput EdgePresetDeploymentType
edgePresetDeploymentOutput_type = Lens.lens (\EdgePresetDeploymentOutput' {type'} -> type') (\s@EdgePresetDeploymentOutput' {} a -> s {type' = a} :: EdgePresetDeploymentOutput)

instance Core.FromJSON EdgePresetDeploymentOutput where
  parseJSON =
    Core.withObject
      "EdgePresetDeploymentOutput"
      ( \x ->
          EdgePresetDeploymentOutput'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Artifact")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..: "Type")
      )

instance Prelude.Hashable EdgePresetDeploymentOutput where
  hashWithSalt _salt EdgePresetDeploymentOutput' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` artifact
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` type'

instance Prelude.NFData EdgePresetDeploymentOutput where
  rnf EdgePresetDeploymentOutput' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf artifact
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf type'
