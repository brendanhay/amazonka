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
-- Module      : Amazonka.IoTThingsGraph.Types.SystemInstanceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.SystemInstanceSummary where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types.DeploymentTarget
import Amazonka.IoTThingsGraph.Types.SystemInstanceDeploymentStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains summary information about a system instance.
--
-- /See:/ 'newSystemInstanceSummary' smart constructor.
data SystemInstanceSummary = SystemInstanceSummary'
  { -- | The status of the system instance.
    status :: Prelude.Maybe SystemInstanceDeploymentStatus,
    -- | The ID of the Greengrass group where the system instance is deployed.
    greengrassGroupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the system instance.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date when the system instance was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the Greengrass group where the system instance is deployed.
    greengrassGroupId :: Prelude.Maybe Prelude.Text,
    -- | The version of the Greengrass group where the system instance is
    -- deployed.
    greengrassGroupVersionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the system instance.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the system instance was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The target of the system instance.
    target :: Prelude.Maybe DeploymentTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SystemInstanceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'systemInstanceSummary_status' - The status of the system instance.
--
-- 'greengrassGroupName', 'systemInstanceSummary_greengrassGroupName' - The ID of the Greengrass group where the system instance is deployed.
--
-- 'arn', 'systemInstanceSummary_arn' - The ARN of the system instance.
--
-- 'createdAt', 'systemInstanceSummary_createdAt' - The date when the system instance was created.
--
-- 'greengrassGroupId', 'systemInstanceSummary_greengrassGroupId' - The ID of the Greengrass group where the system instance is deployed.
--
-- 'greengrassGroupVersionId', 'systemInstanceSummary_greengrassGroupVersionId' - The version of the Greengrass group where the system instance is
-- deployed.
--
-- 'id', 'systemInstanceSummary_id' - The ID of the system instance.
--
-- 'updatedAt', 'systemInstanceSummary_updatedAt' - The date and time when the system instance was last updated.
--
-- 'target', 'systemInstanceSummary_target' - The target of the system instance.
newSystemInstanceSummary ::
  SystemInstanceSummary
newSystemInstanceSummary =
  SystemInstanceSummary'
    { status = Prelude.Nothing,
      greengrassGroupName = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      greengrassGroupId = Prelude.Nothing,
      greengrassGroupVersionId = Prelude.Nothing,
      id = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | The status of the system instance.
systemInstanceSummary_status :: Lens.Lens' SystemInstanceSummary (Prelude.Maybe SystemInstanceDeploymentStatus)
systemInstanceSummary_status = Lens.lens (\SystemInstanceSummary' {status} -> status) (\s@SystemInstanceSummary' {} a -> s {status = a} :: SystemInstanceSummary)

-- | The ID of the Greengrass group where the system instance is deployed.
systemInstanceSummary_greengrassGroupName :: Lens.Lens' SystemInstanceSummary (Prelude.Maybe Prelude.Text)
systemInstanceSummary_greengrassGroupName = Lens.lens (\SystemInstanceSummary' {greengrassGroupName} -> greengrassGroupName) (\s@SystemInstanceSummary' {} a -> s {greengrassGroupName = a} :: SystemInstanceSummary)

-- | The ARN of the system instance.
systemInstanceSummary_arn :: Lens.Lens' SystemInstanceSummary (Prelude.Maybe Prelude.Text)
systemInstanceSummary_arn = Lens.lens (\SystemInstanceSummary' {arn} -> arn) (\s@SystemInstanceSummary' {} a -> s {arn = a} :: SystemInstanceSummary)

-- | The date when the system instance was created.
systemInstanceSummary_createdAt :: Lens.Lens' SystemInstanceSummary (Prelude.Maybe Prelude.UTCTime)
systemInstanceSummary_createdAt = Lens.lens (\SystemInstanceSummary' {createdAt} -> createdAt) (\s@SystemInstanceSummary' {} a -> s {createdAt = a} :: SystemInstanceSummary) Prelude.. Lens.mapping Core._Time

-- | The ID of the Greengrass group where the system instance is deployed.
systemInstanceSummary_greengrassGroupId :: Lens.Lens' SystemInstanceSummary (Prelude.Maybe Prelude.Text)
systemInstanceSummary_greengrassGroupId = Lens.lens (\SystemInstanceSummary' {greengrassGroupId} -> greengrassGroupId) (\s@SystemInstanceSummary' {} a -> s {greengrassGroupId = a} :: SystemInstanceSummary)

-- | The version of the Greengrass group where the system instance is
-- deployed.
systemInstanceSummary_greengrassGroupVersionId :: Lens.Lens' SystemInstanceSummary (Prelude.Maybe Prelude.Text)
systemInstanceSummary_greengrassGroupVersionId = Lens.lens (\SystemInstanceSummary' {greengrassGroupVersionId} -> greengrassGroupVersionId) (\s@SystemInstanceSummary' {} a -> s {greengrassGroupVersionId = a} :: SystemInstanceSummary)

-- | The ID of the system instance.
systemInstanceSummary_id :: Lens.Lens' SystemInstanceSummary (Prelude.Maybe Prelude.Text)
systemInstanceSummary_id = Lens.lens (\SystemInstanceSummary' {id} -> id) (\s@SystemInstanceSummary' {} a -> s {id = a} :: SystemInstanceSummary)

-- | The date and time when the system instance was last updated.
systemInstanceSummary_updatedAt :: Lens.Lens' SystemInstanceSummary (Prelude.Maybe Prelude.UTCTime)
systemInstanceSummary_updatedAt = Lens.lens (\SystemInstanceSummary' {updatedAt} -> updatedAt) (\s@SystemInstanceSummary' {} a -> s {updatedAt = a} :: SystemInstanceSummary) Prelude.. Lens.mapping Core._Time

-- | The target of the system instance.
systemInstanceSummary_target :: Lens.Lens' SystemInstanceSummary (Prelude.Maybe DeploymentTarget)
systemInstanceSummary_target = Lens.lens (\SystemInstanceSummary' {target} -> target) (\s@SystemInstanceSummary' {} a -> s {target = a} :: SystemInstanceSummary)

instance Core.FromJSON SystemInstanceSummary where
  parseJSON =
    Core.withObject
      "SystemInstanceSummary"
      ( \x ->
          SystemInstanceSummary'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "greengrassGroupName")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "greengrassGroupId")
            Prelude.<*> (x Core..:? "greengrassGroupVersionId")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "updatedAt")
            Prelude.<*> (x Core..:? "target")
      )

instance Prelude.Hashable SystemInstanceSummary where
  hashWithSalt _salt SystemInstanceSummary' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` greengrassGroupName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` greengrassGroupId
      `Prelude.hashWithSalt` greengrassGroupVersionId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` target

instance Prelude.NFData SystemInstanceSummary where
  rnf SystemInstanceSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf greengrassGroupName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf greengrassGroupId
      `Prelude.seq` Prelude.rnf greengrassGroupVersionId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf target
