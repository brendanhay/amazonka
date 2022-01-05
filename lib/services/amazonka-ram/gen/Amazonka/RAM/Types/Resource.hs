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
-- Module      : Amazonka.RAM.Types.Resource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.ResourceStatus

-- | Describes a resource associated with a resource share.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The time when the resource was associated with the resource share.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the resource.
    status :: Prelude.Maybe ResourceStatus,
    -- | The Amazon Resource Name (ARN) of the resource share.
    resourceShareArn :: Prelude.Maybe Prelude.Text,
    -- | The time when the association was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource group. This value is
    -- returned only if the resource is a resource group.
    resourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | A message about the status of the resource.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'resource_creationTime' - The time when the resource was associated with the resource share.
--
-- 'status', 'resource_status' - The status of the resource.
--
-- 'resourceShareArn', 'resource_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share.
--
-- 'lastUpdatedTime', 'resource_lastUpdatedTime' - The time when the association was last updated.
--
-- 'arn', 'resource_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'resourceGroupArn', 'resource_resourceGroupArn' - The Amazon Resource Name (ARN) of the resource group. This value is
-- returned only if the resource is a resource group.
--
-- 'statusMessage', 'resource_statusMessage' - A message about the status of the resource.
--
-- 'type'', 'resource_type' - The resource type.
newResource ::
  Resource
newResource =
  Resource'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      resourceShareArn = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      resourceGroupArn = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The time when the resource was associated with the resource share.
resource_creationTime :: Lens.Lens' Resource (Prelude.Maybe Prelude.UTCTime)
resource_creationTime = Lens.lens (\Resource' {creationTime} -> creationTime) (\s@Resource' {} a -> s {creationTime = a} :: Resource) Prelude.. Lens.mapping Core._Time

-- | The status of the resource.
resource_status :: Lens.Lens' Resource (Prelude.Maybe ResourceStatus)
resource_status = Lens.lens (\Resource' {status} -> status) (\s@Resource' {} a -> s {status = a} :: Resource)

-- | The Amazon Resource Name (ARN) of the resource share.
resource_resourceShareArn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceShareArn = Lens.lens (\Resource' {resourceShareArn} -> resourceShareArn) (\s@Resource' {} a -> s {resourceShareArn = a} :: Resource)

-- | The time when the association was last updated.
resource_lastUpdatedTime :: Lens.Lens' Resource (Prelude.Maybe Prelude.UTCTime)
resource_lastUpdatedTime = Lens.lens (\Resource' {lastUpdatedTime} -> lastUpdatedTime) (\s@Resource' {} a -> s {lastUpdatedTime = a} :: Resource) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the resource.
resource_arn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_arn = Lens.lens (\Resource' {arn} -> arn) (\s@Resource' {} a -> s {arn = a} :: Resource)

-- | The Amazon Resource Name (ARN) of the resource group. This value is
-- returned only if the resource is a resource group.
resource_resourceGroupArn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceGroupArn = Lens.lens (\Resource' {resourceGroupArn} -> resourceGroupArn) (\s@Resource' {} a -> s {resourceGroupArn = a} :: Resource)

-- | A message about the status of the resource.
resource_statusMessage :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_statusMessage = Lens.lens (\Resource' {statusMessage} -> statusMessage) (\s@Resource' {} a -> s {statusMessage = a} :: Resource)

-- | The resource type.
resource_type :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_type = Lens.lens (\Resource' {type'} -> type') (\s@Resource' {} a -> s {type' = a} :: Resource)

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "resourceShareArn")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "resourceGroupArn")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "type")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` resourceShareArn
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` resourceGroupArn
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf resourceShareArn
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf resourceGroupArn
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf type'
