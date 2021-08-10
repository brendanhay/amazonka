{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateMaintenanceWindowTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the target of an existing maintenance window. You can change
-- the following:
--
-- -   Name
--
-- -   Description
--
-- -   Owner
--
-- -   IDs for an ID target
--
-- -   Tags for a Tag target
--
-- -   From any supported tag type to another. The three supported tag
--     types are ID target, Tag target, and resource group. For more
--     information, see Target.
--
-- If a parameter is null, then the corresponding field is not modified.
module Network.AWS.SSM.UpdateMaintenanceWindowTarget
  ( -- * Creating a Request
    UpdateMaintenanceWindowTarget (..),
    newUpdateMaintenanceWindowTarget,

    -- * Request Lenses
    updateMaintenanceWindowTarget_targets,
    updateMaintenanceWindowTarget_name,
    updateMaintenanceWindowTarget_replace,
    updateMaintenanceWindowTarget_description,
    updateMaintenanceWindowTarget_ownerInformation,
    updateMaintenanceWindowTarget_windowId,
    updateMaintenanceWindowTarget_windowTargetId,

    -- * Destructuring the Response
    UpdateMaintenanceWindowTargetResponse (..),
    newUpdateMaintenanceWindowTargetResponse,

    -- * Response Lenses
    updateMaintenanceWindowTargetResponse_windowTargetId,
    updateMaintenanceWindowTargetResponse_targets,
    updateMaintenanceWindowTargetResponse_name,
    updateMaintenanceWindowTargetResponse_windowId,
    updateMaintenanceWindowTargetResponse_description,
    updateMaintenanceWindowTargetResponse_ownerInformation,
    updateMaintenanceWindowTargetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdateMaintenanceWindowTarget' smart constructor.
data UpdateMaintenanceWindowTarget = UpdateMaintenanceWindowTarget'
  { -- | The targets to add or replace.
    targets :: Prelude.Maybe [Target],
    -- | A name for the update.
    name :: Prelude.Maybe Prelude.Text,
    -- | If True, then all fields that are required by the
    -- RegisterTargetWithMaintenanceWindow action are also required for this
    -- API request. Optional fields that are not specified are set to null.
    replace :: Prelude.Maybe Prelude.Bool,
    -- | An optional description for the update.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | User-provided value that will be included in any CloudWatch events
    -- raised while running tasks for these targets in this maintenance window.
    ownerInformation :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The maintenance window ID with which to modify the target.
    windowId :: Prelude.Text,
    -- | The target ID to modify.
    windowTargetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMaintenanceWindowTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targets', 'updateMaintenanceWindowTarget_targets' - The targets to add or replace.
--
-- 'name', 'updateMaintenanceWindowTarget_name' - A name for the update.
--
-- 'replace', 'updateMaintenanceWindowTarget_replace' - If True, then all fields that are required by the
-- RegisterTargetWithMaintenanceWindow action are also required for this
-- API request. Optional fields that are not specified are set to null.
--
-- 'description', 'updateMaintenanceWindowTarget_description' - An optional description for the update.
--
-- 'ownerInformation', 'updateMaintenanceWindowTarget_ownerInformation' - User-provided value that will be included in any CloudWatch events
-- raised while running tasks for these targets in this maintenance window.
--
-- 'windowId', 'updateMaintenanceWindowTarget_windowId' - The maintenance window ID with which to modify the target.
--
-- 'windowTargetId', 'updateMaintenanceWindowTarget_windowTargetId' - The target ID to modify.
newUpdateMaintenanceWindowTarget ::
  -- | 'windowId'
  Prelude.Text ->
  -- | 'windowTargetId'
  Prelude.Text ->
  UpdateMaintenanceWindowTarget
newUpdateMaintenanceWindowTarget
  pWindowId_
  pWindowTargetId_ =
    UpdateMaintenanceWindowTarget'
      { targets =
          Prelude.Nothing,
        name = Prelude.Nothing,
        replace = Prelude.Nothing,
        description = Prelude.Nothing,
        ownerInformation = Prelude.Nothing,
        windowId = pWindowId_,
        windowTargetId = pWindowTargetId_
      }

-- | The targets to add or replace.
updateMaintenanceWindowTarget_targets :: Lens.Lens' UpdateMaintenanceWindowTarget (Prelude.Maybe [Target])
updateMaintenanceWindowTarget_targets = Lens.lens (\UpdateMaintenanceWindowTarget' {targets} -> targets) (\s@UpdateMaintenanceWindowTarget' {} a -> s {targets = a} :: UpdateMaintenanceWindowTarget) Prelude.. Lens.mapping Lens._Coerce

-- | A name for the update.
updateMaintenanceWindowTarget_name :: Lens.Lens' UpdateMaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTarget_name = Lens.lens (\UpdateMaintenanceWindowTarget' {name} -> name) (\s@UpdateMaintenanceWindowTarget' {} a -> s {name = a} :: UpdateMaintenanceWindowTarget)

-- | If True, then all fields that are required by the
-- RegisterTargetWithMaintenanceWindow action are also required for this
-- API request. Optional fields that are not specified are set to null.
updateMaintenanceWindowTarget_replace :: Lens.Lens' UpdateMaintenanceWindowTarget (Prelude.Maybe Prelude.Bool)
updateMaintenanceWindowTarget_replace = Lens.lens (\UpdateMaintenanceWindowTarget' {replace} -> replace) (\s@UpdateMaintenanceWindowTarget' {} a -> s {replace = a} :: UpdateMaintenanceWindowTarget)

-- | An optional description for the update.
updateMaintenanceWindowTarget_description :: Lens.Lens' UpdateMaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTarget_description = Lens.lens (\UpdateMaintenanceWindowTarget' {description} -> description) (\s@UpdateMaintenanceWindowTarget' {} a -> s {description = a} :: UpdateMaintenanceWindowTarget) Prelude.. Lens.mapping Core._Sensitive

-- | User-provided value that will be included in any CloudWatch events
-- raised while running tasks for these targets in this maintenance window.
updateMaintenanceWindowTarget_ownerInformation :: Lens.Lens' UpdateMaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTarget_ownerInformation = Lens.lens (\UpdateMaintenanceWindowTarget' {ownerInformation} -> ownerInformation) (\s@UpdateMaintenanceWindowTarget' {} a -> s {ownerInformation = a} :: UpdateMaintenanceWindowTarget) Prelude.. Lens.mapping Core._Sensitive

-- | The maintenance window ID with which to modify the target.
updateMaintenanceWindowTarget_windowId :: Lens.Lens' UpdateMaintenanceWindowTarget Prelude.Text
updateMaintenanceWindowTarget_windowId = Lens.lens (\UpdateMaintenanceWindowTarget' {windowId} -> windowId) (\s@UpdateMaintenanceWindowTarget' {} a -> s {windowId = a} :: UpdateMaintenanceWindowTarget)

-- | The target ID to modify.
updateMaintenanceWindowTarget_windowTargetId :: Lens.Lens' UpdateMaintenanceWindowTarget Prelude.Text
updateMaintenanceWindowTarget_windowTargetId = Lens.lens (\UpdateMaintenanceWindowTarget' {windowTargetId} -> windowTargetId) (\s@UpdateMaintenanceWindowTarget' {} a -> s {windowTargetId = a} :: UpdateMaintenanceWindowTarget)

instance
  Core.AWSRequest
    UpdateMaintenanceWindowTarget
  where
  type
    AWSResponse UpdateMaintenanceWindowTarget =
      UpdateMaintenanceWindowTargetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMaintenanceWindowTargetResponse'
            Prelude.<$> (x Core..?> "WindowTargetId")
            Prelude.<*> (x Core..?> "Targets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "WindowId")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "OwnerInformation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateMaintenanceWindowTarget

instance Prelude.NFData UpdateMaintenanceWindowTarget

instance Core.ToHeaders UpdateMaintenanceWindowTarget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.UpdateMaintenanceWindowTarget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateMaintenanceWindowTarget where
  toJSON UpdateMaintenanceWindowTarget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Targets" Core..=) Prelude.<$> targets,
            ("Name" Core..=) Prelude.<$> name,
            ("Replace" Core..=) Prelude.<$> replace,
            ("Description" Core..=) Prelude.<$> description,
            ("OwnerInformation" Core..=)
              Prelude.<$> ownerInformation,
            Prelude.Just ("WindowId" Core..= windowId),
            Prelude.Just
              ("WindowTargetId" Core..= windowTargetId)
          ]
      )

instance Core.ToPath UpdateMaintenanceWindowTarget where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateMaintenanceWindowTarget where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMaintenanceWindowTargetResponse' smart constructor.
data UpdateMaintenanceWindowTargetResponse = UpdateMaintenanceWindowTargetResponse'
  { -- | The target ID specified in the update request.
    windowTargetId :: Prelude.Maybe Prelude.Text,
    -- | The updated targets.
    targets :: Prelude.Maybe [Target],
    -- | The updated name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The maintenance window ID specified in the update request.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The updated description.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The updated owner.
    ownerInformation :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMaintenanceWindowTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowTargetId', 'updateMaintenanceWindowTargetResponse_windowTargetId' - The target ID specified in the update request.
--
-- 'targets', 'updateMaintenanceWindowTargetResponse_targets' - The updated targets.
--
-- 'name', 'updateMaintenanceWindowTargetResponse_name' - The updated name.
--
-- 'windowId', 'updateMaintenanceWindowTargetResponse_windowId' - The maintenance window ID specified in the update request.
--
-- 'description', 'updateMaintenanceWindowTargetResponse_description' - The updated description.
--
-- 'ownerInformation', 'updateMaintenanceWindowTargetResponse_ownerInformation' - The updated owner.
--
-- 'httpStatus', 'updateMaintenanceWindowTargetResponse_httpStatus' - The response's http status code.
newUpdateMaintenanceWindowTargetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMaintenanceWindowTargetResponse
newUpdateMaintenanceWindowTargetResponse pHttpStatus_ =
  UpdateMaintenanceWindowTargetResponse'
    { windowTargetId =
        Prelude.Nothing,
      targets = Prelude.Nothing,
      name = Prelude.Nothing,
      windowId = Prelude.Nothing,
      description = Prelude.Nothing,
      ownerInformation = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The target ID specified in the update request.
updateMaintenanceWindowTargetResponse_windowTargetId :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTargetResponse_windowTargetId = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {windowTargetId} -> windowTargetId) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {windowTargetId = a} :: UpdateMaintenanceWindowTargetResponse)

-- | The updated targets.
updateMaintenanceWindowTargetResponse_targets :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Prelude.Maybe [Target])
updateMaintenanceWindowTargetResponse_targets = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {targets} -> targets) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {targets = a} :: UpdateMaintenanceWindowTargetResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The updated name.
updateMaintenanceWindowTargetResponse_name :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTargetResponse_name = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {name} -> name) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {name = a} :: UpdateMaintenanceWindowTargetResponse)

-- | The maintenance window ID specified in the update request.
updateMaintenanceWindowTargetResponse_windowId :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTargetResponse_windowId = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {windowId} -> windowId) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {windowId = a} :: UpdateMaintenanceWindowTargetResponse)

-- | The updated description.
updateMaintenanceWindowTargetResponse_description :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTargetResponse_description = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {description} -> description) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {description = a} :: UpdateMaintenanceWindowTargetResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The updated owner.
updateMaintenanceWindowTargetResponse_ownerInformation :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTargetResponse_ownerInformation = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {ownerInformation} -> ownerInformation) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {ownerInformation = a} :: UpdateMaintenanceWindowTargetResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
updateMaintenanceWindowTargetResponse_httpStatus :: Lens.Lens' UpdateMaintenanceWindowTargetResponse Prelude.Int
updateMaintenanceWindowTargetResponse_httpStatus = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {httpStatus} -> httpStatus) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {httpStatus = a} :: UpdateMaintenanceWindowTargetResponse)

instance
  Prelude.NFData
    UpdateMaintenanceWindowTargetResponse
