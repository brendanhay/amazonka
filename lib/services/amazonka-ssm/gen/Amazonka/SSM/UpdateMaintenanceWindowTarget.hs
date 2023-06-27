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
-- Module      : Amazonka.SSM.UpdateMaintenanceWindowTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- If a parameter is null, then the corresponding field isn\'t modified.
module Amazonka.SSM.UpdateMaintenanceWindowTarget
  ( -- * Creating a Request
    UpdateMaintenanceWindowTarget (..),
    newUpdateMaintenanceWindowTarget,

    -- * Request Lenses
    updateMaintenanceWindowTarget_description,
    updateMaintenanceWindowTarget_name,
    updateMaintenanceWindowTarget_ownerInformation,
    updateMaintenanceWindowTarget_replace,
    updateMaintenanceWindowTarget_targets,
    updateMaintenanceWindowTarget_windowId,
    updateMaintenanceWindowTarget_windowTargetId,

    -- * Destructuring the Response
    UpdateMaintenanceWindowTargetResponse (..),
    newUpdateMaintenanceWindowTargetResponse,

    -- * Response Lenses
    updateMaintenanceWindowTargetResponse_description,
    updateMaintenanceWindowTargetResponse_name,
    updateMaintenanceWindowTargetResponse_ownerInformation,
    updateMaintenanceWindowTargetResponse_targets,
    updateMaintenanceWindowTargetResponse_windowId,
    updateMaintenanceWindowTargetResponse_windowTargetId,
    updateMaintenanceWindowTargetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateMaintenanceWindowTarget' smart constructor.
data UpdateMaintenanceWindowTarget = UpdateMaintenanceWindowTarget'
  { -- | An optional description for the update.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A name for the update.
    name :: Prelude.Maybe Prelude.Text,
    -- | User-provided value that will be included in any Amazon CloudWatch
    -- Events events raised while running tasks for these targets in this
    -- maintenance window.
    ownerInformation :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | If @True@, then all fields that are required by the
    -- RegisterTargetWithMaintenanceWindow operation are also required for this
    -- API request. Optional fields that aren\'t specified are set to null.
    replace :: Prelude.Maybe Prelude.Bool,
    -- | The targets to add or replace.
    targets :: Prelude.Maybe [Target],
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
-- 'description', 'updateMaintenanceWindowTarget_description' - An optional description for the update.
--
-- 'name', 'updateMaintenanceWindowTarget_name' - A name for the update.
--
-- 'ownerInformation', 'updateMaintenanceWindowTarget_ownerInformation' - User-provided value that will be included in any Amazon CloudWatch
-- Events events raised while running tasks for these targets in this
-- maintenance window.
--
-- 'replace', 'updateMaintenanceWindowTarget_replace' - If @True@, then all fields that are required by the
-- RegisterTargetWithMaintenanceWindow operation are also required for this
-- API request. Optional fields that aren\'t specified are set to null.
--
-- 'targets', 'updateMaintenanceWindowTarget_targets' - The targets to add or replace.
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
      { description =
          Prelude.Nothing,
        name = Prelude.Nothing,
        ownerInformation = Prelude.Nothing,
        replace = Prelude.Nothing,
        targets = Prelude.Nothing,
        windowId = pWindowId_,
        windowTargetId = pWindowTargetId_
      }

-- | An optional description for the update.
updateMaintenanceWindowTarget_description :: Lens.Lens' UpdateMaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTarget_description = Lens.lens (\UpdateMaintenanceWindowTarget' {description} -> description) (\s@UpdateMaintenanceWindowTarget' {} a -> s {description = a} :: UpdateMaintenanceWindowTarget) Prelude.. Lens.mapping Data._Sensitive

-- | A name for the update.
updateMaintenanceWindowTarget_name :: Lens.Lens' UpdateMaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTarget_name = Lens.lens (\UpdateMaintenanceWindowTarget' {name} -> name) (\s@UpdateMaintenanceWindowTarget' {} a -> s {name = a} :: UpdateMaintenanceWindowTarget)

-- | User-provided value that will be included in any Amazon CloudWatch
-- Events events raised while running tasks for these targets in this
-- maintenance window.
updateMaintenanceWindowTarget_ownerInformation :: Lens.Lens' UpdateMaintenanceWindowTarget (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTarget_ownerInformation = Lens.lens (\UpdateMaintenanceWindowTarget' {ownerInformation} -> ownerInformation) (\s@UpdateMaintenanceWindowTarget' {} a -> s {ownerInformation = a} :: UpdateMaintenanceWindowTarget) Prelude.. Lens.mapping Data._Sensitive

-- | If @True@, then all fields that are required by the
-- RegisterTargetWithMaintenanceWindow operation are also required for this
-- API request. Optional fields that aren\'t specified are set to null.
updateMaintenanceWindowTarget_replace :: Lens.Lens' UpdateMaintenanceWindowTarget (Prelude.Maybe Prelude.Bool)
updateMaintenanceWindowTarget_replace = Lens.lens (\UpdateMaintenanceWindowTarget' {replace} -> replace) (\s@UpdateMaintenanceWindowTarget' {} a -> s {replace = a} :: UpdateMaintenanceWindowTarget)

-- | The targets to add or replace.
updateMaintenanceWindowTarget_targets :: Lens.Lens' UpdateMaintenanceWindowTarget (Prelude.Maybe [Target])
updateMaintenanceWindowTarget_targets = Lens.lens (\UpdateMaintenanceWindowTarget' {targets} -> targets) (\s@UpdateMaintenanceWindowTarget' {} a -> s {targets = a} :: UpdateMaintenanceWindowTarget) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMaintenanceWindowTargetResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "OwnerInformation")
            Prelude.<*> (x Data..?> "Targets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "WindowId")
            Prelude.<*> (x Data..?> "WindowTargetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateMaintenanceWindowTarget
  where
  hashWithSalt _salt UpdateMaintenanceWindowTarget' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ownerInformation
      `Prelude.hashWithSalt` replace
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` windowId
      `Prelude.hashWithSalt` windowTargetId

instance Prelude.NFData UpdateMaintenanceWindowTarget where
  rnf UpdateMaintenanceWindowTarget' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerInformation
      `Prelude.seq` Prelude.rnf replace
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf windowTargetId

instance Data.ToHeaders UpdateMaintenanceWindowTarget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.UpdateMaintenanceWindowTarget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMaintenanceWindowTarget where
  toJSON UpdateMaintenanceWindowTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("OwnerInformation" Data..=)
              Prelude.<$> ownerInformation,
            ("Replace" Data..=) Prelude.<$> replace,
            ("Targets" Data..=) Prelude.<$> targets,
            Prelude.Just ("WindowId" Data..= windowId),
            Prelude.Just
              ("WindowTargetId" Data..= windowTargetId)
          ]
      )

instance Data.ToPath UpdateMaintenanceWindowTarget where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateMaintenanceWindowTarget where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMaintenanceWindowTargetResponse' smart constructor.
data UpdateMaintenanceWindowTargetResponse = UpdateMaintenanceWindowTargetResponse'
  { -- | The updated description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The updated name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated owner.
    ownerInformation :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The updated targets.
    targets :: Prelude.Maybe [Target],
    -- | The maintenance window ID specified in the update request.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The target ID specified in the update request.
    windowTargetId :: Prelude.Maybe Prelude.Text,
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
-- 'description', 'updateMaintenanceWindowTargetResponse_description' - The updated description.
--
-- 'name', 'updateMaintenanceWindowTargetResponse_name' - The updated name.
--
-- 'ownerInformation', 'updateMaintenanceWindowTargetResponse_ownerInformation' - The updated owner.
--
-- 'targets', 'updateMaintenanceWindowTargetResponse_targets' - The updated targets.
--
-- 'windowId', 'updateMaintenanceWindowTargetResponse_windowId' - The maintenance window ID specified in the update request.
--
-- 'windowTargetId', 'updateMaintenanceWindowTargetResponse_windowTargetId' - The target ID specified in the update request.
--
-- 'httpStatus', 'updateMaintenanceWindowTargetResponse_httpStatus' - The response's http status code.
newUpdateMaintenanceWindowTargetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMaintenanceWindowTargetResponse
newUpdateMaintenanceWindowTargetResponse pHttpStatus_ =
  UpdateMaintenanceWindowTargetResponse'
    { description =
        Prelude.Nothing,
      name = Prelude.Nothing,
      ownerInformation = Prelude.Nothing,
      targets = Prelude.Nothing,
      windowId = Prelude.Nothing,
      windowTargetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated description.
updateMaintenanceWindowTargetResponse_description :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTargetResponse_description = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {description} -> description) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {description = a} :: UpdateMaintenanceWindowTargetResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The updated name.
updateMaintenanceWindowTargetResponse_name :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTargetResponse_name = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {name} -> name) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {name = a} :: UpdateMaintenanceWindowTargetResponse)

-- | The updated owner.
updateMaintenanceWindowTargetResponse_ownerInformation :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTargetResponse_ownerInformation = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {ownerInformation} -> ownerInformation) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {ownerInformation = a} :: UpdateMaintenanceWindowTargetResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The updated targets.
updateMaintenanceWindowTargetResponse_targets :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Prelude.Maybe [Target])
updateMaintenanceWindowTargetResponse_targets = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {targets} -> targets) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {targets = a} :: UpdateMaintenanceWindowTargetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The maintenance window ID specified in the update request.
updateMaintenanceWindowTargetResponse_windowId :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTargetResponse_windowId = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {windowId} -> windowId) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {windowId = a} :: UpdateMaintenanceWindowTargetResponse)

-- | The target ID specified in the update request.
updateMaintenanceWindowTargetResponse_windowTargetId :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Prelude.Maybe Prelude.Text)
updateMaintenanceWindowTargetResponse_windowTargetId = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {windowTargetId} -> windowTargetId) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {windowTargetId = a} :: UpdateMaintenanceWindowTargetResponse)

-- | The response's http status code.
updateMaintenanceWindowTargetResponse_httpStatus :: Lens.Lens' UpdateMaintenanceWindowTargetResponse Prelude.Int
updateMaintenanceWindowTargetResponse_httpStatus = Lens.lens (\UpdateMaintenanceWindowTargetResponse' {httpStatus} -> httpStatus) (\s@UpdateMaintenanceWindowTargetResponse' {} a -> s {httpStatus = a} :: UpdateMaintenanceWindowTargetResponse)

instance
  Prelude.NFData
    UpdateMaintenanceWindowTargetResponse
  where
  rnf UpdateMaintenanceWindowTargetResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerInformation
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf windowTargetId
      `Prelude.seq` Prelude.rnf httpStatus
