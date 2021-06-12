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
-- Module      : Network.AWS.Athena.UpdateWorkGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the workgroup with the specified name. The workgroup\'s name
-- cannot be changed.
module Network.AWS.Athena.UpdateWorkGroup
  ( -- * Creating a Request
    UpdateWorkGroup (..),
    newUpdateWorkGroup,

    -- * Request Lenses
    updateWorkGroup_configurationUpdates,
    updateWorkGroup_state,
    updateWorkGroup_description,
    updateWorkGroup_workGroup,

    -- * Destructuring the Response
    UpdateWorkGroupResponse (..),
    newUpdateWorkGroupResponse,

    -- * Response Lenses
    updateWorkGroupResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateWorkGroup' smart constructor.
data UpdateWorkGroup = UpdateWorkGroup'
  { -- | The workgroup configuration that will be updated for the given
    -- workgroup.
    configurationUpdates :: Core.Maybe WorkGroupConfigurationUpdates,
    -- | The workgroup state that will be updated for the given workgroup.
    state :: Core.Maybe WorkGroupState,
    -- | The workgroup description.
    description :: Core.Maybe Core.Text,
    -- | The specified workgroup that will be updated.
    workGroup :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateWorkGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationUpdates', 'updateWorkGroup_configurationUpdates' - The workgroup configuration that will be updated for the given
-- workgroup.
--
-- 'state', 'updateWorkGroup_state' - The workgroup state that will be updated for the given workgroup.
--
-- 'description', 'updateWorkGroup_description' - The workgroup description.
--
-- 'workGroup', 'updateWorkGroup_workGroup' - The specified workgroup that will be updated.
newUpdateWorkGroup ::
  -- | 'workGroup'
  Core.Text ->
  UpdateWorkGroup
newUpdateWorkGroup pWorkGroup_ =
  UpdateWorkGroup'
    { configurationUpdates =
        Core.Nothing,
      state = Core.Nothing,
      description = Core.Nothing,
      workGroup = pWorkGroup_
    }

-- | The workgroup configuration that will be updated for the given
-- workgroup.
updateWorkGroup_configurationUpdates :: Lens.Lens' UpdateWorkGroup (Core.Maybe WorkGroupConfigurationUpdates)
updateWorkGroup_configurationUpdates = Lens.lens (\UpdateWorkGroup' {configurationUpdates} -> configurationUpdates) (\s@UpdateWorkGroup' {} a -> s {configurationUpdates = a} :: UpdateWorkGroup)

-- | The workgroup state that will be updated for the given workgroup.
updateWorkGroup_state :: Lens.Lens' UpdateWorkGroup (Core.Maybe WorkGroupState)
updateWorkGroup_state = Lens.lens (\UpdateWorkGroup' {state} -> state) (\s@UpdateWorkGroup' {} a -> s {state = a} :: UpdateWorkGroup)

-- | The workgroup description.
updateWorkGroup_description :: Lens.Lens' UpdateWorkGroup (Core.Maybe Core.Text)
updateWorkGroup_description = Lens.lens (\UpdateWorkGroup' {description} -> description) (\s@UpdateWorkGroup' {} a -> s {description = a} :: UpdateWorkGroup)

-- | The specified workgroup that will be updated.
updateWorkGroup_workGroup :: Lens.Lens' UpdateWorkGroup Core.Text
updateWorkGroup_workGroup = Lens.lens (\UpdateWorkGroup' {workGroup} -> workGroup) (\s@UpdateWorkGroup' {} a -> s {workGroup = a} :: UpdateWorkGroup)

instance Core.AWSRequest UpdateWorkGroup where
  type
    AWSResponse UpdateWorkGroup =
      UpdateWorkGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateWorkGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateWorkGroup

instance Core.NFData UpdateWorkGroup

instance Core.ToHeaders UpdateWorkGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonAthena.UpdateWorkGroup" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateWorkGroup where
  toJSON UpdateWorkGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConfigurationUpdates" Core..=)
              Core.<$> configurationUpdates,
            ("State" Core..=) Core.<$> state,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("WorkGroup" Core..= workGroup)
          ]
      )

instance Core.ToPath UpdateWorkGroup where
  toPath = Core.const "/"

instance Core.ToQuery UpdateWorkGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateWorkGroupResponse' smart constructor.
data UpdateWorkGroupResponse = UpdateWorkGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateWorkGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWorkGroupResponse_httpStatus' - The response's http status code.
newUpdateWorkGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateWorkGroupResponse
newUpdateWorkGroupResponse pHttpStatus_ =
  UpdateWorkGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateWorkGroupResponse_httpStatus :: Lens.Lens' UpdateWorkGroupResponse Core.Int
updateWorkGroupResponse_httpStatus = Lens.lens (\UpdateWorkGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkGroupResponse' {} a -> s {httpStatus = a} :: UpdateWorkGroupResponse)

instance Core.NFData UpdateWorkGroupResponse
