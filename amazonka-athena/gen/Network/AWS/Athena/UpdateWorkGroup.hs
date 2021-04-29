{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateWorkGroup' smart constructor.
data UpdateWorkGroup = UpdateWorkGroup'
  { -- | The workgroup configuration that will be updated for the given
    -- workgroup.
    configurationUpdates :: Prelude.Maybe WorkGroupConfigurationUpdates,
    -- | The workgroup state that will be updated for the given workgroup.
    state :: Prelude.Maybe WorkGroupState,
    -- | The workgroup description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The specified workgroup that will be updated.
    workGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateWorkGroup
newUpdateWorkGroup pWorkGroup_ =
  UpdateWorkGroup'
    { configurationUpdates =
        Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      workGroup = pWorkGroup_
    }

-- | The workgroup configuration that will be updated for the given
-- workgroup.
updateWorkGroup_configurationUpdates :: Lens.Lens' UpdateWorkGroup (Prelude.Maybe WorkGroupConfigurationUpdates)
updateWorkGroup_configurationUpdates = Lens.lens (\UpdateWorkGroup' {configurationUpdates} -> configurationUpdates) (\s@UpdateWorkGroup' {} a -> s {configurationUpdates = a} :: UpdateWorkGroup)

-- | The workgroup state that will be updated for the given workgroup.
updateWorkGroup_state :: Lens.Lens' UpdateWorkGroup (Prelude.Maybe WorkGroupState)
updateWorkGroup_state = Lens.lens (\UpdateWorkGroup' {state} -> state) (\s@UpdateWorkGroup' {} a -> s {state = a} :: UpdateWorkGroup)

-- | The workgroup description.
updateWorkGroup_description :: Lens.Lens' UpdateWorkGroup (Prelude.Maybe Prelude.Text)
updateWorkGroup_description = Lens.lens (\UpdateWorkGroup' {description} -> description) (\s@UpdateWorkGroup' {} a -> s {description = a} :: UpdateWorkGroup)

-- | The specified workgroup that will be updated.
updateWorkGroup_workGroup :: Lens.Lens' UpdateWorkGroup Prelude.Text
updateWorkGroup_workGroup = Lens.lens (\UpdateWorkGroup' {workGroup} -> workGroup) (\s@UpdateWorkGroup' {} a -> s {workGroup = a} :: UpdateWorkGroup)

instance Prelude.AWSRequest UpdateWorkGroup where
  type Rs UpdateWorkGroup = UpdateWorkGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateWorkGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWorkGroup

instance Prelude.NFData UpdateWorkGroup

instance Prelude.ToHeaders UpdateWorkGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonAthena.UpdateWorkGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateWorkGroup where
  toJSON UpdateWorkGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ConfigurationUpdates" Prelude..=)
              Prelude.<$> configurationUpdates,
            ("State" Prelude..=) Prelude.<$> state,
            ("Description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("WorkGroup" Prelude..= workGroup)
          ]
      )

instance Prelude.ToPath UpdateWorkGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateWorkGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkGroupResponse' smart constructor.
data UpdateWorkGroupResponse = UpdateWorkGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateWorkGroupResponse
newUpdateWorkGroupResponse pHttpStatus_ =
  UpdateWorkGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateWorkGroupResponse_httpStatus :: Lens.Lens' UpdateWorkGroupResponse Prelude.Int
updateWorkGroupResponse_httpStatus = Lens.lens (\UpdateWorkGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkGroupResponse' {} a -> s {httpStatus = a} :: UpdateWorkGroupResponse)

instance Prelude.NFData UpdateWorkGroupResponse
