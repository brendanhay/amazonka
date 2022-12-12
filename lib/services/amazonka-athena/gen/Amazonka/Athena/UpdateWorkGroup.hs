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
-- Module      : Amazonka.Athena.UpdateWorkGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the workgroup with the specified name. The workgroup\'s name
-- cannot be changed. Only one of @ConfigurationsUpdates@ or
-- @ConfigurationUpdates@ can be specified; @ConfigurationsUpdates@ for a
-- workgroup with multi engine support (for example, an Apache Spark
-- enabled workgroup) or @ConfigurationUpdates@ for an Athena SQL
-- workgroup.
module Amazonka.Athena.UpdateWorkGroup
  ( -- * Creating a Request
    UpdateWorkGroup (..),
    newUpdateWorkGroup,

    -- * Request Lenses
    updateWorkGroup_configurationUpdates,
    updateWorkGroup_description,
    updateWorkGroup_state,
    updateWorkGroup_workGroup,

    -- * Destructuring the Response
    UpdateWorkGroupResponse (..),
    newUpdateWorkGroupResponse,

    -- * Response Lenses
    updateWorkGroupResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWorkGroup' smart constructor.
data UpdateWorkGroup = UpdateWorkGroup'
  { -- | Contains configuration updates for an Athena SQL workgroup.
    configurationUpdates :: Prelude.Maybe WorkGroupConfigurationUpdates,
    -- | The workgroup description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The workgroup state that will be updated for the given workgroup.
    state :: Prelude.Maybe WorkGroupState,
    -- | The specified workgroup that will be updated.
    workGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationUpdates', 'updateWorkGroup_configurationUpdates' - Contains configuration updates for an Athena SQL workgroup.
--
-- 'description', 'updateWorkGroup_description' - The workgroup description.
--
-- 'state', 'updateWorkGroup_state' - The workgroup state that will be updated for the given workgroup.
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
      description = Prelude.Nothing,
      state = Prelude.Nothing,
      workGroup = pWorkGroup_
    }

-- | Contains configuration updates for an Athena SQL workgroup.
updateWorkGroup_configurationUpdates :: Lens.Lens' UpdateWorkGroup (Prelude.Maybe WorkGroupConfigurationUpdates)
updateWorkGroup_configurationUpdates = Lens.lens (\UpdateWorkGroup' {configurationUpdates} -> configurationUpdates) (\s@UpdateWorkGroup' {} a -> s {configurationUpdates = a} :: UpdateWorkGroup)

-- | The workgroup description.
updateWorkGroup_description :: Lens.Lens' UpdateWorkGroup (Prelude.Maybe Prelude.Text)
updateWorkGroup_description = Lens.lens (\UpdateWorkGroup' {description} -> description) (\s@UpdateWorkGroup' {} a -> s {description = a} :: UpdateWorkGroup)

-- | The workgroup state that will be updated for the given workgroup.
updateWorkGroup_state :: Lens.Lens' UpdateWorkGroup (Prelude.Maybe WorkGroupState)
updateWorkGroup_state = Lens.lens (\UpdateWorkGroup' {state} -> state) (\s@UpdateWorkGroup' {} a -> s {state = a} :: UpdateWorkGroup)

-- | The specified workgroup that will be updated.
updateWorkGroup_workGroup :: Lens.Lens' UpdateWorkGroup Prelude.Text
updateWorkGroup_workGroup = Lens.lens (\UpdateWorkGroup' {workGroup} -> workGroup) (\s@UpdateWorkGroup' {} a -> s {workGroup = a} :: UpdateWorkGroup)

instance Core.AWSRequest UpdateWorkGroup where
  type
    AWSResponse UpdateWorkGroup =
      UpdateWorkGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateWorkGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWorkGroup where
  hashWithSalt _salt UpdateWorkGroup' {..} =
    _salt `Prelude.hashWithSalt` configurationUpdates
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` workGroup

instance Prelude.NFData UpdateWorkGroup where
  rnf UpdateWorkGroup' {..} =
    Prelude.rnf configurationUpdates
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf workGroup

instance Data.ToHeaders UpdateWorkGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.UpdateWorkGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkGroup where
  toJSON UpdateWorkGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigurationUpdates" Data..=)
              Prelude.<$> configurationUpdates,
            ("Description" Data..=) Prelude.<$> description,
            ("State" Data..=) Prelude.<$> state,
            Prelude.Just ("WorkGroup" Data..= workGroup)
          ]
      )

instance Data.ToPath UpdateWorkGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateWorkGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkGroupResponse' smart constructor.
data UpdateWorkGroupResponse = UpdateWorkGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData UpdateWorkGroupResponse where
  rnf UpdateWorkGroupResponse' {..} =
    Prelude.rnf httpStatus
