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
-- Module      : Amazonka.M2.UpdateEnvironment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration details for a specific environment.
module Amazonka.M2.UpdateEnvironment
  ( -- * Creating a Request
    UpdateEnvironment (..),
    newUpdateEnvironment,

    -- * Request Lenses
    updateEnvironment_instanceType,
    updateEnvironment_preferredMaintenanceWindow,
    updateEnvironment_desiredCapacity,
    updateEnvironment_applyDuringMaintenanceWindow,
    updateEnvironment_engineVersion,
    updateEnvironment_environmentId,

    -- * Destructuring the Response
    UpdateEnvironmentResponse (..),
    newUpdateEnvironmentResponse,

    -- * Response Lenses
    updateEnvironmentResponse_httpStatus,
    updateEnvironmentResponse_environmentId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { -- | The instance type for the environment to update.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Configures a desired maintenance window for the environment. If you do
    -- not provide a value, a random system-generated value will be assigned.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The desired capacity for the environment to update.
    desiredCapacity :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether to update the environment during the maintenance
    -- window. The default is false. Currently, Amazon Web Services Mainframe
    -- Modernization accepts the @engineVersion@ parameter only if
    -- @applyDuringMaintenanceWindow@ is true. If any parameter other than
    -- @engineVersion@ is provided in @UpdateEnvironmentRequest@, it will fail
    -- if @applyDuringMaintenanceWindow@ is set to true.
    applyDuringMaintenanceWindow :: Prelude.Maybe Prelude.Bool,
    -- | The version of the runtime engine for the environment.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the runtime environment that you want to
    -- update.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'updateEnvironment_instanceType' - The instance type for the environment to update.
--
-- 'preferredMaintenanceWindow', 'updateEnvironment_preferredMaintenanceWindow' - Configures a desired maintenance window for the environment. If you do
-- not provide a value, a random system-generated value will be assigned.
--
-- 'desiredCapacity', 'updateEnvironment_desiredCapacity' - The desired capacity for the environment to update.
--
-- 'applyDuringMaintenanceWindow', 'updateEnvironment_applyDuringMaintenanceWindow' - Indicates whether to update the environment during the maintenance
-- window. The default is false. Currently, Amazon Web Services Mainframe
-- Modernization accepts the @engineVersion@ parameter only if
-- @applyDuringMaintenanceWindow@ is true. If any parameter other than
-- @engineVersion@ is provided in @UpdateEnvironmentRequest@, it will fail
-- if @applyDuringMaintenanceWindow@ is set to true.
--
-- 'engineVersion', 'updateEnvironment_engineVersion' - The version of the runtime engine for the environment.
--
-- 'environmentId', 'updateEnvironment_environmentId' - The unique identifier of the runtime environment that you want to
-- update.
newUpdateEnvironment ::
  -- | 'environmentId'
  Prelude.Text ->
  UpdateEnvironment
newUpdateEnvironment pEnvironmentId_ =
  UpdateEnvironment'
    { instanceType = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      desiredCapacity = Prelude.Nothing,
      applyDuringMaintenanceWindow = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      environmentId = pEnvironmentId_
    }

-- | The instance type for the environment to update.
updateEnvironment_instanceType :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_instanceType = Lens.lens (\UpdateEnvironment' {instanceType} -> instanceType) (\s@UpdateEnvironment' {} a -> s {instanceType = a} :: UpdateEnvironment)

-- | Configures a desired maintenance window for the environment. If you do
-- not provide a value, a random system-generated value will be assigned.
updateEnvironment_preferredMaintenanceWindow :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_preferredMaintenanceWindow = Lens.lens (\UpdateEnvironment' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@UpdateEnvironment' {} a -> s {preferredMaintenanceWindow = a} :: UpdateEnvironment)

-- | The desired capacity for the environment to update.
updateEnvironment_desiredCapacity :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Natural)
updateEnvironment_desiredCapacity = Lens.lens (\UpdateEnvironment' {desiredCapacity} -> desiredCapacity) (\s@UpdateEnvironment' {} a -> s {desiredCapacity = a} :: UpdateEnvironment)

-- | Indicates whether to update the environment during the maintenance
-- window. The default is false. Currently, Amazon Web Services Mainframe
-- Modernization accepts the @engineVersion@ parameter only if
-- @applyDuringMaintenanceWindow@ is true. If any parameter other than
-- @engineVersion@ is provided in @UpdateEnvironmentRequest@, it will fail
-- if @applyDuringMaintenanceWindow@ is set to true.
updateEnvironment_applyDuringMaintenanceWindow :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Bool)
updateEnvironment_applyDuringMaintenanceWindow = Lens.lens (\UpdateEnvironment' {applyDuringMaintenanceWindow} -> applyDuringMaintenanceWindow) (\s@UpdateEnvironment' {} a -> s {applyDuringMaintenanceWindow = a} :: UpdateEnvironment)

-- | The version of the runtime engine for the environment.
updateEnvironment_engineVersion :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_engineVersion = Lens.lens (\UpdateEnvironment' {engineVersion} -> engineVersion) (\s@UpdateEnvironment' {} a -> s {engineVersion = a} :: UpdateEnvironment)

-- | The unique identifier of the runtime environment that you want to
-- update.
updateEnvironment_environmentId :: Lens.Lens' UpdateEnvironment Prelude.Text
updateEnvironment_environmentId = Lens.lens (\UpdateEnvironment' {environmentId} -> environmentId) (\s@UpdateEnvironment' {} a -> s {environmentId = a} :: UpdateEnvironment)

instance Core.AWSRequest UpdateEnvironment where
  type
    AWSResponse UpdateEnvironment =
      UpdateEnvironmentResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "environmentId")
      )

instance Prelude.Hashable UpdateEnvironment where
  hashWithSalt _salt UpdateEnvironment' {..} =
    _salt `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` desiredCapacity
      `Prelude.hashWithSalt` applyDuringMaintenanceWindow
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData UpdateEnvironment where
  rnf UpdateEnvironment' {..} =
    Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf desiredCapacity
      `Prelude.seq` Prelude.rnf applyDuringMaintenanceWindow
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf environmentId

instance Core.ToHeaders UpdateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateEnvironment where
  toJSON UpdateEnvironment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("instanceType" Core..=) Prelude.<$> instanceType,
            ("preferredMaintenanceWindow" Core..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("desiredCapacity" Core..=)
              Prelude.<$> desiredCapacity,
            ("applyDuringMaintenanceWindow" Core..=)
              Prelude.<$> applyDuringMaintenanceWindow,
            ("engineVersion" Core..=) Prelude.<$> engineVersion
          ]
      )

instance Core.ToPath UpdateEnvironment where
  toPath UpdateEnvironment' {..} =
    Prelude.mconcat
      ["/environments/", Core.toBS environmentId]

instance Core.ToQuery UpdateEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEnvironmentResponse' smart constructor.
data UpdateEnvironmentResponse = UpdateEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of the runtime environment that was updated.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEnvironmentResponse_httpStatus' - The response's http status code.
--
-- 'environmentId', 'updateEnvironmentResponse_environmentId' - The unique identifier of the runtime environment that was updated.
newUpdateEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environmentId'
  Prelude.Text ->
  UpdateEnvironmentResponse
newUpdateEnvironmentResponse
  pHttpStatus_
  pEnvironmentId_ =
    UpdateEnvironmentResponse'
      { httpStatus =
          pHttpStatus_,
        environmentId = pEnvironmentId_
      }

-- | The response's http status code.
updateEnvironmentResponse_httpStatus :: Lens.Lens' UpdateEnvironmentResponse Prelude.Int
updateEnvironmentResponse_httpStatus = Lens.lens (\UpdateEnvironmentResponse' {httpStatus} -> httpStatus) (\s@UpdateEnvironmentResponse' {} a -> s {httpStatus = a} :: UpdateEnvironmentResponse)

-- | The unique identifier of the runtime environment that was updated.
updateEnvironmentResponse_environmentId :: Lens.Lens' UpdateEnvironmentResponse Prelude.Text
updateEnvironmentResponse_environmentId = Lens.lens (\UpdateEnvironmentResponse' {environmentId} -> environmentId) (\s@UpdateEnvironmentResponse' {} a -> s {environmentId = a} :: UpdateEnvironmentResponse)

instance Prelude.NFData UpdateEnvironmentResponse where
  rnf UpdateEnvironmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentId
