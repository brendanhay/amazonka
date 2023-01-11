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
-- Module      : Amazonka.SSMSAP.UpdateApplicationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.UpdateApplicationSettings
  ( -- * Creating a Request
    UpdateApplicationSettings (..),
    newUpdateApplicationSettings,

    -- * Request Lenses
    updateApplicationSettings_credentialsToAddOrUpdate,
    updateApplicationSettings_credentialsToRemove,
    updateApplicationSettings_applicationId,

    -- * Destructuring the Response
    UpdateApplicationSettingsResponse (..),
    newUpdateApplicationSettingsResponse,

    -- * Response Lenses
    updateApplicationSettingsResponse_message,
    updateApplicationSettingsResponse_operationIds,
    updateApplicationSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newUpdateApplicationSettings' smart constructor.
data UpdateApplicationSettings = UpdateApplicationSettings'
  { credentialsToAddOrUpdate :: Prelude.Maybe (Prelude.NonEmpty ApplicationCredential),
    credentialsToRemove :: Prelude.Maybe (Prelude.NonEmpty ApplicationCredential),
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentialsToAddOrUpdate', 'updateApplicationSettings_credentialsToAddOrUpdate' -
--
-- 'credentialsToRemove', 'updateApplicationSettings_credentialsToRemove' -
--
-- 'applicationId', 'updateApplicationSettings_applicationId' -
newUpdateApplicationSettings ::
  -- | 'applicationId'
  Prelude.Text ->
  UpdateApplicationSettings
newUpdateApplicationSettings pApplicationId_ =
  UpdateApplicationSettings'
    { credentialsToAddOrUpdate =
        Prelude.Nothing,
      credentialsToRemove = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- |
updateApplicationSettings_credentialsToAddOrUpdate :: Lens.Lens' UpdateApplicationSettings (Prelude.Maybe (Prelude.NonEmpty ApplicationCredential))
updateApplicationSettings_credentialsToAddOrUpdate = Lens.lens (\UpdateApplicationSettings' {credentialsToAddOrUpdate} -> credentialsToAddOrUpdate) (\s@UpdateApplicationSettings' {} a -> s {credentialsToAddOrUpdate = a} :: UpdateApplicationSettings) Prelude.. Lens.mapping Lens.coerced

-- |
updateApplicationSettings_credentialsToRemove :: Lens.Lens' UpdateApplicationSettings (Prelude.Maybe (Prelude.NonEmpty ApplicationCredential))
updateApplicationSettings_credentialsToRemove = Lens.lens (\UpdateApplicationSettings' {credentialsToRemove} -> credentialsToRemove) (\s@UpdateApplicationSettings' {} a -> s {credentialsToRemove = a} :: UpdateApplicationSettings) Prelude.. Lens.mapping Lens.coerced

-- |
updateApplicationSettings_applicationId :: Lens.Lens' UpdateApplicationSettings Prelude.Text
updateApplicationSettings_applicationId = Lens.lens (\UpdateApplicationSettings' {applicationId} -> applicationId) (\s@UpdateApplicationSettings' {} a -> s {applicationId = a} :: UpdateApplicationSettings)

instance Core.AWSRequest UpdateApplicationSettings where
  type
    AWSResponse UpdateApplicationSettings =
      UpdateApplicationSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApplicationSettingsResponse'
            Prelude.<$> (x Data..?> "Message")
            Prelude.<*> (x Data..?> "OperationIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateApplicationSettings where
  hashWithSalt _salt UpdateApplicationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` credentialsToAddOrUpdate
      `Prelude.hashWithSalt` credentialsToRemove
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData UpdateApplicationSettings where
  rnf UpdateApplicationSettings' {..} =
    Prelude.rnf credentialsToAddOrUpdate
      `Prelude.seq` Prelude.rnf credentialsToRemove
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders UpdateApplicationSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApplicationSettings where
  toJSON UpdateApplicationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CredentialsToAddOrUpdate" Data..=)
              Prelude.<$> credentialsToAddOrUpdate,
            ("CredentialsToRemove" Data..=)
              Prelude.<$> credentialsToRemove,
            Prelude.Just
              ("ApplicationId" Data..= applicationId)
          ]
      )

instance Data.ToPath UpdateApplicationSettings where
  toPath = Prelude.const "/update-application-settings"

instance Data.ToQuery UpdateApplicationSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationSettingsResponse' smart constructor.
data UpdateApplicationSettingsResponse = UpdateApplicationSettingsResponse'
  { message :: Prelude.Maybe Prelude.Text,
    operationIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'updateApplicationSettingsResponse_message' -
--
-- 'operationIds', 'updateApplicationSettingsResponse_operationIds' -
--
-- 'httpStatus', 'updateApplicationSettingsResponse_httpStatus' - The response's http status code.
newUpdateApplicationSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateApplicationSettingsResponse
newUpdateApplicationSettingsResponse pHttpStatus_ =
  UpdateApplicationSettingsResponse'
    { message =
        Prelude.Nothing,
      operationIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
updateApplicationSettingsResponse_message :: Lens.Lens' UpdateApplicationSettingsResponse (Prelude.Maybe Prelude.Text)
updateApplicationSettingsResponse_message = Lens.lens (\UpdateApplicationSettingsResponse' {message} -> message) (\s@UpdateApplicationSettingsResponse' {} a -> s {message = a} :: UpdateApplicationSettingsResponse)

-- |
updateApplicationSettingsResponse_operationIds :: Lens.Lens' UpdateApplicationSettingsResponse (Prelude.Maybe [Prelude.Text])
updateApplicationSettingsResponse_operationIds = Lens.lens (\UpdateApplicationSettingsResponse' {operationIds} -> operationIds) (\s@UpdateApplicationSettingsResponse' {} a -> s {operationIds = a} :: UpdateApplicationSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateApplicationSettingsResponse_httpStatus :: Lens.Lens' UpdateApplicationSettingsResponse Prelude.Int
updateApplicationSettingsResponse_httpStatus = Lens.lens (\UpdateApplicationSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationSettingsResponse' {} a -> s {httpStatus = a} :: UpdateApplicationSettingsResponse)

instance
  Prelude.NFData
    UpdateApplicationSettingsResponse
  where
  rnf UpdateApplicationSettingsResponse' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf operationIds
      `Prelude.seq` Prelude.rnf httpStatus
