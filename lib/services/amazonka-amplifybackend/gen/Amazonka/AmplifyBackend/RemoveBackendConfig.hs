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
-- Module      : Amazonka.AmplifyBackend.RemoveBackendConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the AWS resources required to access the Amplify Admin UI.
module Amazonka.AmplifyBackend.RemoveBackendConfig
  ( -- * Creating a Request
    RemoveBackendConfig (..),
    newRemoveBackendConfig,

    -- * Request Lenses
    removeBackendConfig_appId,

    -- * Destructuring the Response
    RemoveBackendConfigResponse (..),
    newRemoveBackendConfigResponse,

    -- * Response Lenses
    removeBackendConfigResponse_error,
    removeBackendConfigResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveBackendConfig' smart constructor.
data RemoveBackendConfig = RemoveBackendConfig'
  { -- | The app ID.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveBackendConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'removeBackendConfig_appId' - The app ID.
newRemoveBackendConfig ::
  -- | 'appId'
  Prelude.Text ->
  RemoveBackendConfig
newRemoveBackendConfig pAppId_ =
  RemoveBackendConfig' {appId = pAppId_}

-- | The app ID.
removeBackendConfig_appId :: Lens.Lens' RemoveBackendConfig Prelude.Text
removeBackendConfig_appId = Lens.lens (\RemoveBackendConfig' {appId} -> appId) (\s@RemoveBackendConfig' {} a -> s {appId = a} :: RemoveBackendConfig)

instance Core.AWSRequest RemoveBackendConfig where
  type
    AWSResponse RemoveBackendConfig =
      RemoveBackendConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveBackendConfigResponse'
            Prelude.<$> (x Data..?> "error")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveBackendConfig where
  hashWithSalt _salt RemoveBackendConfig' {..} =
    _salt `Prelude.hashWithSalt` appId

instance Prelude.NFData RemoveBackendConfig where
  rnf RemoveBackendConfig' {..} = Prelude.rnf appId

instance Data.ToHeaders RemoveBackendConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveBackendConfig where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath RemoveBackendConfig where
  toPath RemoveBackendConfig' {..} =
    Prelude.mconcat
      ["/backend/", Data.toBS appId, "/config/remove"]

instance Data.ToQuery RemoveBackendConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveBackendConfigResponse' smart constructor.
data RemoveBackendConfigResponse = RemoveBackendConfigResponse'
  { -- | If the request fails, this error is returned.
    error :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveBackendConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'removeBackendConfigResponse_error' - If the request fails, this error is returned.
--
-- 'httpStatus', 'removeBackendConfigResponse_httpStatus' - The response's http status code.
newRemoveBackendConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveBackendConfigResponse
newRemoveBackendConfigResponse pHttpStatus_ =
  RemoveBackendConfigResponse'
    { error =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the request fails, this error is returned.
removeBackendConfigResponse_error :: Lens.Lens' RemoveBackendConfigResponse (Prelude.Maybe Prelude.Text)
removeBackendConfigResponse_error = Lens.lens (\RemoveBackendConfigResponse' {error} -> error) (\s@RemoveBackendConfigResponse' {} a -> s {error = a} :: RemoveBackendConfigResponse)

-- | The response's http status code.
removeBackendConfigResponse_httpStatus :: Lens.Lens' RemoveBackendConfigResponse Prelude.Int
removeBackendConfigResponse_httpStatus = Lens.lens (\RemoveBackendConfigResponse' {httpStatus} -> httpStatus) (\s@RemoveBackendConfigResponse' {} a -> s {httpStatus = a} :: RemoveBackendConfigResponse)

instance Prelude.NFData RemoveBackendConfigResponse where
  rnf RemoveBackendConfigResponse' {..} =
    Prelude.rnf error `Prelude.seq`
      Prelude.rnf httpStatus
