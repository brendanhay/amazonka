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
-- Module      : Amazonka.WorkSpaces.UpdateConnectClientAddIn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Amazon Connect client add-in. Use this action to update the
-- name and endpoint URL of a Amazon Connect client add-in.
module Amazonka.WorkSpaces.UpdateConnectClientAddIn
  ( -- * Creating a Request
    UpdateConnectClientAddIn (..),
    newUpdateConnectClientAddIn,

    -- * Request Lenses
    updateConnectClientAddIn_name,
    updateConnectClientAddIn_url,
    updateConnectClientAddIn_addInId,
    updateConnectClientAddIn_resourceId,

    -- * Destructuring the Response
    UpdateConnectClientAddInResponse (..),
    newUpdateConnectClientAddInResponse,

    -- * Response Lenses
    updateConnectClientAddInResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newUpdateConnectClientAddIn' smart constructor.
data UpdateConnectClientAddIn = UpdateConnectClientAddIn'
  { -- | The name of the client add-in.
    name :: Prelude.Maybe Prelude.Text,
    -- | The endpoint URL of the Amazon Connect client add-in.
    url :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the client add-in to update.
    addInId :: Prelude.Text,
    -- | The directory identifier for which the client add-in is configured.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectClientAddIn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateConnectClientAddIn_name' - The name of the client add-in.
--
-- 'url', 'updateConnectClientAddIn_url' - The endpoint URL of the Amazon Connect client add-in.
--
-- 'addInId', 'updateConnectClientAddIn_addInId' - The identifier of the client add-in to update.
--
-- 'resourceId', 'updateConnectClientAddIn_resourceId' - The directory identifier for which the client add-in is configured.
newUpdateConnectClientAddIn ::
  -- | 'addInId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  UpdateConnectClientAddIn
newUpdateConnectClientAddIn pAddInId_ pResourceId_ =
  UpdateConnectClientAddIn'
    { name = Prelude.Nothing,
      url = Prelude.Nothing,
      addInId = pAddInId_,
      resourceId = pResourceId_
    }

-- | The name of the client add-in.
updateConnectClientAddIn_name :: Lens.Lens' UpdateConnectClientAddIn (Prelude.Maybe Prelude.Text)
updateConnectClientAddIn_name = Lens.lens (\UpdateConnectClientAddIn' {name} -> name) (\s@UpdateConnectClientAddIn' {} a -> s {name = a} :: UpdateConnectClientAddIn)

-- | The endpoint URL of the Amazon Connect client add-in.
updateConnectClientAddIn_url :: Lens.Lens' UpdateConnectClientAddIn (Prelude.Maybe Prelude.Text)
updateConnectClientAddIn_url = Lens.lens (\UpdateConnectClientAddIn' {url} -> url) (\s@UpdateConnectClientAddIn' {} a -> s {url = a} :: UpdateConnectClientAddIn)

-- | The identifier of the client add-in to update.
updateConnectClientAddIn_addInId :: Lens.Lens' UpdateConnectClientAddIn Prelude.Text
updateConnectClientAddIn_addInId = Lens.lens (\UpdateConnectClientAddIn' {addInId} -> addInId) (\s@UpdateConnectClientAddIn' {} a -> s {addInId = a} :: UpdateConnectClientAddIn)

-- | The directory identifier for which the client add-in is configured.
updateConnectClientAddIn_resourceId :: Lens.Lens' UpdateConnectClientAddIn Prelude.Text
updateConnectClientAddIn_resourceId = Lens.lens (\UpdateConnectClientAddIn' {resourceId} -> resourceId) (\s@UpdateConnectClientAddIn' {} a -> s {resourceId = a} :: UpdateConnectClientAddIn)

instance Core.AWSRequest UpdateConnectClientAddIn where
  type
    AWSResponse UpdateConnectClientAddIn =
      UpdateConnectClientAddInResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConnectClientAddInResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConnectClientAddIn where
  hashWithSalt _salt UpdateConnectClientAddIn' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` addInId
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData UpdateConnectClientAddIn where
  rnf UpdateConnectClientAddIn' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf addInId
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders UpdateConnectClientAddIn where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.UpdateConnectClientAddIn" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConnectClientAddIn where
  toJSON UpdateConnectClientAddIn' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("URL" Data..=) Prelude.<$> url,
            Prelude.Just ("AddInId" Data..= addInId),
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath UpdateConnectClientAddIn where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateConnectClientAddIn where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConnectClientAddInResponse' smart constructor.
data UpdateConnectClientAddInResponse = UpdateConnectClientAddInResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectClientAddInResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConnectClientAddInResponse_httpStatus' - The response's http status code.
newUpdateConnectClientAddInResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConnectClientAddInResponse
newUpdateConnectClientAddInResponse pHttpStatus_ =
  UpdateConnectClientAddInResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateConnectClientAddInResponse_httpStatus :: Lens.Lens' UpdateConnectClientAddInResponse Prelude.Int
updateConnectClientAddInResponse_httpStatus = Lens.lens (\UpdateConnectClientAddInResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectClientAddInResponse' {} a -> s {httpStatus = a} :: UpdateConnectClientAddInResponse)

instance
  Prelude.NFData
    UpdateConnectClientAddInResponse
  where
  rnf UpdateConnectClientAddInResponse' {..} =
    Prelude.rnf httpStatus
