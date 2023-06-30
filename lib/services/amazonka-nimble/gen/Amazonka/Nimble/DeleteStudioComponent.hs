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
-- Module      : Amazonka.Nimble.DeleteStudioComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a studio component resource.
module Amazonka.Nimble.DeleteStudioComponent
  ( -- * Creating a Request
    DeleteStudioComponent (..),
    newDeleteStudioComponent,

    -- * Request Lenses
    deleteStudioComponent_clientToken,
    deleteStudioComponent_studioComponentId,
    deleteStudioComponent_studioId,

    -- * Destructuring the Response
    DeleteStudioComponentResponse (..),
    newDeleteStudioComponentResponse,

    -- * Response Lenses
    deleteStudioComponentResponse_studioComponent,
    deleteStudioComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStudioComponent' smart constructor.
data DeleteStudioComponent = DeleteStudioComponent'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the
    -- Amazon Web Services SDK automatically generates a client token and uses
    -- it for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The studio component ID.
    studioComponentId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteStudioComponent_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
--
-- 'studioComponentId', 'deleteStudioComponent_studioComponentId' - The studio component ID.
--
-- 'studioId', 'deleteStudioComponent_studioId' - The studio ID.
newDeleteStudioComponent ::
  -- | 'studioComponentId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  DeleteStudioComponent
newDeleteStudioComponent
  pStudioComponentId_
  pStudioId_ =
    DeleteStudioComponent'
      { clientToken =
          Prelude.Nothing,
        studioComponentId = pStudioComponentId_,
        studioId = pStudioId_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
deleteStudioComponent_clientToken :: Lens.Lens' DeleteStudioComponent (Prelude.Maybe Prelude.Text)
deleteStudioComponent_clientToken = Lens.lens (\DeleteStudioComponent' {clientToken} -> clientToken) (\s@DeleteStudioComponent' {} a -> s {clientToken = a} :: DeleteStudioComponent)

-- | The studio component ID.
deleteStudioComponent_studioComponentId :: Lens.Lens' DeleteStudioComponent Prelude.Text
deleteStudioComponent_studioComponentId = Lens.lens (\DeleteStudioComponent' {studioComponentId} -> studioComponentId) (\s@DeleteStudioComponent' {} a -> s {studioComponentId = a} :: DeleteStudioComponent)

-- | The studio ID.
deleteStudioComponent_studioId :: Lens.Lens' DeleteStudioComponent Prelude.Text
deleteStudioComponent_studioId = Lens.lens (\DeleteStudioComponent' {studioId} -> studioId) (\s@DeleteStudioComponent' {} a -> s {studioId = a} :: DeleteStudioComponent)

instance Core.AWSRequest DeleteStudioComponent where
  type
    AWSResponse DeleteStudioComponent =
      DeleteStudioComponentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteStudioComponentResponse'
            Prelude.<$> (x Data..?> "studioComponent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStudioComponent where
  hashWithSalt _salt DeleteStudioComponent' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` studioComponentId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData DeleteStudioComponent where
  rnf DeleteStudioComponent' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf studioComponentId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders DeleteStudioComponent where
  toHeaders DeleteStudioComponent' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeleteStudioComponent where
  toPath DeleteStudioComponent' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/studio-components/",
        Data.toBS studioComponentId
      ]

instance Data.ToQuery DeleteStudioComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStudioComponentResponse' smart constructor.
data DeleteStudioComponentResponse = DeleteStudioComponentResponse'
  { -- | Information about the studio component.
    studioComponent :: Prelude.Maybe StudioComponent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioComponent', 'deleteStudioComponentResponse_studioComponent' - Information about the studio component.
--
-- 'httpStatus', 'deleteStudioComponentResponse_httpStatus' - The response's http status code.
newDeleteStudioComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStudioComponentResponse
newDeleteStudioComponentResponse pHttpStatus_ =
  DeleteStudioComponentResponse'
    { studioComponent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the studio component.
deleteStudioComponentResponse_studioComponent :: Lens.Lens' DeleteStudioComponentResponse (Prelude.Maybe StudioComponent)
deleteStudioComponentResponse_studioComponent = Lens.lens (\DeleteStudioComponentResponse' {studioComponent} -> studioComponent) (\s@DeleteStudioComponentResponse' {} a -> s {studioComponent = a} :: DeleteStudioComponentResponse)

-- | The response's http status code.
deleteStudioComponentResponse_httpStatus :: Lens.Lens' DeleteStudioComponentResponse Prelude.Int
deleteStudioComponentResponse_httpStatus = Lens.lens (\DeleteStudioComponentResponse' {httpStatus} -> httpStatus) (\s@DeleteStudioComponentResponse' {} a -> s {httpStatus = a} :: DeleteStudioComponentResponse)

instance Prelude.NFData DeleteStudioComponentResponse where
  rnf DeleteStudioComponentResponse' {..} =
    Prelude.rnf studioComponent
      `Prelude.seq` Prelude.rnf httpStatus
