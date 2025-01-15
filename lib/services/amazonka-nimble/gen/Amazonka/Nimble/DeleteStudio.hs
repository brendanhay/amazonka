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
-- Module      : Amazonka.Nimble.DeleteStudio
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a studio resource.
module Amazonka.Nimble.DeleteStudio
  ( -- * Creating a Request
    DeleteStudio (..),
    newDeleteStudio,

    -- * Request Lenses
    deleteStudio_clientToken,
    deleteStudio_studioId,

    -- * Destructuring the Response
    DeleteStudioResponse (..),
    newDeleteStudioResponse,

    -- * Response Lenses
    deleteStudioResponse_httpStatus,
    deleteStudioResponse_studio,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStudio' smart constructor.
data DeleteStudio = DeleteStudio'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the
    -- Amazon Web Services SDK automatically generates a client token and uses
    -- it for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteStudio_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
--
-- 'studioId', 'deleteStudio_studioId' - The studio ID.
newDeleteStudio ::
  -- | 'studioId'
  Prelude.Text ->
  DeleteStudio
newDeleteStudio pStudioId_ =
  DeleteStudio'
    { clientToken = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
deleteStudio_clientToken :: Lens.Lens' DeleteStudio (Prelude.Maybe Prelude.Text)
deleteStudio_clientToken = Lens.lens (\DeleteStudio' {clientToken} -> clientToken) (\s@DeleteStudio' {} a -> s {clientToken = a} :: DeleteStudio)

-- | The studio ID.
deleteStudio_studioId :: Lens.Lens' DeleteStudio Prelude.Text
deleteStudio_studioId = Lens.lens (\DeleteStudio' {studioId} -> studioId) (\s@DeleteStudio' {} a -> s {studioId = a} :: DeleteStudio)

instance Core.AWSRequest DeleteStudio where
  type AWSResponse DeleteStudio = DeleteStudioResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteStudioResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "studio")
      )

instance Prelude.Hashable DeleteStudio where
  hashWithSalt _salt DeleteStudio' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData DeleteStudio where
  rnf DeleteStudio' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf studioId

instance Data.ToHeaders DeleteStudio where
  toHeaders DeleteStudio' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeleteStudio where
  toPath DeleteStudio' {..} =
    Prelude.mconcat
      ["/2020-08-01/studios/", Data.toBS studioId]

instance Data.ToQuery DeleteStudio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStudioResponse' smart constructor.
data DeleteStudioResponse = DeleteStudioResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about a studio.
    studio :: Studio
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStudioResponse_httpStatus' - The response's http status code.
--
-- 'studio', 'deleteStudioResponse_studio' - Information about a studio.
newDeleteStudioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'studio'
  Studio ->
  DeleteStudioResponse
newDeleteStudioResponse pHttpStatus_ pStudio_ =
  DeleteStudioResponse'
    { httpStatus = pHttpStatus_,
      studio = pStudio_
    }

-- | The response's http status code.
deleteStudioResponse_httpStatus :: Lens.Lens' DeleteStudioResponse Prelude.Int
deleteStudioResponse_httpStatus = Lens.lens (\DeleteStudioResponse' {httpStatus} -> httpStatus) (\s@DeleteStudioResponse' {} a -> s {httpStatus = a} :: DeleteStudioResponse)

-- | Information about a studio.
deleteStudioResponse_studio :: Lens.Lens' DeleteStudioResponse Studio
deleteStudioResponse_studio = Lens.lens (\DeleteStudioResponse' {studio} -> studio) (\s@DeleteStudioResponse' {} a -> s {studio = a} :: DeleteStudioResponse)

instance Prelude.NFData DeleteStudioResponse where
  rnf DeleteStudioResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf studio
