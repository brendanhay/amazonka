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
-- Module      : Amazonka.DrS.DeleteSourceServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a single Source Server by ID. The Source Server must be
-- disconnected first.
module Amazonka.DrS.DeleteSourceServer
  ( -- * Creating a Request
    DeleteSourceServer (..),
    newDeleteSourceServer,

    -- * Request Lenses
    deleteSourceServer_sourceServerID,

    -- * Destructuring the Response
    DeleteSourceServerResponse (..),
    newDeleteSourceServerResponse,

    -- * Response Lenses
    deleteSourceServerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSourceServer' smart constructor.
data DeleteSourceServer = DeleteSourceServer'
  { -- | The ID of the Source Server to be deleted.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSourceServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceServerID', 'deleteSourceServer_sourceServerID' - The ID of the Source Server to be deleted.
newDeleteSourceServer ::
  -- | 'sourceServerID'
  Prelude.Text ->
  DeleteSourceServer
newDeleteSourceServer pSourceServerID_ =
  DeleteSourceServer'
    { sourceServerID =
        pSourceServerID_
    }

-- | The ID of the Source Server to be deleted.
deleteSourceServer_sourceServerID :: Lens.Lens' DeleteSourceServer Prelude.Text
deleteSourceServer_sourceServerID = Lens.lens (\DeleteSourceServer' {sourceServerID} -> sourceServerID) (\s@DeleteSourceServer' {} a -> s {sourceServerID = a} :: DeleteSourceServer)

instance Core.AWSRequest DeleteSourceServer where
  type
    AWSResponse DeleteSourceServer =
      DeleteSourceServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSourceServerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSourceServer where
  hashWithSalt _salt DeleteSourceServer' {..} =
    _salt `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData DeleteSourceServer where
  rnf DeleteSourceServer' {..} =
    Prelude.rnf sourceServerID

instance Data.ToHeaders DeleteSourceServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSourceServer where
  toJSON DeleteSourceServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath DeleteSourceServer where
  toPath = Prelude.const "/DeleteSourceServer"

instance Data.ToQuery DeleteSourceServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSourceServerResponse' smart constructor.
data DeleteSourceServerResponse = DeleteSourceServerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSourceServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSourceServerResponse_httpStatus' - The response's http status code.
newDeleteSourceServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSourceServerResponse
newDeleteSourceServerResponse pHttpStatus_ =
  DeleteSourceServerResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSourceServerResponse_httpStatus :: Lens.Lens' DeleteSourceServerResponse Prelude.Int
deleteSourceServerResponse_httpStatus = Lens.lens (\DeleteSourceServerResponse' {httpStatus} -> httpStatus) (\s@DeleteSourceServerResponse' {} a -> s {httpStatus = a} :: DeleteSourceServerResponse)

instance Prelude.NFData DeleteSourceServerResponse where
  rnf DeleteSourceServerResponse' {..} =
    Prelude.rnf httpStatus
