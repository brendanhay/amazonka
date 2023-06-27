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
-- Module      : Amazonka.DrS.DeleteSourceNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete Source Network resource.
module Amazonka.DrS.DeleteSourceNetwork
  ( -- * Creating a Request
    DeleteSourceNetwork (..),
    newDeleteSourceNetwork,

    -- * Request Lenses
    deleteSourceNetwork_sourceNetworkID,

    -- * Destructuring the Response
    DeleteSourceNetworkResponse (..),
    newDeleteSourceNetworkResponse,

    -- * Response Lenses
    deleteSourceNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSourceNetwork' smart constructor.
data DeleteSourceNetwork = DeleteSourceNetwork'
  { -- | ID of the Source Network to delete.
    sourceNetworkID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSourceNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceNetworkID', 'deleteSourceNetwork_sourceNetworkID' - ID of the Source Network to delete.
newDeleteSourceNetwork ::
  -- | 'sourceNetworkID'
  Prelude.Text ->
  DeleteSourceNetwork
newDeleteSourceNetwork pSourceNetworkID_ =
  DeleteSourceNetwork'
    { sourceNetworkID =
        pSourceNetworkID_
    }

-- | ID of the Source Network to delete.
deleteSourceNetwork_sourceNetworkID :: Lens.Lens' DeleteSourceNetwork Prelude.Text
deleteSourceNetwork_sourceNetworkID = Lens.lens (\DeleteSourceNetwork' {sourceNetworkID} -> sourceNetworkID) (\s@DeleteSourceNetwork' {} a -> s {sourceNetworkID = a} :: DeleteSourceNetwork)

instance Core.AWSRequest DeleteSourceNetwork where
  type
    AWSResponse DeleteSourceNetwork =
      DeleteSourceNetworkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSourceNetworkResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSourceNetwork where
  hashWithSalt _salt DeleteSourceNetwork' {..} =
    _salt `Prelude.hashWithSalt` sourceNetworkID

instance Prelude.NFData DeleteSourceNetwork where
  rnf DeleteSourceNetwork' {..} =
    Prelude.rnf sourceNetworkID

instance Data.ToHeaders DeleteSourceNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSourceNetwork where
  toJSON DeleteSourceNetwork' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceNetworkID" Data..= sourceNetworkID)
          ]
      )

instance Data.ToPath DeleteSourceNetwork where
  toPath = Prelude.const "/DeleteSourceNetwork"

instance Data.ToQuery DeleteSourceNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSourceNetworkResponse' smart constructor.
data DeleteSourceNetworkResponse = DeleteSourceNetworkResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSourceNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSourceNetworkResponse_httpStatus' - The response's http status code.
newDeleteSourceNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSourceNetworkResponse
newDeleteSourceNetworkResponse pHttpStatus_ =
  DeleteSourceNetworkResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSourceNetworkResponse_httpStatus :: Lens.Lens' DeleteSourceNetworkResponse Prelude.Int
deleteSourceNetworkResponse_httpStatus = Lens.lens (\DeleteSourceNetworkResponse' {httpStatus} -> httpStatus) (\s@DeleteSourceNetworkResponse' {} a -> s {httpStatus = a} :: DeleteSourceNetworkResponse)

instance Prelude.NFData DeleteSourceNetworkResponse where
  rnf DeleteSourceNetworkResponse' {..} =
    Prelude.rnf httpStatus
