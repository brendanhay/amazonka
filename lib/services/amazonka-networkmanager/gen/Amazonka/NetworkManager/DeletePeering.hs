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
-- Module      : Amazonka.NetworkManager.DeletePeering
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing peering connection.
module Amazonka.NetworkManager.DeletePeering
  ( -- * Creating a Request
    DeletePeering (..),
    newDeletePeering,

    -- * Request Lenses
    deletePeering_peeringId,

    -- * Destructuring the Response
    DeletePeeringResponse (..),
    newDeletePeeringResponse,

    -- * Response Lenses
    deletePeeringResponse_peering,
    deletePeeringResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePeering' smart constructor.
data DeletePeering = DeletePeering'
  { -- | The ID of the peering connection to delete.
    peeringId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePeering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'peeringId', 'deletePeering_peeringId' - The ID of the peering connection to delete.
newDeletePeering ::
  -- | 'peeringId'
  Prelude.Text ->
  DeletePeering
newDeletePeering pPeeringId_ =
  DeletePeering' {peeringId = pPeeringId_}

-- | The ID of the peering connection to delete.
deletePeering_peeringId :: Lens.Lens' DeletePeering Prelude.Text
deletePeering_peeringId = Lens.lens (\DeletePeering' {peeringId} -> peeringId) (\s@DeletePeering' {} a -> s {peeringId = a} :: DeletePeering)

instance Core.AWSRequest DeletePeering where
  type
    AWSResponse DeletePeering =
      DeletePeeringResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePeeringResponse'
            Prelude.<$> (x Data..?> "Peering")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePeering where
  hashWithSalt _salt DeletePeering' {..} =
    _salt `Prelude.hashWithSalt` peeringId

instance Prelude.NFData DeletePeering where
  rnf DeletePeering' {..} = Prelude.rnf peeringId

instance Data.ToHeaders DeletePeering where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeletePeering where
  toPath DeletePeering' {..} =
    Prelude.mconcat ["/peerings/", Data.toBS peeringId]

instance Data.ToQuery DeletePeering where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePeeringResponse' smart constructor.
data DeletePeeringResponse = DeletePeeringResponse'
  { -- | Information about a deleted peering connection.
    peering :: Prelude.Maybe Peering,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePeeringResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'peering', 'deletePeeringResponse_peering' - Information about a deleted peering connection.
--
-- 'httpStatus', 'deletePeeringResponse_httpStatus' - The response's http status code.
newDeletePeeringResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePeeringResponse
newDeletePeeringResponse pHttpStatus_ =
  DeletePeeringResponse'
    { peering = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a deleted peering connection.
deletePeeringResponse_peering :: Lens.Lens' DeletePeeringResponse (Prelude.Maybe Peering)
deletePeeringResponse_peering = Lens.lens (\DeletePeeringResponse' {peering} -> peering) (\s@DeletePeeringResponse' {} a -> s {peering = a} :: DeletePeeringResponse)

-- | The response's http status code.
deletePeeringResponse_httpStatus :: Lens.Lens' DeletePeeringResponse Prelude.Int
deletePeeringResponse_httpStatus = Lens.lens (\DeletePeeringResponse' {httpStatus} -> httpStatus) (\s@DeletePeeringResponse' {} a -> s {httpStatus = a} :: DeletePeeringResponse)

instance Prelude.NFData DeletePeeringResponse where
  rnf DeletePeeringResponse' {..} =
    Prelude.rnf peering
      `Prelude.seq` Prelude.rnf httpStatus
