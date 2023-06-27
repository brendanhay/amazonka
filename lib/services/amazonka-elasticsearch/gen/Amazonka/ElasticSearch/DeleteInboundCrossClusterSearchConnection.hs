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
-- Module      : Amazonka.ElasticSearch.DeleteInboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the destination domain owner to delete an existing inbound
-- cross-cluster search connection.
module Amazonka.ElasticSearch.DeleteInboundCrossClusterSearchConnection
  ( -- * Creating a Request
    DeleteInboundCrossClusterSearchConnection (..),
    newDeleteInboundCrossClusterSearchConnection,

    -- * Request Lenses
    deleteInboundCrossClusterSearchConnection_crossClusterSearchConnectionId,

    -- * Destructuring the Response
    DeleteInboundCrossClusterSearchConnectionResponse (..),
    newDeleteInboundCrossClusterSearchConnectionResponse,

    -- * Response Lenses
    deleteInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    deleteInboundCrossClusterSearchConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the
-- @DeleteInboundCrossClusterSearchConnection@ operation.
--
-- /See:/ 'newDeleteInboundCrossClusterSearchConnection' smart constructor.
data DeleteInboundCrossClusterSearchConnection = DeleteInboundCrossClusterSearchConnection'
  { -- | The id of the inbound connection that you want to permanently delete.
    crossClusterSearchConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInboundCrossClusterSearchConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnectionId', 'deleteInboundCrossClusterSearchConnection_crossClusterSearchConnectionId' - The id of the inbound connection that you want to permanently delete.
newDeleteInboundCrossClusterSearchConnection ::
  -- | 'crossClusterSearchConnectionId'
  Prelude.Text ->
  DeleteInboundCrossClusterSearchConnection
newDeleteInboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    DeleteInboundCrossClusterSearchConnection'
      { crossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the inbound connection that you want to permanently delete.
deleteInboundCrossClusterSearchConnection_crossClusterSearchConnectionId :: Lens.Lens' DeleteInboundCrossClusterSearchConnection Prelude.Text
deleteInboundCrossClusterSearchConnection_crossClusterSearchConnectionId = Lens.lens (\DeleteInboundCrossClusterSearchConnection' {crossClusterSearchConnectionId} -> crossClusterSearchConnectionId) (\s@DeleteInboundCrossClusterSearchConnection' {} a -> s {crossClusterSearchConnectionId = a} :: DeleteInboundCrossClusterSearchConnection)

instance
  Core.AWSRequest
    DeleteInboundCrossClusterSearchConnection
  where
  type
    AWSResponse
      DeleteInboundCrossClusterSearchConnection =
      DeleteInboundCrossClusterSearchConnectionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInboundCrossClusterSearchConnectionResponse'
            Prelude.<$> (x Data..?> "CrossClusterSearchConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteInboundCrossClusterSearchConnection
  where
  hashWithSalt
    _salt
    DeleteInboundCrossClusterSearchConnection' {..} =
      _salt
        `Prelude.hashWithSalt` crossClusterSearchConnectionId

instance
  Prelude.NFData
    DeleteInboundCrossClusterSearchConnection
  where
  rnf DeleteInboundCrossClusterSearchConnection' {..} =
    Prelude.rnf crossClusterSearchConnectionId

instance
  Data.ToHeaders
    DeleteInboundCrossClusterSearchConnection
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteInboundCrossClusterSearchConnection
  where
  toPath DeleteInboundCrossClusterSearchConnection' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/ccs/inboundConnection/",
        Data.toBS crossClusterSearchConnectionId
      ]

instance
  Data.ToQuery
    DeleteInboundCrossClusterSearchConnection
  where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @DeleteInboundCrossClusterSearchConnection@ operation.
-- Contains details of deleted inbound connection.
--
-- /See:/ 'newDeleteInboundCrossClusterSearchConnectionResponse' smart constructor.
data DeleteInboundCrossClusterSearchConnectionResponse = DeleteInboundCrossClusterSearchConnectionResponse'
  { -- | Specifies the @InboundCrossClusterSearchConnection@ of deleted inbound
    -- connection.
    crossClusterSearchConnection :: Prelude.Maybe InboundCrossClusterSearchConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInboundCrossClusterSearchConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnection', 'deleteInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection' - Specifies the @InboundCrossClusterSearchConnection@ of deleted inbound
-- connection.
--
-- 'httpStatus', 'deleteInboundCrossClusterSearchConnectionResponse_httpStatus' - The response's http status code.
newDeleteInboundCrossClusterSearchConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteInboundCrossClusterSearchConnectionResponse
newDeleteInboundCrossClusterSearchConnectionResponse
  pHttpStatus_ =
    DeleteInboundCrossClusterSearchConnectionResponse'
      { crossClusterSearchConnection =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Specifies the @InboundCrossClusterSearchConnection@ of deleted inbound
-- connection.
deleteInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection :: Lens.Lens' DeleteInboundCrossClusterSearchConnectionResponse (Prelude.Maybe InboundCrossClusterSearchConnection)
deleteInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection = Lens.lens (\DeleteInboundCrossClusterSearchConnectionResponse' {crossClusterSearchConnection} -> crossClusterSearchConnection) (\s@DeleteInboundCrossClusterSearchConnectionResponse' {} a -> s {crossClusterSearchConnection = a} :: DeleteInboundCrossClusterSearchConnectionResponse)

-- | The response's http status code.
deleteInboundCrossClusterSearchConnectionResponse_httpStatus :: Lens.Lens' DeleteInboundCrossClusterSearchConnectionResponse Prelude.Int
deleteInboundCrossClusterSearchConnectionResponse_httpStatus = Lens.lens (\DeleteInboundCrossClusterSearchConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteInboundCrossClusterSearchConnectionResponse' {} a -> s {httpStatus = a} :: DeleteInboundCrossClusterSearchConnectionResponse)

instance
  Prelude.NFData
    DeleteInboundCrossClusterSearchConnectionResponse
  where
  rnf
    DeleteInboundCrossClusterSearchConnectionResponse' {..} =
      Prelude.rnf crossClusterSearchConnection
        `Prelude.seq` Prelude.rnf httpStatus
