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
-- Module      : Amazonka.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the source domain owner to delete an existing outbound
-- cross-cluster search connection.
module Amazonka.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
  ( -- * Creating a Request
    DeleteOutboundCrossClusterSearchConnection (..),
    newDeleteOutboundCrossClusterSearchConnection,

    -- * Request Lenses
    deleteOutboundCrossClusterSearchConnection_crossClusterSearchConnectionId,

    -- * Destructuring the Response
    DeleteOutboundCrossClusterSearchConnectionResponse (..),
    newDeleteOutboundCrossClusterSearchConnectionResponse,

    -- * Response Lenses
    deleteOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    deleteOutboundCrossClusterSearchConnectionResponse_httpStatus,
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
-- @DeleteOutboundCrossClusterSearchConnection@ operation.
--
-- /See:/ 'newDeleteOutboundCrossClusterSearchConnection' smart constructor.
data DeleteOutboundCrossClusterSearchConnection = DeleteOutboundCrossClusterSearchConnection'
  { -- | The id of the outbound connection that you want to permanently delete.
    crossClusterSearchConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOutboundCrossClusterSearchConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnectionId', 'deleteOutboundCrossClusterSearchConnection_crossClusterSearchConnectionId' - The id of the outbound connection that you want to permanently delete.
newDeleteOutboundCrossClusterSearchConnection ::
  -- | 'crossClusterSearchConnectionId'
  Prelude.Text ->
  DeleteOutboundCrossClusterSearchConnection
newDeleteOutboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    DeleteOutboundCrossClusterSearchConnection'
      { crossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the outbound connection that you want to permanently delete.
deleteOutboundCrossClusterSearchConnection_crossClusterSearchConnectionId :: Lens.Lens' DeleteOutboundCrossClusterSearchConnection Prelude.Text
deleteOutboundCrossClusterSearchConnection_crossClusterSearchConnectionId = Lens.lens (\DeleteOutboundCrossClusterSearchConnection' {crossClusterSearchConnectionId} -> crossClusterSearchConnectionId) (\s@DeleteOutboundCrossClusterSearchConnection' {} a -> s {crossClusterSearchConnectionId = a} :: DeleteOutboundCrossClusterSearchConnection)

instance
  Core.AWSRequest
    DeleteOutboundCrossClusterSearchConnection
  where
  type
    AWSResponse
      DeleteOutboundCrossClusterSearchConnection =
      DeleteOutboundCrossClusterSearchConnectionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteOutboundCrossClusterSearchConnectionResponse'
            Prelude.<$> (x Data..?> "CrossClusterSearchConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteOutboundCrossClusterSearchConnection
  where
  hashWithSalt
    _salt
    DeleteOutboundCrossClusterSearchConnection' {..} =
      _salt
        `Prelude.hashWithSalt` crossClusterSearchConnectionId

instance
  Prelude.NFData
    DeleteOutboundCrossClusterSearchConnection
  where
  rnf DeleteOutboundCrossClusterSearchConnection' {..} =
    Prelude.rnf crossClusterSearchConnectionId

instance
  Data.ToHeaders
    DeleteOutboundCrossClusterSearchConnection
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteOutboundCrossClusterSearchConnection
  where
  toPath
    DeleteOutboundCrossClusterSearchConnection' {..} =
      Prelude.mconcat
        [ "/2015-01-01/es/ccs/outboundConnection/",
          Data.toBS crossClusterSearchConnectionId
        ]

instance
  Data.ToQuery
    DeleteOutboundCrossClusterSearchConnection
  where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @DeleteOutboundCrossClusterSearchConnection@ operation.
-- Contains details of deleted outbound connection.
--
-- /See:/ 'newDeleteOutboundCrossClusterSearchConnectionResponse' smart constructor.
data DeleteOutboundCrossClusterSearchConnectionResponse = DeleteOutboundCrossClusterSearchConnectionResponse'
  { -- | Specifies the @OutboundCrossClusterSearchConnection@ of deleted outbound
    -- connection.
    crossClusterSearchConnection :: Prelude.Maybe OutboundCrossClusterSearchConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOutboundCrossClusterSearchConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnection', 'deleteOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection' - Specifies the @OutboundCrossClusterSearchConnection@ of deleted outbound
-- connection.
--
-- 'httpStatus', 'deleteOutboundCrossClusterSearchConnectionResponse_httpStatus' - The response's http status code.
newDeleteOutboundCrossClusterSearchConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteOutboundCrossClusterSearchConnectionResponse
newDeleteOutboundCrossClusterSearchConnectionResponse
  pHttpStatus_ =
    DeleteOutboundCrossClusterSearchConnectionResponse'
      { crossClusterSearchConnection =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Specifies the @OutboundCrossClusterSearchConnection@ of deleted outbound
-- connection.
deleteOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection :: Lens.Lens' DeleteOutboundCrossClusterSearchConnectionResponse (Prelude.Maybe OutboundCrossClusterSearchConnection)
deleteOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection = Lens.lens (\DeleteOutboundCrossClusterSearchConnectionResponse' {crossClusterSearchConnection} -> crossClusterSearchConnection) (\s@DeleteOutboundCrossClusterSearchConnectionResponse' {} a -> s {crossClusterSearchConnection = a} :: DeleteOutboundCrossClusterSearchConnectionResponse)

-- | The response's http status code.
deleteOutboundCrossClusterSearchConnectionResponse_httpStatus :: Lens.Lens' DeleteOutboundCrossClusterSearchConnectionResponse Prelude.Int
deleteOutboundCrossClusterSearchConnectionResponse_httpStatus = Lens.lens (\DeleteOutboundCrossClusterSearchConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteOutboundCrossClusterSearchConnectionResponse' {} a -> s {httpStatus = a} :: DeleteOutboundCrossClusterSearchConnectionResponse)

instance
  Prelude.NFData
    DeleteOutboundCrossClusterSearchConnectionResponse
  where
  rnf
    DeleteOutboundCrossClusterSearchConnectionResponse' {..} =
      Prelude.rnf crossClusterSearchConnection `Prelude.seq`
        Prelude.rnf httpStatus
