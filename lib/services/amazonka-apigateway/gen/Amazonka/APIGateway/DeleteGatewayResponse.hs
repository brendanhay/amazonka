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
-- Module      : Amazonka.APIGateway.DeleteGatewayResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Clears any customization of a GatewayResponse of a specified response
-- type on the given RestApi and resets it with the default settings.
module Amazonka.APIGateway.DeleteGatewayResponse
  ( -- * Creating a Request
    DeleteGatewayResponse (..),
    newDeleteGatewayResponse,

    -- * Request Lenses
    deleteGatewayResponse_restApiId,
    deleteGatewayResponse_responseType,

    -- * Destructuring the Response
    DeleteGatewayResponseResponse (..),
    newDeleteGatewayResponseResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Clears any customization of a GatewayResponse of a specified response
-- type on the given RestApi and resets it with the default settings.
--
-- /See:/ 'newDeleteGatewayResponse' smart constructor.
data DeleteGatewayResponse = DeleteGatewayResponse'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The response type of the associated GatewayResponse.
    responseType :: GatewayResponseType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteGatewayResponse_restApiId' - The string identifier of the associated RestApi.
--
-- 'responseType', 'deleteGatewayResponse_responseType' - The response type of the associated GatewayResponse.
newDeleteGatewayResponse ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  DeleteGatewayResponse
newDeleteGatewayResponse pRestApiId_ pResponseType_ =
  DeleteGatewayResponse'
    { restApiId = pRestApiId_,
      responseType = pResponseType_
    }

-- | The string identifier of the associated RestApi.
deleteGatewayResponse_restApiId :: Lens.Lens' DeleteGatewayResponse Prelude.Text
deleteGatewayResponse_restApiId = Lens.lens (\DeleteGatewayResponse' {restApiId} -> restApiId) (\s@DeleteGatewayResponse' {} a -> s {restApiId = a} :: DeleteGatewayResponse)

-- | The response type of the associated GatewayResponse.
deleteGatewayResponse_responseType :: Lens.Lens' DeleteGatewayResponse GatewayResponseType
deleteGatewayResponse_responseType = Lens.lens (\DeleteGatewayResponse' {responseType} -> responseType) (\s@DeleteGatewayResponse' {} a -> s {responseType = a} :: DeleteGatewayResponse)

instance Core.AWSRequest DeleteGatewayResponse where
  type
    AWSResponse DeleteGatewayResponse =
      DeleteGatewayResponseResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteGatewayResponseResponse'

instance Prelude.Hashable DeleteGatewayResponse where
  hashWithSalt _salt DeleteGatewayResponse' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` responseType

instance Prelude.NFData DeleteGatewayResponse where
  rnf DeleteGatewayResponse' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf responseType

instance Data.ToHeaders DeleteGatewayResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteGatewayResponse where
  toPath DeleteGatewayResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/gatewayresponses/",
        Data.toBS responseType
      ]

instance Data.ToQuery DeleteGatewayResponse where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGatewayResponseResponse' smart constructor.
data DeleteGatewayResponseResponse = DeleteGatewayResponseResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGatewayResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteGatewayResponseResponse ::
  DeleteGatewayResponseResponse
newDeleteGatewayResponseResponse =
  DeleteGatewayResponseResponse'

instance Prelude.NFData DeleteGatewayResponseResponse where
  rnf _ = ()
