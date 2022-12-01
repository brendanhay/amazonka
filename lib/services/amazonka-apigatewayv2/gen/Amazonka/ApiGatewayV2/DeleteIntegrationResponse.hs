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
-- Module      : Amazonka.ApiGatewayV2.DeleteIntegrationResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an IntegrationResponses.
module Amazonka.ApiGatewayV2.DeleteIntegrationResponse
  ( -- * Creating a Request
    DeleteIntegrationResponse (..),
    newDeleteIntegrationResponse,

    -- * Request Lenses
    deleteIntegrationResponse_apiId,
    deleteIntegrationResponse_integrationResponseId,
    deleteIntegrationResponse_integrationId,

    -- * Destructuring the Response
    DeleteIntegrationResponseResponse (..),
    newDeleteIntegrationResponseResponse,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteIntegrationResponse' smart constructor.
data DeleteIntegrationResponse = DeleteIntegrationResponse'
  { -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The integration response ID.
    integrationResponseId :: Prelude.Text,
    -- | The integration ID.
    integrationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'deleteIntegrationResponse_apiId' - The API identifier.
--
-- 'integrationResponseId', 'deleteIntegrationResponse_integrationResponseId' - The integration response ID.
--
-- 'integrationId', 'deleteIntegrationResponse_integrationId' - The integration ID.
newDeleteIntegrationResponse ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'integrationResponseId'
  Prelude.Text ->
  -- | 'integrationId'
  Prelude.Text ->
  DeleteIntegrationResponse
newDeleteIntegrationResponse
  pApiId_
  pIntegrationResponseId_
  pIntegrationId_ =
    DeleteIntegrationResponse'
      { apiId = pApiId_,
        integrationResponseId = pIntegrationResponseId_,
        integrationId = pIntegrationId_
      }

-- | The API identifier.
deleteIntegrationResponse_apiId :: Lens.Lens' DeleteIntegrationResponse Prelude.Text
deleteIntegrationResponse_apiId = Lens.lens (\DeleteIntegrationResponse' {apiId} -> apiId) (\s@DeleteIntegrationResponse' {} a -> s {apiId = a} :: DeleteIntegrationResponse)

-- | The integration response ID.
deleteIntegrationResponse_integrationResponseId :: Lens.Lens' DeleteIntegrationResponse Prelude.Text
deleteIntegrationResponse_integrationResponseId = Lens.lens (\DeleteIntegrationResponse' {integrationResponseId} -> integrationResponseId) (\s@DeleteIntegrationResponse' {} a -> s {integrationResponseId = a} :: DeleteIntegrationResponse)

-- | The integration ID.
deleteIntegrationResponse_integrationId :: Lens.Lens' DeleteIntegrationResponse Prelude.Text
deleteIntegrationResponse_integrationId = Lens.lens (\DeleteIntegrationResponse' {integrationId} -> integrationId) (\s@DeleteIntegrationResponse' {} a -> s {integrationId = a} :: DeleteIntegrationResponse)

instance Core.AWSRequest DeleteIntegrationResponse where
  type
    AWSResponse DeleteIntegrationResponse =
      DeleteIntegrationResponseResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteIntegrationResponseResponse'

instance Prelude.Hashable DeleteIntegrationResponse where
  hashWithSalt _salt DeleteIntegrationResponse' {..} =
    _salt `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` integrationResponseId
      `Prelude.hashWithSalt` integrationId

instance Prelude.NFData DeleteIntegrationResponse where
  rnf DeleteIntegrationResponse' {..} =
    Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf integrationResponseId
      `Prelude.seq` Prelude.rnf integrationId

instance Core.ToHeaders DeleteIntegrationResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteIntegrationResponse where
  toPath DeleteIntegrationResponse' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/integrations/",
        Core.toBS integrationId,
        "/integrationresponses/",
        Core.toBS integrationResponseId
      ]

instance Core.ToQuery DeleteIntegrationResponse where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIntegrationResponseResponse' smart constructor.
data DeleteIntegrationResponseResponse = DeleteIntegrationResponseResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntegrationResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntegrationResponseResponse ::
  DeleteIntegrationResponseResponse
newDeleteIntegrationResponseResponse =
  DeleteIntegrationResponseResponse'

instance
  Prelude.NFData
    DeleteIntegrationResponseResponse
  where
  rnf _ = ()
