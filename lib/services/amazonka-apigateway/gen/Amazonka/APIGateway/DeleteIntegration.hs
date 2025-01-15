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
-- Module      : Amazonka.APIGateway.DeleteIntegration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a delete integration.
module Amazonka.APIGateway.DeleteIntegration
  ( -- * Creating a Request
    DeleteIntegration (..),
    newDeleteIntegration,

    -- * Request Lenses
    deleteIntegration_restApiId,
    deleteIntegration_resourceId,
    deleteIntegration_httpMethod,

    -- * Destructuring the Response
    DeleteIntegrationResponse' (..),
    newDeleteIntegrationResponse',
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a delete integration request.
--
-- /See:/ 'newDeleteIntegration' smart constructor.
data DeleteIntegration = DeleteIntegration'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | Specifies a delete integration request\'s resource identifier.
    resourceId :: Prelude.Text,
    -- | Specifies a delete integration request\'s HTTP method.
    httpMethod :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteIntegration_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'deleteIntegration_resourceId' - Specifies a delete integration request\'s resource identifier.
--
-- 'httpMethod', 'deleteIntegration_httpMethod' - Specifies a delete integration request\'s HTTP method.
newDeleteIntegration ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  DeleteIntegration
newDeleteIntegration
  pRestApiId_
  pResourceId_
  pHttpMethod_ =
    DeleteIntegration'
      { restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_
      }

-- | The string identifier of the associated RestApi.
deleteIntegration_restApiId :: Lens.Lens' DeleteIntegration Prelude.Text
deleteIntegration_restApiId = Lens.lens (\DeleteIntegration' {restApiId} -> restApiId) (\s@DeleteIntegration' {} a -> s {restApiId = a} :: DeleteIntegration)

-- | Specifies a delete integration request\'s resource identifier.
deleteIntegration_resourceId :: Lens.Lens' DeleteIntegration Prelude.Text
deleteIntegration_resourceId = Lens.lens (\DeleteIntegration' {resourceId} -> resourceId) (\s@DeleteIntegration' {} a -> s {resourceId = a} :: DeleteIntegration)

-- | Specifies a delete integration request\'s HTTP method.
deleteIntegration_httpMethod :: Lens.Lens' DeleteIntegration Prelude.Text
deleteIntegration_httpMethod = Lens.lens (\DeleteIntegration' {httpMethod} -> httpMethod) (\s@DeleteIntegration' {} a -> s {httpMethod = a} :: DeleteIntegration)

instance Core.AWSRequest DeleteIntegration where
  type
    AWSResponse DeleteIntegration =
      DeleteIntegrationResponse'
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteIntegrationResponse''

instance Prelude.Hashable DeleteIntegration where
  hashWithSalt _salt DeleteIntegration' {..} =
    _salt
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod

instance Prelude.NFData DeleteIntegration where
  rnf DeleteIntegration' {..} =
    Prelude.rnf restApiId `Prelude.seq`
      Prelude.rnf resourceId `Prelude.seq`
        Prelude.rnf httpMethod

instance Data.ToHeaders DeleteIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteIntegration where
  toPath DeleteIntegration' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS resourceId,
        "/methods/",
        Data.toBS httpMethod,
        "/integration"
      ]

instance Data.ToQuery DeleteIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIntegrationResponse'' smart constructor.
data DeleteIntegrationResponse' = DeleteIntegrationResponse''
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntegrationResponse'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntegrationResponse' ::
  DeleteIntegrationResponse'
newDeleteIntegrationResponse' =
  DeleteIntegrationResponse''

instance Prelude.NFData DeleteIntegrationResponse' where
  rnf _ = ()
