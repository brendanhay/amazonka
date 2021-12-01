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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a delete integration request.
--
-- /See:/ 'newDeleteIntegration' smart constructor.
data DeleteIntegration = DeleteIntegration'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] Specifies a delete integration request\'s resource
    -- identifier.
    resourceId :: Prelude.Text,
    -- | [Required] Specifies a delete integration request\'s HTTP method.
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
-- 'restApiId', 'deleteIntegration_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'deleteIntegration_resourceId' - [Required] Specifies a delete integration request\'s resource
-- identifier.
--
-- 'httpMethod', 'deleteIntegration_httpMethod' - [Required] Specifies a delete integration request\'s HTTP method.
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

-- | [Required] The string identifier of the associated RestApi.
deleteIntegration_restApiId :: Lens.Lens' DeleteIntegration Prelude.Text
deleteIntegration_restApiId = Lens.lens (\DeleteIntegration' {restApiId} -> restApiId) (\s@DeleteIntegration' {} a -> s {restApiId = a} :: DeleteIntegration)

-- | [Required] Specifies a delete integration request\'s resource
-- identifier.
deleteIntegration_resourceId :: Lens.Lens' DeleteIntegration Prelude.Text
deleteIntegration_resourceId = Lens.lens (\DeleteIntegration' {resourceId} -> resourceId) (\s@DeleteIntegration' {} a -> s {resourceId = a} :: DeleteIntegration)

-- | [Required] Specifies a delete integration request\'s HTTP method.
deleteIntegration_httpMethod :: Lens.Lens' DeleteIntegration Prelude.Text
deleteIntegration_httpMethod = Lens.lens (\DeleteIntegration' {httpMethod} -> httpMethod) (\s@DeleteIntegration' {} a -> s {httpMethod = a} :: DeleteIntegration)

instance Core.AWSRequest DeleteIntegration where
  type
    AWSResponse DeleteIntegration =
      DeleteIntegrationResponse'
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteIntegrationResponse''

instance Prelude.Hashable DeleteIntegration where
  hashWithSalt salt' DeleteIntegration' {..} =
    salt' `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData DeleteIntegration where
  rnf DeleteIntegration' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf resourceId

instance Core.ToHeaders DeleteIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath DeleteIntegration where
  toPath DeleteIntegration' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId,
        "/methods/",
        Core.toBS httpMethod,
        "/integration"
      ]

instance Core.ToQuery DeleteIntegration where
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
