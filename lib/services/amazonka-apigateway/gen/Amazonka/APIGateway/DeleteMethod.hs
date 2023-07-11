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
-- Module      : Amazonka.APIGateway.DeleteMethod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Method resource.
module Amazonka.APIGateway.DeleteMethod
  ( -- * Creating a Request
    DeleteMethod (..),
    newDeleteMethod,

    -- * Request Lenses
    deleteMethod_restApiId,
    deleteMethod_resourceId,
    deleteMethod_httpMethod,

    -- * Destructuring the Response
    DeleteMethodResponse' (..),
    newDeleteMethodResponse',
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to delete an existing Method resource.
--
-- /See:/ 'newDeleteMethod' smart constructor.
data DeleteMethod = DeleteMethod'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The Resource identifier for the Method resource.
    resourceId :: Prelude.Text,
    -- | The HTTP verb of the Method resource.
    httpMethod :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteMethod_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'deleteMethod_resourceId' - The Resource identifier for the Method resource.
--
-- 'httpMethod', 'deleteMethod_httpMethod' - The HTTP verb of the Method resource.
newDeleteMethod ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  DeleteMethod
newDeleteMethod pRestApiId_ pResourceId_ pHttpMethod_ =
  DeleteMethod'
    { restApiId = pRestApiId_,
      resourceId = pResourceId_,
      httpMethod = pHttpMethod_
    }

-- | The string identifier of the associated RestApi.
deleteMethod_restApiId :: Lens.Lens' DeleteMethod Prelude.Text
deleteMethod_restApiId = Lens.lens (\DeleteMethod' {restApiId} -> restApiId) (\s@DeleteMethod' {} a -> s {restApiId = a} :: DeleteMethod)

-- | The Resource identifier for the Method resource.
deleteMethod_resourceId :: Lens.Lens' DeleteMethod Prelude.Text
deleteMethod_resourceId = Lens.lens (\DeleteMethod' {resourceId} -> resourceId) (\s@DeleteMethod' {} a -> s {resourceId = a} :: DeleteMethod)

-- | The HTTP verb of the Method resource.
deleteMethod_httpMethod :: Lens.Lens' DeleteMethod Prelude.Text
deleteMethod_httpMethod = Lens.lens (\DeleteMethod' {httpMethod} -> httpMethod) (\s@DeleteMethod' {} a -> s {httpMethod = a} :: DeleteMethod)

instance Core.AWSRequest DeleteMethod where
  type AWSResponse DeleteMethod = DeleteMethodResponse'
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteMethodResponse''

instance Prelude.Hashable DeleteMethod where
  hashWithSalt _salt DeleteMethod' {..} =
    _salt
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod

instance Prelude.NFData DeleteMethod where
  rnf DeleteMethod' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpMethod

instance Data.ToHeaders DeleteMethod where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteMethod where
  toPath DeleteMethod' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS resourceId,
        "/methods/",
        Data.toBS httpMethod
      ]

instance Data.ToQuery DeleteMethod where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMethodResponse'' smart constructor.
data DeleteMethodResponse' = DeleteMethodResponse''
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMethodResponse'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMethodResponse' ::
  DeleteMethodResponse'
newDeleteMethodResponse' = DeleteMethodResponse''

instance Prelude.NFData DeleteMethodResponse' where
  rnf _ = ()
