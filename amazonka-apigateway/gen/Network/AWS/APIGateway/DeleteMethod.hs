{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.DeleteMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Method resource.
module Network.AWS.APIGateway.DeleteMethod
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

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete an existing Method resource.
--
-- /See:/ 'newDeleteMethod' smart constructor.
data DeleteMethod = DeleteMethod'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The Resource identifier for the Method resource.
    resourceId :: Prelude.Text,
    -- | [Required] The HTTP verb of the Method resource.
    httpMethod :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteMethod_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'deleteMethod_resourceId' - [Required] The Resource identifier for the Method resource.
--
-- 'httpMethod', 'deleteMethod_httpMethod' - [Required] The HTTP verb of the Method resource.
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

-- | [Required] The string identifier of the associated RestApi.
deleteMethod_restApiId :: Lens.Lens' DeleteMethod Prelude.Text
deleteMethod_restApiId = Lens.lens (\DeleteMethod' {restApiId} -> restApiId) (\s@DeleteMethod' {} a -> s {restApiId = a} :: DeleteMethod)

-- | [Required] The Resource identifier for the Method resource.
deleteMethod_resourceId :: Lens.Lens' DeleteMethod Prelude.Text
deleteMethod_resourceId = Lens.lens (\DeleteMethod' {resourceId} -> resourceId) (\s@DeleteMethod' {} a -> s {resourceId = a} :: DeleteMethod)

-- | [Required] The HTTP verb of the Method resource.
deleteMethod_httpMethod :: Lens.Lens' DeleteMethod Prelude.Text
deleteMethod_httpMethod = Lens.lens (\DeleteMethod' {httpMethod} -> httpMethod) (\s@DeleteMethod' {} a -> s {httpMethod = a} :: DeleteMethod)

instance Prelude.AWSRequest DeleteMethod where
  type Rs DeleteMethod = DeleteMethodResponse'
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteMethodResponse''

instance Prelude.Hashable DeleteMethod

instance Prelude.NFData DeleteMethod

instance Prelude.ToHeaders DeleteMethod where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteMethod where
  toPath DeleteMethod' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/resources/",
        Prelude.toBS resourceId,
        "/methods/",
        Prelude.toBS httpMethod
      ]

instance Prelude.ToQuery DeleteMethod where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMethodResponse'' smart constructor.
data DeleteMethodResponse' = DeleteMethodResponse''
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteMethodResponse'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMethodResponse' ::
  DeleteMethodResponse'
newDeleteMethodResponse' = DeleteMethodResponse''

instance Prelude.NFData DeleteMethodResponse'
