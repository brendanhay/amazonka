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
-- Module      : Amazonka.APIGateway.DeleteResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Resource resource.
module Amazonka.APIGateway.DeleteResource
  ( -- * Creating a Request
    DeleteResource (..),
    newDeleteResource,

    -- * Request Lenses
    deleteResource_restApiId,
    deleteResource_resourceId,

    -- * Destructuring the Response
    DeleteResourceResponse (..),
    newDeleteResourceResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to delete a Resource.
--
-- /See:/ 'newDeleteResource' smart constructor.
data DeleteResource = DeleteResource'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The identifier of the Resource resource.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteResource_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'deleteResource_resourceId' - The identifier of the Resource resource.
newDeleteResource ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  DeleteResource
newDeleteResource pRestApiId_ pResourceId_ =
  DeleteResource'
    { restApiId = pRestApiId_,
      resourceId = pResourceId_
    }

-- | The string identifier of the associated RestApi.
deleteResource_restApiId :: Lens.Lens' DeleteResource Prelude.Text
deleteResource_restApiId = Lens.lens (\DeleteResource' {restApiId} -> restApiId) (\s@DeleteResource' {} a -> s {restApiId = a} :: DeleteResource)

-- | The identifier of the Resource resource.
deleteResource_resourceId :: Lens.Lens' DeleteResource Prelude.Text
deleteResource_resourceId = Lens.lens (\DeleteResource' {resourceId} -> resourceId) (\s@DeleteResource' {} a -> s {resourceId = a} :: DeleteResource)

instance Core.AWSRequest DeleteResource where
  type
    AWSResponse DeleteResource =
      DeleteResourceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteResourceResponse'

instance Prelude.Hashable DeleteResource where
  hashWithSalt _salt DeleteResource' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData DeleteResource where
  rnf DeleteResource' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId

instance Core.ToHeaders DeleteResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath DeleteResource where
  toPath DeleteResource' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId
      ]

instance Core.ToQuery DeleteResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourceResponse' smart constructor.
data DeleteResourceResponse = DeleteResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteResourceResponse ::
  DeleteResourceResponse
newDeleteResourceResponse = DeleteResourceResponse'

instance Prelude.NFData DeleteResourceResponse where
  rnf _ = ()
