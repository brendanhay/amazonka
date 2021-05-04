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
-- Module      : Network.AWS.APIGateway.DeleteResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Resource resource.
module Network.AWS.APIGateway.DeleteResource
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

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete a Resource.
--
-- /See:/ 'newDeleteResource' smart constructor.
data DeleteResource = DeleteResource'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The identifier of the Resource resource.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteResource_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'deleteResource_resourceId' - [Required] The identifier of the Resource resource.
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

-- | [Required] The string identifier of the associated RestApi.
deleteResource_restApiId :: Lens.Lens' DeleteResource Prelude.Text
deleteResource_restApiId = Lens.lens (\DeleteResource' {restApiId} -> restApiId) (\s@DeleteResource' {} a -> s {restApiId = a} :: DeleteResource)

-- | [Required] The identifier of the Resource resource.
deleteResource_resourceId :: Lens.Lens' DeleteResource Prelude.Text
deleteResource_resourceId = Lens.lens (\DeleteResource' {resourceId} -> resourceId) (\s@DeleteResource' {} a -> s {resourceId = a} :: DeleteResource)

instance Prelude.AWSRequest DeleteResource where
  type Rs DeleteResource = DeleteResourceResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteResourceResponse'

instance Prelude.Hashable DeleteResource

instance Prelude.NFData DeleteResource

instance Prelude.ToHeaders DeleteResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteResource where
  toPath DeleteResource' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/resources/",
        Prelude.toBS resourceId
      ]

instance Prelude.ToQuery DeleteResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourceResponse' smart constructor.
data DeleteResourceResponse = DeleteResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteResourceResponse ::
  DeleteResourceResponse
newDeleteResourceResponse = DeleteResourceResponse'

instance Prelude.NFData DeleteResourceResponse
