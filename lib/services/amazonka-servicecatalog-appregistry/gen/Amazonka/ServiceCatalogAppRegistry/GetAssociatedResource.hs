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
-- Module      : Amazonka.ServiceCatalogAppRegistry.GetAssociatedResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the resource associated with the application.
module Amazonka.ServiceCatalogAppRegistry.GetAssociatedResource
  ( -- * Creating a Request
    GetAssociatedResource (..),
    newGetAssociatedResource,

    -- * Request Lenses
    getAssociatedResource_application,
    getAssociatedResource_resourceType,
    getAssociatedResource_resource,

    -- * Destructuring the Response
    GetAssociatedResourceResponse (..),
    newGetAssociatedResourceResponse,

    -- * Response Lenses
    getAssociatedResourceResponse_resource,
    getAssociatedResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newGetAssociatedResource' smart constructor.
data GetAssociatedResource = GetAssociatedResource'
  { -- | The name or ID of the application.
    application :: Prelude.Text,
    -- | The type of resource associated with the application.
    resourceType :: ResourceType,
    -- | The name or ID of the resource associated with the application.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssociatedResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'application', 'getAssociatedResource_application' - The name or ID of the application.
--
-- 'resourceType', 'getAssociatedResource_resourceType' - The type of resource associated with the application.
--
-- 'resource', 'getAssociatedResource_resource' - The name or ID of the resource associated with the application.
newGetAssociatedResource ::
  -- | 'application'
  Prelude.Text ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'resource'
  Prelude.Text ->
  GetAssociatedResource
newGetAssociatedResource
  pApplication_
  pResourceType_
  pResource_ =
    GetAssociatedResource'
      { application = pApplication_,
        resourceType = pResourceType_,
        resource = pResource_
      }

-- | The name or ID of the application.
getAssociatedResource_application :: Lens.Lens' GetAssociatedResource Prelude.Text
getAssociatedResource_application = Lens.lens (\GetAssociatedResource' {application} -> application) (\s@GetAssociatedResource' {} a -> s {application = a} :: GetAssociatedResource)

-- | The type of resource associated with the application.
getAssociatedResource_resourceType :: Lens.Lens' GetAssociatedResource ResourceType
getAssociatedResource_resourceType = Lens.lens (\GetAssociatedResource' {resourceType} -> resourceType) (\s@GetAssociatedResource' {} a -> s {resourceType = a} :: GetAssociatedResource)

-- | The name or ID of the resource associated with the application.
getAssociatedResource_resource :: Lens.Lens' GetAssociatedResource Prelude.Text
getAssociatedResource_resource = Lens.lens (\GetAssociatedResource' {resource} -> resource) (\s@GetAssociatedResource' {} a -> s {resource = a} :: GetAssociatedResource)

instance Core.AWSRequest GetAssociatedResource where
  type
    AWSResponse GetAssociatedResource =
      GetAssociatedResourceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssociatedResourceResponse'
            Prelude.<$> (x Core..?> "resource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAssociatedResource where
  hashWithSalt _salt GetAssociatedResource' {..} =
    _salt `Prelude.hashWithSalt` application
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resource

instance Prelude.NFData GetAssociatedResource where
  rnf GetAssociatedResource' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resource

instance Core.ToHeaders GetAssociatedResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetAssociatedResource where
  toPath GetAssociatedResource' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS application,
        "/resources/",
        Core.toBS resourceType,
        "/",
        Core.toBS resource
      ]

instance Core.ToQuery GetAssociatedResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssociatedResourceResponse' smart constructor.
data GetAssociatedResourceResponse = GetAssociatedResourceResponse'
  { -- | The resource associated with the application.
    resource :: Prelude.Maybe Resource,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssociatedResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'getAssociatedResourceResponse_resource' - The resource associated with the application.
--
-- 'httpStatus', 'getAssociatedResourceResponse_httpStatus' - The response's http status code.
newGetAssociatedResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssociatedResourceResponse
newGetAssociatedResourceResponse pHttpStatus_ =
  GetAssociatedResourceResponse'
    { resource =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource associated with the application.
getAssociatedResourceResponse_resource :: Lens.Lens' GetAssociatedResourceResponse (Prelude.Maybe Resource)
getAssociatedResourceResponse_resource = Lens.lens (\GetAssociatedResourceResponse' {resource} -> resource) (\s@GetAssociatedResourceResponse' {} a -> s {resource = a} :: GetAssociatedResourceResponse)

-- | The response's http status code.
getAssociatedResourceResponse_httpStatus :: Lens.Lens' GetAssociatedResourceResponse Prelude.Int
getAssociatedResourceResponse_httpStatus = Lens.lens (\GetAssociatedResourceResponse' {httpStatus} -> httpStatus) (\s@GetAssociatedResourceResponse' {} a -> s {httpStatus = a} :: GetAssociatedResourceResponse)

instance Prelude.NFData GetAssociatedResourceResponse where
  rnf GetAssociatedResourceResponse' {..} =
    Prelude.rnf resource
      `Prelude.seq` Prelude.rnf httpStatus
