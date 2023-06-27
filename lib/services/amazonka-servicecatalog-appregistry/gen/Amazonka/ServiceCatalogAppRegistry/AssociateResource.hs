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
-- Module      : Amazonka.ServiceCatalogAppRegistry.AssociateResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a resource with an application. The resource can be specified
-- by its ARN or name. The application can be specified by ARN, ID, or
-- name.
module Amazonka.ServiceCatalogAppRegistry.AssociateResource
  ( -- * Creating a Request
    AssociateResource (..),
    newAssociateResource,

    -- * Request Lenses
    associateResource_application,
    associateResource_resourceType,
    associateResource_resource,

    -- * Destructuring the Response
    AssociateResourceResponse (..),
    newAssociateResourceResponse,

    -- * Response Lenses
    associateResourceResponse_applicationArn,
    associateResourceResponse_resourceArn,
    associateResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newAssociateResource' smart constructor.
data AssociateResource = AssociateResource'
  { -- | The name, ID, or ARN of the application.
    application :: Prelude.Text,
    -- | The type of resource of which the application will be associated.
    resourceType :: ResourceType,
    -- | The name or ID of the resource of which the application will be
    -- associated.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'application', 'associateResource_application' - The name, ID, or ARN of the application.
--
-- 'resourceType', 'associateResource_resourceType' - The type of resource of which the application will be associated.
--
-- 'resource', 'associateResource_resource' - The name or ID of the resource of which the application will be
-- associated.
newAssociateResource ::
  -- | 'application'
  Prelude.Text ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'resource'
  Prelude.Text ->
  AssociateResource
newAssociateResource
  pApplication_
  pResourceType_
  pResource_ =
    AssociateResource'
      { application = pApplication_,
        resourceType = pResourceType_,
        resource = pResource_
      }

-- | The name, ID, or ARN of the application.
associateResource_application :: Lens.Lens' AssociateResource Prelude.Text
associateResource_application = Lens.lens (\AssociateResource' {application} -> application) (\s@AssociateResource' {} a -> s {application = a} :: AssociateResource)

-- | The type of resource of which the application will be associated.
associateResource_resourceType :: Lens.Lens' AssociateResource ResourceType
associateResource_resourceType = Lens.lens (\AssociateResource' {resourceType} -> resourceType) (\s@AssociateResource' {} a -> s {resourceType = a} :: AssociateResource)

-- | The name or ID of the resource of which the application will be
-- associated.
associateResource_resource :: Lens.Lens' AssociateResource Prelude.Text
associateResource_resource = Lens.lens (\AssociateResource' {resource} -> resource) (\s@AssociateResource' {} a -> s {resource = a} :: AssociateResource)

instance Core.AWSRequest AssociateResource where
  type
    AWSResponse AssociateResource =
      AssociateResourceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateResourceResponse'
            Prelude.<$> (x Data..?> "applicationArn")
            Prelude.<*> (x Data..?> "resourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateResource where
  hashWithSalt _salt AssociateResource' {..} =
    _salt
      `Prelude.hashWithSalt` application
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resource

instance Prelude.NFData AssociateResource where
  rnf AssociateResource' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resource

instance Data.ToHeaders AssociateResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateResource where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath AssociateResource where
  toPath AssociateResource' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS application,
        "/resources/",
        Data.toBS resourceType,
        "/",
        Data.toBS resource
      ]

instance Data.ToQuery AssociateResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateResourceResponse' smart constructor.
data AssociateResourceResponse = AssociateResourceResponse'
  { -- | The Amazon resource name (ARN) of the application that was augmented
    -- with attributes.
    applicationArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon resource name (ARN) that specifies the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationArn', 'associateResourceResponse_applicationArn' - The Amazon resource name (ARN) of the application that was augmented
-- with attributes.
--
-- 'resourceArn', 'associateResourceResponse_resourceArn' - The Amazon resource name (ARN) that specifies the resource.
--
-- 'httpStatus', 'associateResourceResponse_httpStatus' - The response's http status code.
newAssociateResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateResourceResponse
newAssociateResourceResponse pHttpStatus_ =
  AssociateResourceResponse'
    { applicationArn =
        Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon resource name (ARN) of the application that was augmented
-- with attributes.
associateResourceResponse_applicationArn :: Lens.Lens' AssociateResourceResponse (Prelude.Maybe Prelude.Text)
associateResourceResponse_applicationArn = Lens.lens (\AssociateResourceResponse' {applicationArn} -> applicationArn) (\s@AssociateResourceResponse' {} a -> s {applicationArn = a} :: AssociateResourceResponse)

-- | The Amazon resource name (ARN) that specifies the resource.
associateResourceResponse_resourceArn :: Lens.Lens' AssociateResourceResponse (Prelude.Maybe Prelude.Text)
associateResourceResponse_resourceArn = Lens.lens (\AssociateResourceResponse' {resourceArn} -> resourceArn) (\s@AssociateResourceResponse' {} a -> s {resourceArn = a} :: AssociateResourceResponse)

-- | The response's http status code.
associateResourceResponse_httpStatus :: Lens.Lens' AssociateResourceResponse Prelude.Int
associateResourceResponse_httpStatus = Lens.lens (\AssociateResourceResponse' {httpStatus} -> httpStatus) (\s@AssociateResourceResponse' {} a -> s {httpStatus = a} :: AssociateResourceResponse)

instance Prelude.NFData AssociateResourceResponse where
  rnf AssociateResourceResponse' {..} =
    Prelude.rnf applicationArn
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf httpStatus
