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
-- Module      : Amazonka.WorkMail.CreateResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new WorkMail resource.
module Amazonka.WorkMail.CreateResource
  ( -- * Creating a Request
    CreateResource (..),
    newCreateResource,

    -- * Request Lenses
    createResource_organizationId,
    createResource_name,
    createResource_type,

    -- * Destructuring the Response
    CreateResourceResponse (..),
    newCreateResourceResponse,

    -- * Response Lenses
    createResourceResponse_resourceId,
    createResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newCreateResource' smart constructor.
data CreateResource = CreateResource'
  { -- | The identifier associated with the organization for which the resource
    -- is created.
    organizationId :: Prelude.Text,
    -- | The name of the new resource.
    name :: Prelude.Text,
    -- | The type of the new resource. The available types are @equipment@ and
    -- @room@.
    type' :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'createResource_organizationId' - The identifier associated with the organization for which the resource
-- is created.
--
-- 'name', 'createResource_name' - The name of the new resource.
--
-- 'type'', 'createResource_type' - The type of the new resource. The available types are @equipment@ and
-- @room@.
newCreateResource ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  ResourceType ->
  CreateResource
newCreateResource pOrganizationId_ pName_ pType_ =
  CreateResource'
    { organizationId = pOrganizationId_,
      name = pName_,
      type' = pType_
    }

-- | The identifier associated with the organization for which the resource
-- is created.
createResource_organizationId :: Lens.Lens' CreateResource Prelude.Text
createResource_organizationId = Lens.lens (\CreateResource' {organizationId} -> organizationId) (\s@CreateResource' {} a -> s {organizationId = a} :: CreateResource)

-- | The name of the new resource.
createResource_name :: Lens.Lens' CreateResource Prelude.Text
createResource_name = Lens.lens (\CreateResource' {name} -> name) (\s@CreateResource' {} a -> s {name = a} :: CreateResource)

-- | The type of the new resource. The available types are @equipment@ and
-- @room@.
createResource_type :: Lens.Lens' CreateResource ResourceType
createResource_type = Lens.lens (\CreateResource' {type'} -> type') (\s@CreateResource' {} a -> s {type' = a} :: CreateResource)

instance Core.AWSRequest CreateResource where
  type
    AWSResponse CreateResource =
      CreateResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceResponse'
            Prelude.<$> (x Data..?> "ResourceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateResource where
  hashWithSalt _salt CreateResource' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateResource where
  rnf CreateResource' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.CreateResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateResource where
  toJSON CreateResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath CreateResource where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResourceResponse' smart constructor.
data CreateResourceResponse = CreateResourceResponse'
  { -- | The identifier of the new resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'createResourceResponse_resourceId' - The identifier of the new resource.
--
-- 'httpStatus', 'createResourceResponse_httpStatus' - The response's http status code.
newCreateResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateResourceResponse
newCreateResourceResponse pHttpStatus_ =
  CreateResourceResponse'
    { resourceId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the new resource.
createResourceResponse_resourceId :: Lens.Lens' CreateResourceResponse (Prelude.Maybe Prelude.Text)
createResourceResponse_resourceId = Lens.lens (\CreateResourceResponse' {resourceId} -> resourceId) (\s@CreateResourceResponse' {} a -> s {resourceId = a} :: CreateResourceResponse)

-- | The response's http status code.
createResourceResponse_httpStatus :: Lens.Lens' CreateResourceResponse Prelude.Int
createResourceResponse_httpStatus = Lens.lens (\CreateResourceResponse' {httpStatus} -> httpStatus) (\s@CreateResourceResponse' {} a -> s {httpStatus = a} :: CreateResourceResponse)

instance Prelude.NFData CreateResourceResponse where
  rnf CreateResourceResponse' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpStatus
