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
-- Module      : Amazonka.Route53RecoveryReadiness.GetResourceSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a Resource Set.
module Amazonka.Route53RecoveryReadiness.GetResourceSet
  ( -- * Creating a Request
    GetResourceSet (..),
    newGetResourceSet,

    -- * Request Lenses
    getResourceSet_resourceSetName,

    -- * Destructuring the Response
    GetResourceSetResponse (..),
    newGetResourceSetResponse,

    -- * Response Lenses
    getResourceSetResponse_resourceSetName,
    getResourceSetResponse_resourceSetType,
    getResourceSetResponse_resources,
    getResourceSetResponse_resourceSetArn,
    getResourceSetResponse_tags,
    getResourceSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetResourceSet' smart constructor.
data GetResourceSet = GetResourceSet'
  { -- | The ResourceSet to get
    resourceSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSetName', 'getResourceSet_resourceSetName' - The ResourceSet to get
newGetResourceSet ::
  -- | 'resourceSetName'
  Prelude.Text ->
  GetResourceSet
newGetResourceSet pResourceSetName_ =
  GetResourceSet'
    { resourceSetName =
        pResourceSetName_
    }

-- | The ResourceSet to get
getResourceSet_resourceSetName :: Lens.Lens' GetResourceSet Prelude.Text
getResourceSet_resourceSetName = Lens.lens (\GetResourceSet' {resourceSetName} -> resourceSetName) (\s@GetResourceSet' {} a -> s {resourceSetName = a} :: GetResourceSet)

instance Core.AWSRequest GetResourceSet where
  type
    AWSResponse GetResourceSet =
      GetResourceSetResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceSetResponse'
            Prelude.<$> (x Core..?> "resourceSetName")
            Prelude.<*> (x Core..?> "resourceSetType")
            Prelude.<*> (x Core..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "resourceSetArn")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceSet

instance Prelude.NFData GetResourceSet

instance Core.ToHeaders GetResourceSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetResourceSet where
  toPath GetResourceSet' {..} =
    Prelude.mconcat
      ["/resourcesets/", Core.toBS resourceSetName]

instance Core.ToQuery GetResourceSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceSetResponse' smart constructor.
data GetResourceSetResponse = GetResourceSetResponse'
  { -- | The name of the ResourceSet
    resourceSetName :: Prelude.Maybe Prelude.Text,
    -- | AWS Resource Type of the resources in the ResourceSet
    resourceSetType :: Prelude.Maybe Prelude.Text,
    -- | A list of Resource objects
    resources :: Prelude.Maybe [Resource],
    -- | The arn for the ResourceSet
    resourceSetArn :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSetName', 'getResourceSetResponse_resourceSetName' - The name of the ResourceSet
--
-- 'resourceSetType', 'getResourceSetResponse_resourceSetType' - AWS Resource Type of the resources in the ResourceSet
--
-- 'resources', 'getResourceSetResponse_resources' - A list of Resource objects
--
-- 'resourceSetArn', 'getResourceSetResponse_resourceSetArn' - The arn for the ResourceSet
--
-- 'tags', 'getResourceSetResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'getResourceSetResponse_httpStatus' - The response's http status code.
newGetResourceSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceSetResponse
newGetResourceSetResponse pHttpStatus_ =
  GetResourceSetResponse'
    { resourceSetName =
        Prelude.Nothing,
      resourceSetType = Prelude.Nothing,
      resources = Prelude.Nothing,
      resourceSetArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the ResourceSet
getResourceSetResponse_resourceSetName :: Lens.Lens' GetResourceSetResponse (Prelude.Maybe Prelude.Text)
getResourceSetResponse_resourceSetName = Lens.lens (\GetResourceSetResponse' {resourceSetName} -> resourceSetName) (\s@GetResourceSetResponse' {} a -> s {resourceSetName = a} :: GetResourceSetResponse)

-- | AWS Resource Type of the resources in the ResourceSet
getResourceSetResponse_resourceSetType :: Lens.Lens' GetResourceSetResponse (Prelude.Maybe Prelude.Text)
getResourceSetResponse_resourceSetType = Lens.lens (\GetResourceSetResponse' {resourceSetType} -> resourceSetType) (\s@GetResourceSetResponse' {} a -> s {resourceSetType = a} :: GetResourceSetResponse)

-- | A list of Resource objects
getResourceSetResponse_resources :: Lens.Lens' GetResourceSetResponse (Prelude.Maybe [Resource])
getResourceSetResponse_resources = Lens.lens (\GetResourceSetResponse' {resources} -> resources) (\s@GetResourceSetResponse' {} a -> s {resources = a} :: GetResourceSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The arn for the ResourceSet
getResourceSetResponse_resourceSetArn :: Lens.Lens' GetResourceSetResponse (Prelude.Maybe Prelude.Text)
getResourceSetResponse_resourceSetArn = Lens.lens (\GetResourceSetResponse' {resourceSetArn} -> resourceSetArn) (\s@GetResourceSetResponse' {} a -> s {resourceSetArn = a} :: GetResourceSetResponse)

-- | Undocumented member.
getResourceSetResponse_tags :: Lens.Lens' GetResourceSetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getResourceSetResponse_tags = Lens.lens (\GetResourceSetResponse' {tags} -> tags) (\s@GetResourceSetResponse' {} a -> s {tags = a} :: GetResourceSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getResourceSetResponse_httpStatus :: Lens.Lens' GetResourceSetResponse Prelude.Int
getResourceSetResponse_httpStatus = Lens.lens (\GetResourceSetResponse' {httpStatus} -> httpStatus) (\s@GetResourceSetResponse' {} a -> s {httpStatus = a} :: GetResourceSetResponse)

instance Prelude.NFData GetResourceSetResponse
