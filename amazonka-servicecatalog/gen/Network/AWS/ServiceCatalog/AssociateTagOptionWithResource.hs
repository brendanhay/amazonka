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
-- Module      : Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate the specified TagOption with the specified portfolio or
-- product.
module Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
  ( -- * Creating a Request
    AssociateTagOptionWithResource (..),
    newAssociateTagOptionWithResource,

    -- * Request Lenses
    associateTagOptionWithResource_resourceId,
    associateTagOptionWithResource_tagOptionId,

    -- * Destructuring the Response
    AssociateTagOptionWithResourceResponse (..),
    newAssociateTagOptionWithResourceResponse,

    -- * Response Lenses
    associateTagOptionWithResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newAssociateTagOptionWithResource' smart constructor.
data AssociateTagOptionWithResource = AssociateTagOptionWithResource'
  { -- | The resource identifier.
    resourceId :: Core.Text,
    -- | The TagOption identifier.
    tagOptionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateTagOptionWithResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'associateTagOptionWithResource_resourceId' - The resource identifier.
--
-- 'tagOptionId', 'associateTagOptionWithResource_tagOptionId' - The TagOption identifier.
newAssociateTagOptionWithResource ::
  -- | 'resourceId'
  Core.Text ->
  -- | 'tagOptionId'
  Core.Text ->
  AssociateTagOptionWithResource
newAssociateTagOptionWithResource
  pResourceId_
  pTagOptionId_ =
    AssociateTagOptionWithResource'
      { resourceId =
          pResourceId_,
        tagOptionId = pTagOptionId_
      }

-- | The resource identifier.
associateTagOptionWithResource_resourceId :: Lens.Lens' AssociateTagOptionWithResource Core.Text
associateTagOptionWithResource_resourceId = Lens.lens (\AssociateTagOptionWithResource' {resourceId} -> resourceId) (\s@AssociateTagOptionWithResource' {} a -> s {resourceId = a} :: AssociateTagOptionWithResource)

-- | The TagOption identifier.
associateTagOptionWithResource_tagOptionId :: Lens.Lens' AssociateTagOptionWithResource Core.Text
associateTagOptionWithResource_tagOptionId = Lens.lens (\AssociateTagOptionWithResource' {tagOptionId} -> tagOptionId) (\s@AssociateTagOptionWithResource' {} a -> s {tagOptionId = a} :: AssociateTagOptionWithResource)

instance
  Core.AWSRequest
    AssociateTagOptionWithResource
  where
  type
    AWSResponse AssociateTagOptionWithResource =
      AssociateTagOptionWithResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateTagOptionWithResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateTagOptionWithResource

instance Core.NFData AssociateTagOptionWithResource

instance
  Core.ToHeaders
    AssociateTagOptionWithResource
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.AssociateTagOptionWithResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateTagOptionWithResource where
  toJSON AssociateTagOptionWithResource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("TagOptionId" Core..= tagOptionId)
          ]
      )

instance Core.ToPath AssociateTagOptionWithResource where
  toPath = Core.const "/"

instance Core.ToQuery AssociateTagOptionWithResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateTagOptionWithResourceResponse' smart constructor.
data AssociateTagOptionWithResourceResponse = AssociateTagOptionWithResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateTagOptionWithResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateTagOptionWithResourceResponse_httpStatus' - The response's http status code.
newAssociateTagOptionWithResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateTagOptionWithResourceResponse
newAssociateTagOptionWithResourceResponse
  pHttpStatus_ =
    AssociateTagOptionWithResourceResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateTagOptionWithResourceResponse_httpStatus :: Lens.Lens' AssociateTagOptionWithResourceResponse Core.Int
associateTagOptionWithResourceResponse_httpStatus = Lens.lens (\AssociateTagOptionWithResourceResponse' {httpStatus} -> httpStatus) (\s@AssociateTagOptionWithResourceResponse' {} a -> s {httpStatus = a} :: AssociateTagOptionWithResourceResponse)

instance
  Core.NFData
    AssociateTagOptionWithResourceResponse
