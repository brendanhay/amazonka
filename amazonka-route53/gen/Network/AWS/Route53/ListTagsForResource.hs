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
-- Module      : Network.AWS.Route53.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags for one health check or hosted zone.
--
-- For information about using tags for cost allocation, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
module Network.AWS.Route53.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_resourceType,
    listTagsForResource_resourceId,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_resourceTagSet,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A complex type containing information about a request for a list of the
-- tags that are associated with an individual resource.
--
-- /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | The type of the resource.
    --
    -- -   The resource type for health checks is @healthcheck@.
    --
    -- -   The resource type for hosted zones is @hostedzone@.
    resourceType :: TagResourceType,
    -- | The ID of the resource for which you want to retrieve tags.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'listTagsForResource_resourceType' - The type of the resource.
--
-- -   The resource type for health checks is @healthcheck@.
--
-- -   The resource type for hosted zones is @hostedzone@.
--
-- 'resourceId', 'listTagsForResource_resourceId' - The ID of the resource for which you want to retrieve tags.
newListTagsForResource ::
  -- | 'resourceType'
  TagResourceType ->
  -- | 'resourceId'
  Prelude.Text ->
  ListTagsForResource
newListTagsForResource pResourceType_ pResourceId_ =
  ListTagsForResource'
    { resourceType = pResourceType_,
      resourceId = pResourceId_
    }

-- | The type of the resource.
--
-- -   The resource type for health checks is @healthcheck@.
--
-- -   The resource type for hosted zones is @hostedzone@.
listTagsForResource_resourceType :: Lens.Lens' ListTagsForResource TagResourceType
listTagsForResource_resourceType = Lens.lens (\ListTagsForResource' {resourceType} -> resourceType) (\s@ListTagsForResource' {} a -> s {resourceType = a} :: ListTagsForResource)

-- | The ID of the resource for which you want to retrieve tags.
listTagsForResource_resourceId :: Lens.Lens' ListTagsForResource Prelude.Text
listTagsForResource_resourceId = Lens.lens (\ListTagsForResource' {resourceId} -> resourceId) (\s@ListTagsForResource' {} a -> s {resourceId = a} :: ListTagsForResource)

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListTagsForResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "ResourceTagSet")
      )

instance Prelude.Hashable ListTagsForResource

instance Prelude.NFData ListTagsForResource

instance Core.ToHeaders ListTagsForResource where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListTagsForResource where
  toPath ListTagsForResource' {..} =
    Prelude.mconcat
      [ "/2013-04-01/tags/",
        Core.toBS resourceType,
        "/",
        Core.toBS resourceId
      ]

instance Core.ToQuery ListTagsForResource where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains information about the health checks or
-- hosted zones for which you want to list tags.
--
-- /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A @ResourceTagSet@ containing tags associated with the specified
    -- resource.
    resourceTagSet :: ResourceTagSet
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
--
-- 'resourceTagSet', 'listTagsForResourceResponse_resourceTagSet' - A @ResourceTagSet@ containing tags associated with the specified
-- resource.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resourceTagSet'
  ResourceTagSet ->
  ListTagsForResourceResponse
newListTagsForResourceResponse
  pHttpStatus_
  pResourceTagSet_ =
    ListTagsForResourceResponse'
      { httpStatus =
          pHttpStatus_,
        resourceTagSet = pResourceTagSet_
      }

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Prelude.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

-- | A @ResourceTagSet@ containing tags associated with the specified
-- resource.
listTagsForResourceResponse_resourceTagSet :: Lens.Lens' ListTagsForResourceResponse ResourceTagSet
listTagsForResourceResponse_resourceTagSet = Lens.lens (\ListTagsForResourceResponse' {resourceTagSet} -> resourceTagSet) (\s@ListTagsForResourceResponse' {} a -> s {resourceTagSet = a} :: ListTagsForResourceResponse)

instance Prelude.NFData ListTagsForResourceResponse
