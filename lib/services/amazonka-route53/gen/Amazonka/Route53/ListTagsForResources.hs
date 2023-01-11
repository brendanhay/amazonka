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
-- Module      : Amazonka.Route53.ListTagsForResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags for up to 10 health checks or hosted zones.
--
-- For information about using tags for cost allocation, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
-- in the /Billing and Cost Management User Guide/.
module Amazonka.Route53.ListTagsForResources
  ( -- * Creating a Request
    ListTagsForResources (..),
    newListTagsForResources,

    -- * Request Lenses
    listTagsForResources_resourceType,
    listTagsForResources_resourceIds,

    -- * Destructuring the Response
    ListTagsForResourcesResponse (..),
    newListTagsForResourcesResponse,

    -- * Response Lenses
    listTagsForResourcesResponse_httpStatus,
    listTagsForResourcesResponse_resourceTagSets,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains information about the health checks or
-- hosted zones for which you want to list tags.
--
-- /See:/ 'newListTagsForResources' smart constructor.
data ListTagsForResources = ListTagsForResources'
  { -- | The type of the resources.
    --
    -- -   The resource type for health checks is @healthcheck@.
    --
    -- -   The resource type for hosted zones is @hostedzone@.
    resourceType :: TagResourceType,
    -- | A complex type that contains the ResourceId element for each resource
    -- for which you want to get a list of tags.
    resourceIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'listTagsForResources_resourceType' - The type of the resources.
--
-- -   The resource type for health checks is @healthcheck@.
--
-- -   The resource type for hosted zones is @hostedzone@.
--
-- 'resourceIds', 'listTagsForResources_resourceIds' - A complex type that contains the ResourceId element for each resource
-- for which you want to get a list of tags.
newListTagsForResources ::
  -- | 'resourceType'
  TagResourceType ->
  -- | 'resourceIds'
  Prelude.NonEmpty Prelude.Text ->
  ListTagsForResources
newListTagsForResources pResourceType_ pResourceIds_ =
  ListTagsForResources'
    { resourceType =
        pResourceType_,
      resourceIds = Lens.coerced Lens.# pResourceIds_
    }

-- | The type of the resources.
--
-- -   The resource type for health checks is @healthcheck@.
--
-- -   The resource type for hosted zones is @hostedzone@.
listTagsForResources_resourceType :: Lens.Lens' ListTagsForResources TagResourceType
listTagsForResources_resourceType = Lens.lens (\ListTagsForResources' {resourceType} -> resourceType) (\s@ListTagsForResources' {} a -> s {resourceType = a} :: ListTagsForResources)

-- | A complex type that contains the ResourceId element for each resource
-- for which you want to get a list of tags.
listTagsForResources_resourceIds :: Lens.Lens' ListTagsForResources (Prelude.NonEmpty Prelude.Text)
listTagsForResources_resourceIds = Lens.lens (\ListTagsForResources' {resourceIds} -> resourceIds) (\s@ListTagsForResources' {} a -> s {resourceIds = a} :: ListTagsForResources) Prelude.. Lens.coerced

instance Core.AWSRequest ListTagsForResources where
  type
    AWSResponse ListTagsForResources =
      ListTagsForResourcesResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListTagsForResourcesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "ResourceTagSets" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "ResourceTagSet"
                        )
      )

instance Prelude.Hashable ListTagsForResources where
  hashWithSalt _salt ListTagsForResources' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceIds

instance Prelude.NFData ListTagsForResources where
  rnf ListTagsForResources' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceIds

instance Data.ToElement ListTagsForResources where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}ListTagsForResourcesRequest"

instance Data.ToHeaders ListTagsForResources where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListTagsForResources where
  toPath ListTagsForResources' {..} =
    Prelude.mconcat
      ["/2013-04-01/tags/", Data.toBS resourceType]

instance Data.ToQuery ListTagsForResources where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML ListTagsForResources where
  toXML ListTagsForResources' {..} =
    Prelude.mconcat
      [ "ResourceIds"
          Data.@= Data.toXMLList "ResourceId" resourceIds
      ]

-- | A complex type containing tags for the specified resources.
--
-- /See:/ 'newListTagsForResourcesResponse' smart constructor.
data ListTagsForResourcesResponse = ListTagsForResourcesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of @ResourceTagSet@s containing tags associated with the
    -- specified resources.
    resourceTagSets :: [ResourceTagSet]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listTagsForResourcesResponse_httpStatus' - The response's http status code.
--
-- 'resourceTagSets', 'listTagsForResourcesResponse_resourceTagSets' - A list of @ResourceTagSet@s containing tags associated with the
-- specified resources.
newListTagsForResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForResourcesResponse
newListTagsForResourcesResponse pHttpStatus_ =
  ListTagsForResourcesResponse'
    { httpStatus =
        pHttpStatus_,
      resourceTagSets = Prelude.mempty
    }

-- | The response's http status code.
listTagsForResourcesResponse_httpStatus :: Lens.Lens' ListTagsForResourcesResponse Prelude.Int
listTagsForResourcesResponse_httpStatus = Lens.lens (\ListTagsForResourcesResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourcesResponse' {} a -> s {httpStatus = a} :: ListTagsForResourcesResponse)

-- | A list of @ResourceTagSet@s containing tags associated with the
-- specified resources.
listTagsForResourcesResponse_resourceTagSets :: Lens.Lens' ListTagsForResourcesResponse [ResourceTagSet]
listTagsForResourcesResponse_resourceTagSets = Lens.lens (\ListTagsForResourcesResponse' {resourceTagSets} -> resourceTagSets) (\s@ListTagsForResourcesResponse' {} a -> s {resourceTagSets = a} :: ListTagsForResourcesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListTagsForResourcesResponse where
  rnf ListTagsForResourcesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceTagSets
