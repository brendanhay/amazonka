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
-- Module      : Network.AWS.Route53.ChangeTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds, edits, or deletes tags for a health check or a hosted zone.
--
-- For information about using tags for cost allocation, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
module Network.AWS.Route53.ChangeTagsForResource
  ( -- * Creating a Request
    ChangeTagsForResource (..),
    newChangeTagsForResource,

    -- * Request Lenses
    changeTagsForResource_addTags,
    changeTagsForResource_removeTagKeys,
    changeTagsForResource_resourceType,
    changeTagsForResource_resourceId,

    -- * Destructuring the Response
    ChangeTagsForResourceResponse (..),
    newChangeTagsForResourceResponse,

    -- * Response Lenses
    changeTagsForResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A complex type that contains information about the tags that you want to
-- add, edit, or delete.
--
-- /See:/ 'newChangeTagsForResource' smart constructor.
data ChangeTagsForResource = ChangeTagsForResource'
  { -- | A complex type that contains a list of the tags that you want to add to
    -- the specified health check or hosted zone and\/or the tags that you want
    -- to edit @Value@ for.
    --
    -- You can add a maximum of 10 tags to a health check or a hosted zone.
    addTags :: Core.Maybe (Core.NonEmpty Tag),
    -- | A complex type that contains a list of the tags that you want to delete
    -- from the specified health check or hosted zone. You can specify up to 10
    -- keys.
    removeTagKeys :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The type of the resource.
    --
    -- -   The resource type for health checks is @healthcheck@.
    --
    -- -   The resource type for hosted zones is @hostedzone@.
    resourceType :: TagResourceType,
    -- | The ID of the resource for which you want to add, change, or delete
    -- tags.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ChangeTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addTags', 'changeTagsForResource_addTags' - A complex type that contains a list of the tags that you want to add to
-- the specified health check or hosted zone and\/or the tags that you want
-- to edit @Value@ for.
--
-- You can add a maximum of 10 tags to a health check or a hosted zone.
--
-- 'removeTagKeys', 'changeTagsForResource_removeTagKeys' - A complex type that contains a list of the tags that you want to delete
-- from the specified health check or hosted zone. You can specify up to 10
-- keys.
--
-- 'resourceType', 'changeTagsForResource_resourceType' - The type of the resource.
--
-- -   The resource type for health checks is @healthcheck@.
--
-- -   The resource type for hosted zones is @hostedzone@.
--
-- 'resourceId', 'changeTagsForResource_resourceId' - The ID of the resource for which you want to add, change, or delete
-- tags.
newChangeTagsForResource ::
  -- | 'resourceType'
  TagResourceType ->
  -- | 'resourceId'
  Core.Text ->
  ChangeTagsForResource
newChangeTagsForResource pResourceType_ pResourceId_ =
  ChangeTagsForResource'
    { addTags = Core.Nothing,
      removeTagKeys = Core.Nothing,
      resourceType = pResourceType_,
      resourceId = pResourceId_
    }

-- | A complex type that contains a list of the tags that you want to add to
-- the specified health check or hosted zone and\/or the tags that you want
-- to edit @Value@ for.
--
-- You can add a maximum of 10 tags to a health check or a hosted zone.
changeTagsForResource_addTags :: Lens.Lens' ChangeTagsForResource (Core.Maybe (Core.NonEmpty Tag))
changeTagsForResource_addTags = Lens.lens (\ChangeTagsForResource' {addTags} -> addTags) (\s@ChangeTagsForResource' {} a -> s {addTags = a} :: ChangeTagsForResource) Core.. Lens.mapping Lens._Coerce

-- | A complex type that contains a list of the tags that you want to delete
-- from the specified health check or hosted zone. You can specify up to 10
-- keys.
changeTagsForResource_removeTagKeys :: Lens.Lens' ChangeTagsForResource (Core.Maybe (Core.NonEmpty Core.Text))
changeTagsForResource_removeTagKeys = Lens.lens (\ChangeTagsForResource' {removeTagKeys} -> removeTagKeys) (\s@ChangeTagsForResource' {} a -> s {removeTagKeys = a} :: ChangeTagsForResource) Core.. Lens.mapping Lens._Coerce

-- | The type of the resource.
--
-- -   The resource type for health checks is @healthcheck@.
--
-- -   The resource type for hosted zones is @hostedzone@.
changeTagsForResource_resourceType :: Lens.Lens' ChangeTagsForResource TagResourceType
changeTagsForResource_resourceType = Lens.lens (\ChangeTagsForResource' {resourceType} -> resourceType) (\s@ChangeTagsForResource' {} a -> s {resourceType = a} :: ChangeTagsForResource)

-- | The ID of the resource for which you want to add, change, or delete
-- tags.
changeTagsForResource_resourceId :: Lens.Lens' ChangeTagsForResource Core.Text
changeTagsForResource_resourceId = Lens.lens (\ChangeTagsForResource' {resourceId} -> resourceId) (\s@ChangeTagsForResource' {} a -> s {resourceId = a} :: ChangeTagsForResource)

instance Core.AWSRequest ChangeTagsForResource where
  type
    AWSResponse ChangeTagsForResource =
      ChangeTagsForResourceResponse
  request = Request.postXML defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ChangeTagsForResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ChangeTagsForResource

instance Core.NFData ChangeTagsForResource

instance Core.ToElement ChangeTagsForResource where
  toElement =
    Core.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}ChangeTagsForResourceRequest"

instance Core.ToHeaders ChangeTagsForResource where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ChangeTagsForResource where
  toPath ChangeTagsForResource' {..} =
    Core.mconcat
      [ "/2013-04-01/tags/",
        Core.toBS resourceType,
        "/",
        Core.toBS resourceId
      ]

instance Core.ToQuery ChangeTagsForResource where
  toQuery = Core.const Core.mempty

instance Core.ToXML ChangeTagsForResource where
  toXML ChangeTagsForResource' {..} =
    Core.mconcat
      [ "AddTags"
          Core.@= Core.toXML (Core.toXMLList "Tag" Core.<$> addTags),
        "RemoveTagKeys"
          Core.@= Core.toXML
            (Core.toXMLList "Key" Core.<$> removeTagKeys)
      ]

-- | Empty response for the request.
--
-- /See:/ 'newChangeTagsForResourceResponse' smart constructor.
data ChangeTagsForResourceResponse = ChangeTagsForResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ChangeTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'changeTagsForResourceResponse_httpStatus' - The response's http status code.
newChangeTagsForResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ChangeTagsForResourceResponse
newChangeTagsForResourceResponse pHttpStatus_ =
  ChangeTagsForResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
changeTagsForResourceResponse_httpStatus :: Lens.Lens' ChangeTagsForResourceResponse Core.Int
changeTagsForResourceResponse_httpStatus = Lens.lens (\ChangeTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ChangeTagsForResourceResponse' {} a -> s {httpStatus = a} :: ChangeTagsForResourceResponse)

instance Core.NFData ChangeTagsForResourceResponse
