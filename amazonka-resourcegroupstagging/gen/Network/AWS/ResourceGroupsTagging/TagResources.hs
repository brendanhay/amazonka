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
-- Module      : Network.AWS.ResourceGroupsTagging.TagResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies one or more tags to the specified resources. Note the following:
--
-- -   Not all resources can have tags. For a list of services with
--     resources that support tagging using this operation, see
--     <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/supported-services.html Services that support the Resource Groups Tagging API>.
--
-- -   Each resource can have up to 50 tags. For other limits, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html#tag-conventions Tag Naming and Usage Conventions>
--     in the /AWS General Reference./
--
-- -   You can only tag resources that are located in the specified AWS
--     Region for the AWS account.
--
-- -   To add tags to a resource, you need the necessary permissions for
--     the service that the resource belongs to as well as permissions for
--     adding tags. For more information, see the documentation for each
--     service.
--
-- Do not store personally identifiable information (PII) or other
-- confidential or sensitive information in tags. We use tags to provide
-- you with billing and administration services. Tags are not intended to
-- be used for private or sensitive data.
module Network.AWS.ResourceGroupsTagging.TagResources
  ( -- * Creating a Request
    TagResources (..),
    newTagResources,

    -- * Request Lenses
    tagResources_resourceARNList,
    tagResources_tags,

    -- * Destructuring the Response
    TagResourcesResponse (..),
    newTagResourcesResponse,

    -- * Response Lenses
    tagResourcesResponse_failedResourcesMap,
    tagResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagResources' smart constructor.
data TagResources = TagResources'
  { -- | Specifies the list of ARNs of the resources that you want to apply tags
    -- to.
    --
    -- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
    -- information, see
    -- <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    resourceARNList :: Core.NonEmpty Core.Text,
    -- | Specifies a list of tags that you want to add to the specified
    -- resources. A tag consists of a key and a value that you define.
    tags :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARNList', 'tagResources_resourceARNList' - Specifies the list of ARNs of the resources that you want to apply tags
-- to.
--
-- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
-- information, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'tags', 'tagResources_tags' - Specifies a list of tags that you want to add to the specified
-- resources. A tag consists of a key and a value that you define.
newTagResources ::
  -- | 'resourceARNList'
  Core.NonEmpty Core.Text ->
  TagResources
newTagResources pResourceARNList_ =
  TagResources'
    { resourceARNList =
        Lens._Coerce Lens.# pResourceARNList_,
      tags = Core.mempty
    }

-- | Specifies the list of ARNs of the resources that you want to apply tags
-- to.
--
-- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
-- information, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
tagResources_resourceARNList :: Lens.Lens' TagResources (Core.NonEmpty Core.Text)
tagResources_resourceARNList = Lens.lens (\TagResources' {resourceARNList} -> resourceARNList) (\s@TagResources' {} a -> s {resourceARNList = a} :: TagResources) Core.. Lens._Coerce

-- | Specifies a list of tags that you want to add to the specified
-- resources. A tag consists of a key and a value that you define.
tagResources_tags :: Lens.Lens' TagResources (Core.HashMap Core.Text Core.Text)
tagResources_tags = Lens.lens (\TagResources' {tags} -> tags) (\s@TagResources' {} a -> s {tags = a} :: TagResources) Core.. Lens._Coerce

instance Core.AWSRequest TagResources where
  type AWSResponse TagResources = TagResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TagResourcesResponse'
            Core.<$> ( x Core..?> "FailedResourcesMap"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable TagResources

instance Core.NFData TagResources

instance Core.ToHeaders TagResources where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ResourceGroupsTaggingAPI_20170126.TagResources" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TagResources where
  toJSON TagResources' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ResourceARNList" Core..= resourceARNList),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath TagResources where
  toPath = Core.const "/"

instance Core.ToQuery TagResources where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newTagResourcesResponse' smart constructor.
data TagResourcesResponse = TagResourcesResponse'
  { -- | A map containing a key-value pair for each failed item that couldn\'t be
    -- tagged. The key is the ARN of the failed resource. The value is a
    -- @FailureInfo@ object that contains an error code, a status code, and an
    -- error message. If there are no errors, the @FailedResourcesMap@ is
    -- empty.
    failedResourcesMap :: Core.Maybe (Core.HashMap Core.Text FailureInfo),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedResourcesMap', 'tagResourcesResponse_failedResourcesMap' - A map containing a key-value pair for each failed item that couldn\'t be
-- tagged. The key is the ARN of the failed resource. The value is a
-- @FailureInfo@ object that contains an error code, a status code, and an
-- error message. If there are no errors, the @FailedResourcesMap@ is
-- empty.
--
-- 'httpStatus', 'tagResourcesResponse_httpStatus' - The response's http status code.
newTagResourcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TagResourcesResponse
newTagResourcesResponse pHttpStatus_ =
  TagResourcesResponse'
    { failedResourcesMap =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map containing a key-value pair for each failed item that couldn\'t be
-- tagged. The key is the ARN of the failed resource. The value is a
-- @FailureInfo@ object that contains an error code, a status code, and an
-- error message. If there are no errors, the @FailedResourcesMap@ is
-- empty.
tagResourcesResponse_failedResourcesMap :: Lens.Lens' TagResourcesResponse (Core.Maybe (Core.HashMap Core.Text FailureInfo))
tagResourcesResponse_failedResourcesMap = Lens.lens (\TagResourcesResponse' {failedResourcesMap} -> failedResourcesMap) (\s@TagResourcesResponse' {} a -> s {failedResourcesMap = a} :: TagResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
tagResourcesResponse_httpStatus :: Lens.Lens' TagResourcesResponse Core.Int
tagResourcesResponse_httpStatus = Lens.lens (\TagResourcesResponse' {httpStatus} -> httpStatus) (\s@TagResourcesResponse' {} a -> s {httpStatus = a} :: TagResourcesResponse)

instance Core.NFData TagResourcesResponse
