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
-- Module      : Network.AWS.ElasticBeanstalk.UpdateTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the list of tags applied to an AWS Elastic Beanstalk resource.
-- Two lists can be passed: @TagsToAdd@ for tags to add or update, and
-- @TagsToRemove@.
--
-- Elastic Beanstalk supports tagging of all of its resources. For details
-- about resource tagging, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/applications-tagging-resources.html Tagging Application Resources>.
--
-- If you create a custom IAM user policy to control permission to this
-- operation, specify one of the following two virtual actions (or both)
-- instead of the API operation name:
--
-- [elasticbeanstalk:AddTags]
--     Controls permission to call @UpdateTagsForResource@ and pass a list
--     of tags to add in the @TagsToAdd@ parameter.
--
-- [elasticbeanstalk:RemoveTags]
--     Controls permission to call @UpdateTagsForResource@ and pass a list
--     of tag keys to remove in the @TagsToRemove@ parameter.
--
-- For details about creating a custom user policy, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/AWSHowTo.iam.managed-policies.html#AWSHowTo.iam.policies Creating a Custom User Policy>.
module Network.AWS.ElasticBeanstalk.UpdateTagsForResource
  ( -- * Creating a Request
    UpdateTagsForResource (..),
    newUpdateTagsForResource,

    -- * Request Lenses
    updateTagsForResource_tagsToRemove,
    updateTagsForResource_tagsToAdd,
    updateTagsForResource_resourceArn,

    -- * Destructuring the Response
    UpdateTagsForResourceResponse (..),
    newUpdateTagsForResourceResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTagsForResource' smart constructor.
data UpdateTagsForResource = UpdateTagsForResource'
  { -- | A list of tag keys to remove. If a tag key doesn\'t exist, it is
    -- silently ignored.
    --
    -- Specify at least one of these parameters: @TagsToAdd@, @TagsToRemove@.
    tagsToRemove :: Core.Maybe [Core.Text],
    -- | A list of tags to add or update. If a key of an existing tag is added,
    -- the tag\'s value is updated.
    --
    -- Specify at least one of these parameters: @TagsToAdd@, @TagsToRemove@.
    tagsToAdd :: Core.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the resouce to be updated.
    --
    -- Must be the ARN of an Elastic Beanstalk resource.
    resourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagsToRemove', 'updateTagsForResource_tagsToRemove' - A list of tag keys to remove. If a tag key doesn\'t exist, it is
-- silently ignored.
--
-- Specify at least one of these parameters: @TagsToAdd@, @TagsToRemove@.
--
-- 'tagsToAdd', 'updateTagsForResource_tagsToAdd' - A list of tags to add or update. If a key of an existing tag is added,
-- the tag\'s value is updated.
--
-- Specify at least one of these parameters: @TagsToAdd@, @TagsToRemove@.
--
-- 'resourceArn', 'updateTagsForResource_resourceArn' - The Amazon Resource Name (ARN) of the resouce to be updated.
--
-- Must be the ARN of an Elastic Beanstalk resource.
newUpdateTagsForResource ::
  -- | 'resourceArn'
  Core.Text ->
  UpdateTagsForResource
newUpdateTagsForResource pResourceArn_ =
  UpdateTagsForResource'
    { tagsToRemove = Core.Nothing,
      tagsToAdd = Core.Nothing,
      resourceArn = pResourceArn_
    }

-- | A list of tag keys to remove. If a tag key doesn\'t exist, it is
-- silently ignored.
--
-- Specify at least one of these parameters: @TagsToAdd@, @TagsToRemove@.
updateTagsForResource_tagsToRemove :: Lens.Lens' UpdateTagsForResource (Core.Maybe [Core.Text])
updateTagsForResource_tagsToRemove = Lens.lens (\UpdateTagsForResource' {tagsToRemove} -> tagsToRemove) (\s@UpdateTagsForResource' {} a -> s {tagsToRemove = a} :: UpdateTagsForResource) Core.. Lens.mapping Lens._Coerce

-- | A list of tags to add or update. If a key of an existing tag is added,
-- the tag\'s value is updated.
--
-- Specify at least one of these parameters: @TagsToAdd@, @TagsToRemove@.
updateTagsForResource_tagsToAdd :: Lens.Lens' UpdateTagsForResource (Core.Maybe [Tag])
updateTagsForResource_tagsToAdd = Lens.lens (\UpdateTagsForResource' {tagsToAdd} -> tagsToAdd) (\s@UpdateTagsForResource' {} a -> s {tagsToAdd = a} :: UpdateTagsForResource) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the resouce to be updated.
--
-- Must be the ARN of an Elastic Beanstalk resource.
updateTagsForResource_resourceArn :: Lens.Lens' UpdateTagsForResource Core.Text
updateTagsForResource_resourceArn = Lens.lens (\UpdateTagsForResource' {resourceArn} -> resourceArn) (\s@UpdateTagsForResource' {} a -> s {resourceArn = a} :: UpdateTagsForResource)

instance Core.AWSRequest UpdateTagsForResource where
  type
    AWSResponse UpdateTagsForResource =
      UpdateTagsForResourceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull UpdateTagsForResourceResponse'

instance Core.Hashable UpdateTagsForResource

instance Core.NFData UpdateTagsForResource

instance Core.ToHeaders UpdateTagsForResource where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateTagsForResource where
  toPath = Core.const "/"

instance Core.ToQuery UpdateTagsForResource where
  toQuery UpdateTagsForResource' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("UpdateTagsForResource" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "TagsToRemove"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> tagsToRemove),
        "TagsToAdd"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> tagsToAdd),
        "ResourceArn" Core.=: resourceArn
      ]

-- | /See:/ 'newUpdateTagsForResourceResponse' smart constructor.
data UpdateTagsForResourceResponse = UpdateTagsForResourceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateTagsForResourceResponse ::
  UpdateTagsForResourceResponse
newUpdateTagsForResourceResponse =
  UpdateTagsForResourceResponse'

instance Core.NFData UpdateTagsForResourceResponse
