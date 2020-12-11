{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the list of tags applied to an AWS Elastic Beanstalk resource. Two lists can be passed: @TagsToAdd@ for tags to add or update, and @TagsToRemove@ .
--
-- Elastic Beanstalk supports tagging of all of its resources. For details about resource tagging, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/applications-tagging-resources.html Tagging Application Resources> .
-- If you create a custom IAM user policy to control permission to this operation, specify one of the following two virtual actions (or both) instead of the API operation name:
--
--     * elasticbeanstalk:AddTags
--
--     * Controls permission to call @UpdateTagsForResource@ and pass a list of tags to add in the @TagsToAdd@ parameter.
--
--
--     * elasticbeanstalk:RemoveTags
--
--     * Controls permission to call @UpdateTagsForResource@ and pass a list of tag keys to remove in the @TagsToRemove@ parameter.
--
--
-- For details about creating a custom user policy, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/AWSHowTo.iam.managed-policies.html#AWSHowTo.iam.policies Creating a Custom User Policy> .
module Network.AWS.ElasticBeanstalk.UpdateTagsForResource
  ( -- * Creating a request
    UpdateTagsForResource (..),
    mkUpdateTagsForResource,

    -- ** Request lenses
    utfrTagsToRemove,
    utfrTagsToAdd,
    utfrResourceARN,

    -- * Destructuring the response
    UpdateTagsForResourceResponse (..),
    mkUpdateTagsForResourceResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateTagsForResource' smart constructor.
data UpdateTagsForResource = UpdateTagsForResource'
  { tagsToRemove ::
      Lude.Maybe [Lude.Text],
    tagsToAdd :: Lude.Maybe [Tag],
    resourceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTagsForResource' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resouce to be updated.
--
-- Must be the ARN of an Elastic Beanstalk resource.
-- * 'tagsToAdd' - A list of tags to add or update. If a key of an existing tag is added, the tag's value is updated.
--
-- Specify at least one of these parameters: @TagsToAdd@ , @TagsToRemove@ .
-- * 'tagsToRemove' - A list of tag keys to remove. If a tag key doesn't exist, it is silently ignored.
--
-- Specify at least one of these parameters: @TagsToAdd@ , @TagsToRemove@ .
mkUpdateTagsForResource ::
  -- | 'resourceARN'
  Lude.Text ->
  UpdateTagsForResource
mkUpdateTagsForResource pResourceARN_ =
  UpdateTagsForResource'
    { tagsToRemove = Lude.Nothing,
      tagsToAdd = Lude.Nothing,
      resourceARN = pResourceARN_
    }

-- | A list of tag keys to remove. If a tag key doesn't exist, it is silently ignored.
--
-- Specify at least one of these parameters: @TagsToAdd@ , @TagsToRemove@ .
--
-- /Note:/ Consider using 'tagsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utfrTagsToRemove :: Lens.Lens' UpdateTagsForResource (Lude.Maybe [Lude.Text])
utfrTagsToRemove = Lens.lens (tagsToRemove :: UpdateTagsForResource -> Lude.Maybe [Lude.Text]) (\s a -> s {tagsToRemove = a} :: UpdateTagsForResource)
{-# DEPRECATED utfrTagsToRemove "Use generic-lens or generic-optics with 'tagsToRemove' instead." #-}

-- | A list of tags to add or update. If a key of an existing tag is added, the tag's value is updated.
--
-- Specify at least one of these parameters: @TagsToAdd@ , @TagsToRemove@ .
--
-- /Note:/ Consider using 'tagsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utfrTagsToAdd :: Lens.Lens' UpdateTagsForResource (Lude.Maybe [Tag])
utfrTagsToAdd = Lens.lens (tagsToAdd :: UpdateTagsForResource -> Lude.Maybe [Tag]) (\s a -> s {tagsToAdd = a} :: UpdateTagsForResource)
{-# DEPRECATED utfrTagsToAdd "Use generic-lens or generic-optics with 'tagsToAdd' instead." #-}

-- | The Amazon Resource Name (ARN) of the resouce to be updated.
--
-- Must be the ARN of an Elastic Beanstalk resource.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utfrResourceARN :: Lens.Lens' UpdateTagsForResource Lude.Text
utfrResourceARN = Lens.lens (resourceARN :: UpdateTagsForResource -> Lude.Text) (\s a -> s {resourceARN = a} :: UpdateTagsForResource)
{-# DEPRECATED utfrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest UpdateTagsForResource where
  type Rs UpdateTagsForResource = UpdateTagsForResourceResponse
  request = Req.postQuery elasticBeanstalkService
  response = Res.receiveNull UpdateTagsForResourceResponse'

instance Lude.ToHeaders UpdateTagsForResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateTagsForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTagsForResource where
  toQuery UpdateTagsForResource' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateTagsForResource" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TagsToRemove"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tagsToRemove),
        "TagsToAdd"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tagsToAdd),
        "ResourceArn" Lude.=: resourceARN
      ]

-- | /See:/ 'mkUpdateTagsForResourceResponse' smart constructor.
data UpdateTagsForResourceResponse = UpdateTagsForResourceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTagsForResourceResponse' with the minimum fields required to make a request.
mkUpdateTagsForResourceResponse ::
  UpdateTagsForResourceResponse
mkUpdateTagsForResourceResponse = UpdateTagsForResourceResponse'
