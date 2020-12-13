{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from the specified resource.
module Network.AWS.CloudWatch.UntagResource
  ( -- * Creating a request
    UntagResource (..),
    mkUntagResource,

    -- ** Request lenses
    urTagKeys,
    urResourceARN,

    -- * Destructuring the response
    UntagResourceResponse (..),
    mkUntagResourceResponse,

    -- ** Response lenses
    urrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The list of tag keys to remove from the resource.
    tagKeys :: [Lude.Text],
    -- | The ARN of the CloudWatch resource that you're removing tags from.
    --
    -- The ARN format of an alarm is @arn:aws:cloudwatch:/Region/ :/account-id/ :alarm:/alarm-name/ @
    -- The ARN format of a Contributor Insights rule is @arn:aws:cloudwatch:/Region/ :/account-id/ :insight-rule:/insight-rule-name/ @
    -- For more information about ARN format, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatch.html#amazoncloudwatch-resources-for-iam-policies Resource Types Defined by Amazon CloudWatch> in the /Amazon Web Services General Reference/ .
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- * 'tagKeys' - The list of tag keys to remove from the resource.
-- * 'resourceARN' - The ARN of the CloudWatch resource that you're removing tags from.
--
-- The ARN format of an alarm is @arn:aws:cloudwatch:/Region/ :/account-id/ :alarm:/alarm-name/ @
-- The ARN format of a Contributor Insights rule is @arn:aws:cloudwatch:/Region/ :/account-id/ :insight-rule:/insight-rule-name/ @
-- For more information about ARN format, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatch.html#amazoncloudwatch-resources-for-iam-policies Resource Types Defined by Amazon CloudWatch> in the /Amazon Web Services General Reference/ .
mkUntagResource ::
  -- | 'resourceARN'
  Lude.Text ->
  UntagResource
mkUntagResource pResourceARN_ =
  UntagResource'
    { tagKeys = Lude.mempty,
      resourceARN = pResourceARN_
    }

-- | The list of tag keys to remove from the resource.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource [Lude.Text]
urTagKeys = Lens.lens (tagKeys :: UntagResource -> [Lude.Text]) (\s a -> s {tagKeys = a} :: UntagResource)
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The ARN of the CloudWatch resource that you're removing tags from.
--
-- The ARN format of an alarm is @arn:aws:cloudwatch:/Region/ :/account-id/ :alarm:/alarm-name/ @
-- The ARN format of a Contributor Insights rule is @arn:aws:cloudwatch:/Region/ :/account-id/ :insight-rule:/insight-rule-name/ @
-- For more information about ARN format, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatch.html#amazoncloudwatch-resources-for-iam-policies Resource Types Defined by Amazon CloudWatch> in the /Amazon Web Services General Reference/ .
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceARN :: Lens.Lens' UntagResource Lude.Text
urResourceARN = Lens.lens (resourceARN :: UntagResource -> Lude.Text) (\s a -> s {resourceARN = a} :: UntagResource)
{-# DEPRECATED urResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "UntagResourceResult"
      ( \s h x ->
          UntagResourceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UntagResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UntagResource where
  toPath = Lude.const "/"

instance Lude.ToQuery UntagResource where
  toQuery UntagResource' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UntagResource" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "TagKeys" Lude.=: Lude.toQueryList "member" tagKeys,
        "ResourceARN" Lude.=: resourceARN
      ]

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
newtype UntagResourceResponse = UntagResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUntagResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UntagResourceResponse
mkUntagResourceResponse pResponseStatus_ =
  UntagResourceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UntagResourceResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UntagResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UntagResourceResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
