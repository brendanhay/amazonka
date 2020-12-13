{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the tags associated with a CloudWatch resource. Currently, alarms and Contributor Insights rules support tagging.
module Network.AWS.CloudWatch.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    ltfrResourceARN,

    -- * Destructuring the response
    ListTagsForResourceResponse (..),
    mkListTagsForResourceResponse,

    -- ** Response lenses
    ltfrrsTags,
    ltfrrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTagsForResource' smart constructor.
newtype ListTagsForResource = ListTagsForResource'
  { -- | The ARN of the CloudWatch resource that you want to view tags for.
    --
    -- The ARN format of an alarm is @arn:aws:cloudwatch:/Region/ :/account-id/ :alarm:/alarm-name/ @
    -- The ARN format of a Contributor Insights rule is @arn:aws:cloudwatch:/Region/ :/account-id/ :insight-rule:/insight-rule-name/ @
    -- For more information about ARN format, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatch.html#amazoncloudwatch-resources-for-iam-policies Resource Types Defined by Amazon CloudWatch> in the /Amazon Web Services General Reference/ .
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN of the CloudWatch resource that you want to view tags for.
--
-- The ARN format of an alarm is @arn:aws:cloudwatch:/Region/ :/account-id/ :alarm:/alarm-name/ @
-- The ARN format of a Contributor Insights rule is @arn:aws:cloudwatch:/Region/ :/account-id/ :insight-rule:/insight-rule-name/ @
-- For more information about ARN format, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatch.html#amazoncloudwatch-resources-for-iam-policies Resource Types Defined by Amazon CloudWatch> in the /Amazon Web Services General Reference/ .
mkListTagsForResource ::
  -- | 'resourceARN'
  Lude.Text ->
  ListTagsForResource
mkListTagsForResource pResourceARN_ =
  ListTagsForResource' {resourceARN = pResourceARN_}

-- | The ARN of the CloudWatch resource that you want to view tags for.
--
-- The ARN format of an alarm is @arn:aws:cloudwatch:/Region/ :/account-id/ :alarm:/alarm-name/ @
-- The ARN format of a Contributor Insights rule is @arn:aws:cloudwatch:/Region/ :/account-id/ :insight-rule:/insight-rule-name/ @
-- For more information about ARN format, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatch.html#amazoncloudwatch-resources-for-iam-policies Resource Types Defined by Amazon CloudWatch> in the /Amazon Web Services General Reference/ .
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceARN :: Lens.Lens' ListTagsForResource Lude.Text
ltfrResourceARN = Lens.lens (resourceARN :: ListTagsForResource -> Lude.Text) (\s a -> s {resourceARN = a} :: ListTagsForResource)
{-# DEPRECATED ltfrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "ListTagsForResourceResult"
      ( \s h x ->
          ListTagsForResourceResponse'
            Lude.<$> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTagsForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListTagsForResource" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "ResourceARN" Lude.=: resourceARN
      ]

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | The list of tag keys and values associated with the resource you specified.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'tags' - The list of tag keys and values associated with the resource you specified.
-- * 'responseStatus' - The response status code.
mkListTagsForResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForResourceResponse
mkListTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
    { tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of tag keys and values associated with the resource you specified.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsTags :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe [Tag])
ltfrrsTags = Lens.lens (tags :: ListTagsForResourceResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Lude.Int
ltfrrsResponseStatus = Lens.lens (responseStatus :: ListTagsForResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
