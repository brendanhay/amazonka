{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.ListResourcesInProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resources that are included in the protection group.
module Network.AWS.Shield.ListResourcesInProtectionGroup
  ( -- * Creating a request
    ListResourcesInProtectionGroup (..),
    mkListResourcesInProtectionGroup,

    -- ** Request lenses
    lripgProtectionGroupId,
    lripgNextToken,
    lripgMaxResults,

    -- * Destructuring the response
    ListResourcesInProtectionGroupResponse (..),
    mkListResourcesInProtectionGroupResponse,

    -- ** Response lenses
    lripgrsResourceARNs,
    lripgrsNextToken,
    lripgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkListResourcesInProtectionGroup' smart constructor.
data ListResourcesInProtectionGroup = ListResourcesInProtectionGroup'
  { -- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
    protectionGroupId :: Lude.Text,
    -- | The next token value from a previous call to @ListResourcesInProtectionGroup@ . Pass null if this is the first call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of resource ARN objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
    --
    -- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourcesInProtectionGroup' with the minimum fields required to make a request.
--
-- * 'protectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
-- * 'nextToken' - The next token value from a previous call to @ListResourcesInProtectionGroup@ . Pass null if this is the first call.
-- * 'maxResults' - The maximum number of resource ARN objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
mkListResourcesInProtectionGroup ::
  -- | 'protectionGroupId'
  Lude.Text ->
  ListResourcesInProtectionGroup
mkListResourcesInProtectionGroup pProtectionGroupId_ =
  ListResourcesInProtectionGroup'
    { protectionGroupId =
        pProtectionGroupId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- /Note:/ Consider using 'protectionGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgProtectionGroupId :: Lens.Lens' ListResourcesInProtectionGroup Lude.Text
lripgProtectionGroupId = Lens.lens (protectionGroupId :: ListResourcesInProtectionGroup -> Lude.Text) (\s a -> s {protectionGroupId = a} :: ListResourcesInProtectionGroup)
{-# DEPRECATED lripgProtectionGroupId "Use generic-lens or generic-optics with 'protectionGroupId' instead." #-}

-- | The next token value from a previous call to @ListResourcesInProtectionGroup@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgNextToken :: Lens.Lens' ListResourcesInProtectionGroup (Lude.Maybe Lude.Text)
lripgNextToken = Lens.lens (nextToken :: ListResourcesInProtectionGroup -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourcesInProtectionGroup)
{-# DEPRECATED lripgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of resource ARN objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgMaxResults :: Lens.Lens' ListResourcesInProtectionGroup (Lude.Maybe Lude.Natural)
lripgMaxResults = Lens.lens (maxResults :: ListResourcesInProtectionGroup -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListResourcesInProtectionGroup)
{-# DEPRECATED lripgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListResourcesInProtectionGroup where
  type
    Rs ListResourcesInProtectionGroup =
      ListResourcesInProtectionGroupResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourcesInProtectionGroupResponse'
            Lude.<$> (x Lude..?> "ResourceArns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResourcesInProtectionGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShield_20160616.ListResourcesInProtectionGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListResourcesInProtectionGroup where
  toJSON ListResourcesInProtectionGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ProtectionGroupId" Lude..= protectionGroupId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListResourcesInProtectionGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ListResourcesInProtectionGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListResourcesInProtectionGroupResponse' smart constructor.
data ListResourcesInProtectionGroupResponse = ListResourcesInProtectionGroupResponse'
  { -- | The Amazon Resource Names (ARNs) of the resources that are included in the protection group.
    resourceARNs :: [Lude.Text],
    -- | If you specify a value for @MaxResults@ and you have more resources in the protection group than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourcesInProtectionGroupResponse' with the minimum fields required to make a request.
--
-- * 'resourceARNs' - The Amazon Resource Names (ARNs) of the resources that are included in the protection group.
-- * 'nextToken' - If you specify a value for @MaxResults@ and you have more resources in the protection group than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects.
-- * 'responseStatus' - The response status code.
mkListResourcesInProtectionGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourcesInProtectionGroupResponse
mkListResourcesInProtectionGroupResponse pResponseStatus_ =
  ListResourcesInProtectionGroupResponse'
    { resourceARNs =
        Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Names (ARNs) of the resources that are included in the protection group.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgrsResourceARNs :: Lens.Lens' ListResourcesInProtectionGroupResponse [Lude.Text]
lripgrsResourceARNs = Lens.lens (resourceARNs :: ListResourcesInProtectionGroupResponse -> [Lude.Text]) (\s a -> s {resourceARNs = a} :: ListResourcesInProtectionGroupResponse)
{-# DEPRECATED lripgrsResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

-- | If you specify a value for @MaxResults@ and you have more resources in the protection group than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgrsNextToken :: Lens.Lens' ListResourcesInProtectionGroupResponse (Lude.Maybe Lude.Text)
lripgrsNextToken = Lens.lens (nextToken :: ListResourcesInProtectionGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourcesInProtectionGroupResponse)
{-# DEPRECATED lripgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgrsResponseStatus :: Lens.Lens' ListResourcesInProtectionGroupResponse Lude.Int
lripgrsResponseStatus = Lens.lens (responseStatus :: ListResourcesInProtectionGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourcesInProtectionGroupResponse)
{-# DEPRECATED lripgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
