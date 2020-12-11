{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    lripgNextToken,
    lripgMaxResults,
    lripgProtectionGroupId,

    -- * Destructuring the response
    ListResourcesInProtectionGroupResponse (..),
    mkListResourcesInProtectionGroupResponse,

    -- ** Response lenses
    lripgrsNextToken,
    lripgrsResponseStatus,
    lripgrsResourceARNs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkListResourcesInProtectionGroup' smart constructor.
data ListResourcesInProtectionGroup = ListResourcesInProtectionGroup'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    protectionGroupId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourcesInProtectionGroup' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of resource ARN objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
-- * 'nextToken' - The next token value from a previous call to @ListResourcesInProtectionGroup@ . Pass null if this is the first call.
-- * 'protectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
mkListResourcesInProtectionGroup ::
  -- | 'protectionGroupId'
  Lude.Text ->
  ListResourcesInProtectionGroup
mkListResourcesInProtectionGroup pProtectionGroupId_ =
  ListResourcesInProtectionGroup'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      protectionGroupId = pProtectionGroupId_
    }

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

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- /Note:/ Consider using 'protectionGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgProtectionGroupId :: Lens.Lens' ListResourcesInProtectionGroup Lude.Text
lripgProtectionGroupId = Lens.lens (protectionGroupId :: ListResourcesInProtectionGroup -> Lude.Text) (\s a -> s {protectionGroupId = a} :: ListResourcesInProtectionGroup)
{-# DEPRECATED lripgProtectionGroupId "Use generic-lens or generic-optics with 'protectionGroupId' instead." #-}

instance Lude.AWSRequest ListResourcesInProtectionGroup where
  type
    Rs ListResourcesInProtectionGroup =
      ListResourcesInProtectionGroupResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourcesInProtectionGroupResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "ResourceArns" Lude..!@ Lude.mempty)
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
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ProtectionGroupId" Lude..= protectionGroupId)
          ]
      )

instance Lude.ToPath ListResourcesInProtectionGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ListResourcesInProtectionGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListResourcesInProtectionGroupResponse' smart constructor.
data ListResourcesInProtectionGroupResponse = ListResourcesInProtectionGroupResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int,
    resourceARNs ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourcesInProtectionGroupResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If you specify a value for @MaxResults@ and you have more resources in the protection group than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects.
-- * 'resourceARNs' - The Amazon Resource Names (ARNs) of the resources that are included in the protection group.
-- * 'responseStatus' - The response status code.
mkListResourcesInProtectionGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourcesInProtectionGroupResponse
mkListResourcesInProtectionGroupResponse pResponseStatus_ =
  ListResourcesInProtectionGroupResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      resourceARNs = Lude.mempty
    }

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

-- | The Amazon Resource Names (ARNs) of the resources that are included in the protection group.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lripgrsResourceARNs :: Lens.Lens' ListResourcesInProtectionGroupResponse [Lude.Text]
lripgrsResourceARNs = Lens.lens (resourceARNs :: ListResourcesInProtectionGroupResponse -> [Lude.Text]) (\s a -> s {resourceARNs = a} :: ListResourcesInProtectionGroupResponse)
{-# DEPRECATED lripgrsResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}
