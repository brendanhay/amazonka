{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.ListProtectionGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the 'ProtectionGroup' objects for the account.
module Network.AWS.Shield.ListProtectionGroups
  ( -- * Creating a request
    ListProtectionGroups (..),
    mkListProtectionGroups,

    -- ** Request lenses
    lpgNextToken,
    lpgMaxResults,

    -- * Destructuring the response
    ListProtectionGroupsResponse (..),
    mkListProtectionGroupsResponse,

    -- ** Response lenses
    lpgrsNextToken,
    lpgrsResponseStatus,
    lpgrsProtectionGroups,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkListProtectionGroups' smart constructor.
data ListProtectionGroups = ListProtectionGroups'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProtectionGroups' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of 'ProtectionGroup' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
-- * 'nextToken' - The next token value from a previous call to @ListProtectionGroups@ . Pass null if this is the first call.
mkListProtectionGroups ::
  ListProtectionGroups
mkListProtectionGroups =
  ListProtectionGroups'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The next token value from a previous call to @ListProtectionGroups@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgNextToken :: Lens.Lens' ListProtectionGroups (Lude.Maybe Lude.Text)
lpgNextToken = Lens.lens (nextToken :: ListProtectionGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProtectionGroups)
{-# DEPRECATED lpgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of 'ProtectionGroup' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgMaxResults :: Lens.Lens' ListProtectionGroups (Lude.Maybe Lude.Natural)
lpgMaxResults = Lens.lens (maxResults :: ListProtectionGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListProtectionGroups)
{-# DEPRECATED lpgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListProtectionGroups where
  type Rs ListProtectionGroups = ListProtectionGroupsResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProtectionGroupsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "ProtectionGroups" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListProtectionGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.ListProtectionGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListProtectionGroups where
  toJSON ListProtectionGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListProtectionGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListProtectionGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListProtectionGroupsResponse' smart constructor.
data ListProtectionGroupsResponse = ListProtectionGroupsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    protectionGroups ::
      [ProtectionGroup]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProtectionGroupsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If you specify a value for @MaxResults@ and you have more protection groups than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects.
-- * 'protectionGroups' -
-- * 'responseStatus' - The response status code.
mkListProtectionGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProtectionGroupsResponse
mkListProtectionGroupsResponse pResponseStatus_ =
  ListProtectionGroupsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      protectionGroups = Lude.mempty
    }

-- | If you specify a value for @MaxResults@ and you have more protection groups than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgrsNextToken :: Lens.Lens' ListProtectionGroupsResponse (Lude.Maybe Lude.Text)
lpgrsNextToken = Lens.lens (nextToken :: ListProtectionGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProtectionGroupsResponse)
{-# DEPRECATED lpgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgrsResponseStatus :: Lens.Lens' ListProtectionGroupsResponse Lude.Int
lpgrsResponseStatus = Lens.lens (responseStatus :: ListProtectionGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProtectionGroupsResponse)
{-# DEPRECATED lpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- |
--
-- /Note:/ Consider using 'protectionGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgrsProtectionGroups :: Lens.Lens' ListProtectionGroupsResponse [ProtectionGroup]
lpgrsProtectionGroups = Lens.lens (protectionGroups :: ListProtectionGroupsResponse -> [ProtectionGroup]) (\s a -> s {protectionGroups = a} :: ListProtectionGroupsResponse)
{-# DEPRECATED lpgrsProtectionGroups "Use generic-lens or generic-optics with 'protectionGroups' instead." #-}
