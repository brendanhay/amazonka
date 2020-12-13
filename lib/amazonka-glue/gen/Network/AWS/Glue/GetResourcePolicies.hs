{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetResourcePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the security configurations for the resource policies set on individual resources, and also the account-level policy.
--
-- This operation also returns the Data Catalog resource policy. However, if you enabled metadata encryption in Data Catalog settings, and you do not have permission on the AWS KMS key, the operation can't return the Data Catalog resource policy.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetResourcePolicies
  ( -- * Creating a request
    GetResourcePolicies (..),
    mkGetResourcePolicies,

    -- ** Request lenses
    grpNextToken,
    grpMaxResults,

    -- * Destructuring the response
    GetResourcePoliciesResponse (..),
    mkGetResourcePoliciesResponse,

    -- ** Response lenses
    grprsGetResourcePoliciesResponseList,
    grprsNextToken,
    grprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetResourcePolicies' smart constructor.
data GetResourcePolicies = GetResourcePolicies'
  { -- | A continuation token, if this is a continuation request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum size of a list to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResourcePolicies' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if this is a continuation request.
-- * 'maxResults' - The maximum size of a list to return.
mkGetResourcePolicies ::
  GetResourcePolicies
mkGetResourcePolicies =
  GetResourcePolicies'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpNextToken :: Lens.Lens' GetResourcePolicies (Lude.Maybe Lude.Text)
grpNextToken = Lens.lens (nextToken :: GetResourcePolicies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetResourcePolicies)
{-# DEPRECATED grpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpMaxResults :: Lens.Lens' GetResourcePolicies (Lude.Maybe Lude.Natural)
grpMaxResults = Lens.lens (maxResults :: GetResourcePolicies -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetResourcePolicies)
{-# DEPRECATED grpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetResourcePolicies where
  page rq rs
    | Page.stop (rs Lens.^. grprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grprsGetResourcePoliciesResponseList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grpNextToken Lens..~ rs Lens.^. grprsNextToken

instance Lude.AWSRequest GetResourcePolicies where
  type Rs GetResourcePolicies = GetResourcePoliciesResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetResourcePoliciesResponse'
            Lude.<$> (x Lude..?> "GetResourcePoliciesResponseList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetResourcePolicies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetResourcePolicies" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetResourcePolicies where
  toJSON GetResourcePolicies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetResourcePolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery GetResourcePolicies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetResourcePoliciesResponse' smart constructor.
data GetResourcePoliciesResponse = GetResourcePoliciesResponse'
  { -- | A list of the individual resource policies and the account-level resource policy.
    getResourcePoliciesResponseList :: Lude.Maybe [GluePolicy],
    -- | A continuation token, if the returned list does not contain the last resource policy available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResourcePoliciesResponse' with the minimum fields required to make a request.
--
-- * 'getResourcePoliciesResponseList' - A list of the individual resource policies and the account-level resource policy.
-- * 'nextToken' - A continuation token, if the returned list does not contain the last resource policy available.
-- * 'responseStatus' - The response status code.
mkGetResourcePoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetResourcePoliciesResponse
mkGetResourcePoliciesResponse pResponseStatus_ =
  GetResourcePoliciesResponse'
    { getResourcePoliciesResponseList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the individual resource policies and the account-level resource policy.
--
-- /Note:/ Consider using 'getResourcePoliciesResponseList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsGetResourcePoliciesResponseList :: Lens.Lens' GetResourcePoliciesResponse (Lude.Maybe [GluePolicy])
grprsGetResourcePoliciesResponseList = Lens.lens (getResourcePoliciesResponseList :: GetResourcePoliciesResponse -> Lude.Maybe [GluePolicy]) (\s a -> s {getResourcePoliciesResponseList = a} :: GetResourcePoliciesResponse)
{-# DEPRECATED grprsGetResourcePoliciesResponseList "Use generic-lens or generic-optics with 'getResourcePoliciesResponseList' instead." #-}

-- | A continuation token, if the returned list does not contain the last resource policy available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsNextToken :: Lens.Lens' GetResourcePoliciesResponse (Lude.Maybe Lude.Text)
grprsNextToken = Lens.lens (nextToken :: GetResourcePoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetResourcePoliciesResponse)
{-# DEPRECATED grprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsResponseStatus :: Lens.Lens' GetResourcePoliciesResponse Lude.Int
grprsResponseStatus = Lens.lens (responseStatus :: GetResourcePoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetResourcePoliciesResponse)
{-# DEPRECATED grprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
