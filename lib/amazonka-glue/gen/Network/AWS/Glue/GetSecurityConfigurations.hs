{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSecurityConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all security configurations.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetSecurityConfigurations
  ( -- * Creating a request
    GetSecurityConfigurations (..),
    mkGetSecurityConfigurations,

    -- ** Request lenses
    gscNextToken,
    gscMaxResults,

    -- * Destructuring the response
    GetSecurityConfigurationsResponse (..),
    mkGetSecurityConfigurationsResponse,

    -- ** Response lenses
    gscsrsSecurityConfigurations,
    gscsrsNextToken,
    gscsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSecurityConfigurations' smart constructor.
data GetSecurityConfigurations = GetSecurityConfigurations'
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

-- | Creates a value of 'GetSecurityConfigurations' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return.
-- * 'nextToken' - A continuation token, if this is a continuation call.
mkGetSecurityConfigurations ::
  GetSecurityConfigurations
mkGetSecurityConfigurations =
  GetSecurityConfigurations'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscNextToken :: Lens.Lens' GetSecurityConfigurations (Lude.Maybe Lude.Text)
gscNextToken = Lens.lens (nextToken :: GetSecurityConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSecurityConfigurations)
{-# DEPRECATED gscNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscMaxResults :: Lens.Lens' GetSecurityConfigurations (Lude.Maybe Lude.Natural)
gscMaxResults = Lens.lens (maxResults :: GetSecurityConfigurations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetSecurityConfigurations)
{-# DEPRECATED gscMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetSecurityConfigurations where
  page rq rs
    | Page.stop (rs Lens.^. gscsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gscsrsSecurityConfigurations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gscNextToken Lens..~ rs Lens.^. gscsrsNextToken

instance Lude.AWSRequest GetSecurityConfigurations where
  type
    Rs GetSecurityConfigurations =
      GetSecurityConfigurationsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSecurityConfigurationsResponse'
            Lude.<$> (x Lude..?> "SecurityConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSecurityConfigurations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetSecurityConfigurations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSecurityConfigurations where
  toJSON GetSecurityConfigurations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetSecurityConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSecurityConfigurations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSecurityConfigurationsResponse' smart constructor.
data GetSecurityConfigurationsResponse = GetSecurityConfigurationsResponse'
  { securityConfigurations ::
      Lude.Maybe
        [SecurityConfiguration],
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSecurityConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if there are more security configurations to return.
-- * 'responseStatus' - The response status code.
-- * 'securityConfigurations' - A list of security configurations.
mkGetSecurityConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSecurityConfigurationsResponse
mkGetSecurityConfigurationsResponse pResponseStatus_ =
  GetSecurityConfigurationsResponse'
    { securityConfigurations =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of security configurations.
--
-- /Note:/ Consider using 'securityConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrsSecurityConfigurations :: Lens.Lens' GetSecurityConfigurationsResponse (Lude.Maybe [SecurityConfiguration])
gscsrsSecurityConfigurations = Lens.lens (securityConfigurations :: GetSecurityConfigurationsResponse -> Lude.Maybe [SecurityConfiguration]) (\s a -> s {securityConfigurations = a} :: GetSecurityConfigurationsResponse)
{-# DEPRECATED gscsrsSecurityConfigurations "Use generic-lens or generic-optics with 'securityConfigurations' instead." #-}

-- | A continuation token, if there are more security configurations to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrsNextToken :: Lens.Lens' GetSecurityConfigurationsResponse (Lude.Maybe Lude.Text)
gscsrsNextToken = Lens.lens (nextToken :: GetSecurityConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSecurityConfigurationsResponse)
{-# DEPRECATED gscsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrsResponseStatus :: Lens.Lens' GetSecurityConfigurationsResponse Lude.Int
gscsrsResponseStatus = Lens.lens (responseStatus :: GetSecurityConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSecurityConfigurationsResponse)
{-# DEPRECATED gscsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
