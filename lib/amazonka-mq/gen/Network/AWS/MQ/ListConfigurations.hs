{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.ListConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all configurations.
module Network.AWS.MQ.ListConfigurations
  ( -- * Creating a request
    ListConfigurations (..),
    mkListConfigurations,

    -- ** Request lenses
    lcNextToken,
    lcMaxResults,

    -- * Destructuring the response
    ListConfigurationsResponse (..),
    mkListConfigurationsResponse,

    -- ** Response lenses
    lcrsConfigurations,
    lcrsNextToken,
    lcrsMaxResults,
    lcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListConfigurations' smart constructor.
data ListConfigurations = ListConfigurations'
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

-- | Creates a value of 'ListConfigurations' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
-- * 'nextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
mkListConfigurations ::
  ListConfigurations
mkListConfigurations =
  ListConfigurations'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListConfigurations (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConfigurations)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListConfigurations (Lude.Maybe Lude.Natural)
lcMaxResults = Lens.lens (maxResults :: ListConfigurations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListConfigurations)
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListConfigurations where
  type Rs ListConfigurations = ListConfigurationsResponse
  request = Req.get mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListConfigurationsResponse'
            Lude.<$> (x Lude..?> "configurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "maxResults")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListConfigurations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListConfigurations where
  toPath = Lude.const "/v1/configurations"

instance Lude.ToQuery ListConfigurations where
  toQuery ListConfigurations' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListConfigurationsResponse' smart constructor.
data ListConfigurationsResponse = ListConfigurationsResponse'
  { configurations ::
      Lude.Maybe [Configuration],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'configurations' - The list of all revisions for the specified configuration.
-- * 'maxResults' - The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
-- * 'nextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
-- * 'responseStatus' - The response status code.
mkListConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListConfigurationsResponse
mkListConfigurationsResponse pResponseStatus_ =
  ListConfigurationsResponse'
    { configurations = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of all revisions for the specified configuration.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsConfigurations :: Lens.Lens' ListConfigurationsResponse (Lude.Maybe [Configuration])
lcrsConfigurations = Lens.lens (configurations :: ListConfigurationsResponse -> Lude.Maybe [Configuration]) (\s a -> s {configurations = a} :: ListConfigurationsResponse)
{-# DEPRECATED lcrsConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextToken :: Lens.Lens' ListConfigurationsResponse (Lude.Maybe Lude.Text)
lcrsNextToken = Lens.lens (nextToken :: ListConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConfigurationsResponse)
{-# DEPRECATED lcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsMaxResults :: Lens.Lens' ListConfigurationsResponse (Lude.Maybe Lude.Int)
lcrsMaxResults = Lens.lens (maxResults :: ListConfigurationsResponse -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListConfigurationsResponse)
{-# DEPRECATED lcrsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListConfigurationsResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListConfigurationsResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
