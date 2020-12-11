{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListSqlInjectionMatchSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'SqlInjectionMatchSet' objects.
module Network.AWS.WAFRegional.ListSqlInjectionMatchSets
  ( -- * Creating a request
    ListSqlInjectionMatchSets (..),
    mkListSqlInjectionMatchSets,

    -- ** Request lenses
    lsimsNextMarker,
    lsimsLimit,

    -- * Destructuring the response
    ListSqlInjectionMatchSetsResponse (..),
    mkListSqlInjectionMatchSetsResponse,

    -- ** Response lenses
    lsimsrsNextMarker,
    lsimsrsSqlInjectionMatchSets,
    lsimsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | A request to list the 'SqlInjectionMatchSet' objects created by the current AWS account.
--
-- /See:/ 'mkListSqlInjectionMatchSets' smart constructor.
data ListSqlInjectionMatchSets = ListSqlInjectionMatchSets'
  { nextMarker ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSqlInjectionMatchSets' with the minimum fields required to make a request.
--
-- * 'limit' - Specifies the number of 'SqlInjectionMatchSet' objects that you want AWS WAF to return for this request. If you have more @SqlInjectionMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
-- * 'nextMarker' - If you specify a value for @Limit@ and you have more 'SqlInjectionMatchSet' objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SqlInjectionMatchSets@ . For the second and subsequent @ListSqlInjectionMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SqlInjectionMatchSets@ .
mkListSqlInjectionMatchSets ::
  ListSqlInjectionMatchSets
mkListSqlInjectionMatchSets =
  ListSqlInjectionMatchSets'
    { nextMarker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more 'SqlInjectionMatchSet' objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SqlInjectionMatchSets@ . For the second and subsequent @ListSqlInjectionMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SqlInjectionMatchSets@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsimsNextMarker :: Lens.Lens' ListSqlInjectionMatchSets (Lude.Maybe Lude.Text)
lsimsNextMarker = Lens.lens (nextMarker :: ListSqlInjectionMatchSets -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListSqlInjectionMatchSets)
{-# DEPRECATED lsimsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Specifies the number of 'SqlInjectionMatchSet' objects that you want AWS WAF to return for this request. If you have more @SqlInjectionMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsimsLimit :: Lens.Lens' ListSqlInjectionMatchSets (Lude.Maybe Lude.Natural)
lsimsLimit = Lens.lens (limit :: ListSqlInjectionMatchSets -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListSqlInjectionMatchSets)
{-# DEPRECATED lsimsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListSqlInjectionMatchSets where
  type
    Rs ListSqlInjectionMatchSets =
      ListSqlInjectionMatchSetsResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSqlInjectionMatchSetsResponse'
            Lude.<$> (x Lude..?> "NextMarker")
            Lude.<*> (x Lude..?> "SqlInjectionMatchSets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSqlInjectionMatchSets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.ListSqlInjectionMatchSets" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSqlInjectionMatchSets where
  toJSON ListSqlInjectionMatchSets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextMarker" Lude..=) Lude.<$> nextMarker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListSqlInjectionMatchSets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSqlInjectionMatchSets where
  toQuery = Lude.const Lude.mempty

-- | The response to a 'ListSqlInjectionMatchSets' request.
--
-- /See:/ 'mkListSqlInjectionMatchSetsResponse' smart constructor.
data ListSqlInjectionMatchSetsResponse = ListSqlInjectionMatchSetsResponse'
  { nextMarker ::
      Lude.Maybe Lude.Text,
    sqlInjectionMatchSets ::
      Lude.Maybe
        [SqlInjectionMatchSetSummary],
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

-- | Creates a value of 'ListSqlInjectionMatchSetsResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If you have more 'SqlInjectionMatchSet' objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SqlInjectionMatchSet@ objects, submit another @ListSqlInjectionMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
-- * 'responseStatus' - The response status code.
-- * 'sqlInjectionMatchSets' - An array of 'SqlInjectionMatchSetSummary' objects.
mkListSqlInjectionMatchSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSqlInjectionMatchSetsResponse
mkListSqlInjectionMatchSetsResponse pResponseStatus_ =
  ListSqlInjectionMatchSetsResponse'
    { nextMarker = Lude.Nothing,
      sqlInjectionMatchSets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If you have more 'SqlInjectionMatchSet' objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SqlInjectionMatchSet@ objects, submit another @ListSqlInjectionMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsimsrsNextMarker :: Lens.Lens' ListSqlInjectionMatchSetsResponse (Lude.Maybe Lude.Text)
lsimsrsNextMarker = Lens.lens (nextMarker :: ListSqlInjectionMatchSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListSqlInjectionMatchSetsResponse)
{-# DEPRECATED lsimsrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | An array of 'SqlInjectionMatchSetSummary' objects.
--
-- /Note:/ Consider using 'sqlInjectionMatchSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsimsrsSqlInjectionMatchSets :: Lens.Lens' ListSqlInjectionMatchSetsResponse (Lude.Maybe [SqlInjectionMatchSetSummary])
lsimsrsSqlInjectionMatchSets = Lens.lens (sqlInjectionMatchSets :: ListSqlInjectionMatchSetsResponse -> Lude.Maybe [SqlInjectionMatchSetSummary]) (\s a -> s {sqlInjectionMatchSets = a} :: ListSqlInjectionMatchSetsResponse)
{-# DEPRECATED lsimsrsSqlInjectionMatchSets "Use generic-lens or generic-optics with 'sqlInjectionMatchSets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsimsrsResponseStatus :: Lens.Lens' ListSqlInjectionMatchSetsResponse Lude.Int
lsimsrsResponseStatus = Lens.lens (responseStatus :: ListSqlInjectionMatchSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSqlInjectionMatchSetsResponse)
{-# DEPRECATED lsimsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
