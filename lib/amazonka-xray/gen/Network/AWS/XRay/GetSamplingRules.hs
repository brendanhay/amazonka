{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetSamplingRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all sampling rules.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetSamplingRules
  ( -- * Creating a request
    GetSamplingRules (..),
    mkGetSamplingRules,

    -- ** Request lenses
    gsrNextToken,

    -- * Destructuring the response
    GetSamplingRulesResponse (..),
    mkGetSamplingRulesResponse,

    -- ** Response lenses
    gsrrsSamplingRuleRecords,
    gsrrsNextToken,
    gsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetSamplingRules' smart constructor.
newtype GetSamplingRules = GetSamplingRules'
  { nextToken ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSamplingRules' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token.
mkGetSamplingRules ::
  GetSamplingRules
mkGetSamplingRules = GetSamplingRules' {nextToken = Lude.Nothing}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrNextToken :: Lens.Lens' GetSamplingRules (Lude.Maybe Lude.Text)
gsrNextToken = Lens.lens (nextToken :: GetSamplingRules -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSamplingRules)
{-# DEPRECATED gsrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager GetSamplingRules where
  page rq rs
    | Page.stop (rs Lens.^. gsrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gsrrsSamplingRuleRecords) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gsrNextToken Lens..~ rs Lens.^. gsrrsNextToken

instance Lude.AWSRequest GetSamplingRules where
  type Rs GetSamplingRules = GetSamplingRulesResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSamplingRulesResponse'
            Lude.<$> (x Lude..?> "SamplingRuleRecords" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSamplingRules where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetSamplingRules where
  toJSON GetSamplingRules' {..} =
    Lude.object
      (Lude.catMaybes [("NextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath GetSamplingRules where
  toPath = Lude.const "/GetSamplingRules"

instance Lude.ToQuery GetSamplingRules where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSamplingRulesResponse' smart constructor.
data GetSamplingRulesResponse = GetSamplingRulesResponse'
  { samplingRuleRecords ::
      Lude.Maybe [SamplingRuleRecord],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetSamplingRulesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token.
-- * 'responseStatus' - The response status code.
-- * 'samplingRuleRecords' - Rule definitions and metadata.
mkGetSamplingRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSamplingRulesResponse
mkGetSamplingRulesResponse pResponseStatus_ =
  GetSamplingRulesResponse'
    { samplingRuleRecords = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Rule definitions and metadata.
--
-- /Note:/ Consider using 'samplingRuleRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSamplingRuleRecords :: Lens.Lens' GetSamplingRulesResponse (Lude.Maybe [SamplingRuleRecord])
gsrrsSamplingRuleRecords = Lens.lens (samplingRuleRecords :: GetSamplingRulesResponse -> Lude.Maybe [SamplingRuleRecord]) (\s a -> s {samplingRuleRecords = a} :: GetSamplingRulesResponse)
{-# DEPRECATED gsrrsSamplingRuleRecords "Use generic-lens or generic-optics with 'samplingRuleRecords' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsNextToken :: Lens.Lens' GetSamplingRulesResponse (Lude.Maybe Lude.Text)
gsrrsNextToken = Lens.lens (nextToken :: GetSamplingRulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSamplingRulesResponse)
{-# DEPRECATED gsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetSamplingRulesResponse Lude.Int
gsrrsResponseStatus = Lens.lens (responseStatus :: GetSamplingRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSamplingRulesResponse)
{-# DEPRECATED gsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
