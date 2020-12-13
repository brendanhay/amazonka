{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetConformancePackComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details for the conformance pack based on the cumulative compliance results of all the rules in that conformance pack.
module Network.AWS.Config.GetConformancePackComplianceSummary
  ( -- * Creating a request
    GetConformancePackComplianceSummary (..),
    mkGetConformancePackComplianceSummary,

    -- ** Request lenses
    gcpcsConformancePackNames,
    gcpcsNextToken,
    gcpcsLimit,

    -- * Destructuring the response
    GetConformancePackComplianceSummaryResponse (..),
    mkGetConformancePackComplianceSummaryResponse,

    -- ** Response lenses
    gcpcsrsConformancePackComplianceSummaryList,
    gcpcsrsNextToken,
    gcpcsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConformancePackComplianceSummary' smart constructor.
data GetConformancePackComplianceSummary = GetConformancePackComplianceSummary'
  { -- | Names of conformance packs.
    conformancePackNames :: Lude.NonEmpty Lude.Text,
    -- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of conformance packs returned on each page.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConformancePackComplianceSummary' with the minimum fields required to make a request.
--
-- * 'conformancePackNames' - Names of conformance packs.
-- * 'nextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The maximum number of conformance packs returned on each page.
mkGetConformancePackComplianceSummary ::
  -- | 'conformancePackNames'
  Lude.NonEmpty Lude.Text ->
  GetConformancePackComplianceSummary
mkGetConformancePackComplianceSummary pConformancePackNames_ =
  GetConformancePackComplianceSummary'
    { conformancePackNames =
        pConformancePackNames_,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | Names of conformance packs.
--
-- /Note:/ Consider using 'conformancePackNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcsConformancePackNames :: Lens.Lens' GetConformancePackComplianceSummary (Lude.NonEmpty Lude.Text)
gcpcsConformancePackNames = Lens.lens (conformancePackNames :: GetConformancePackComplianceSummary -> Lude.NonEmpty Lude.Text) (\s a -> s {conformancePackNames = a} :: GetConformancePackComplianceSummary)
{-# DEPRECATED gcpcsConformancePackNames "Use generic-lens or generic-optics with 'conformancePackNames' instead." #-}

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcsNextToken :: Lens.Lens' GetConformancePackComplianceSummary (Lude.Maybe Lude.Text)
gcpcsNextToken = Lens.lens (nextToken :: GetConformancePackComplianceSummary -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetConformancePackComplianceSummary)
{-# DEPRECATED gcpcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of conformance packs returned on each page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcsLimit :: Lens.Lens' GetConformancePackComplianceSummary (Lude.Maybe Lude.Natural)
gcpcsLimit = Lens.lens (limit :: GetConformancePackComplianceSummary -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetConformancePackComplianceSummary)
{-# DEPRECATED gcpcsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest GetConformancePackComplianceSummary where
  type
    Rs GetConformancePackComplianceSummary =
      GetConformancePackComplianceSummaryResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConformancePackComplianceSummaryResponse'
            Lude.<$> (x Lude..?> "ConformancePackComplianceSummaryList")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConformancePackComplianceSummary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetConformancePackComplianceSummary" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetConformancePackComplianceSummary where
  toJSON GetConformancePackComplianceSummary' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ConformancePackNames" Lude..= conformancePackNames),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath GetConformancePackComplianceSummary where
  toPath = Lude.const "/"

instance Lude.ToQuery GetConformancePackComplianceSummary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetConformancePackComplianceSummaryResponse' smart constructor.
data GetConformancePackComplianceSummaryResponse = GetConformancePackComplianceSummaryResponse'
  { -- | A list of @ConformancePackComplianceSummary@ objects.
    conformancePackComplianceSummaryList :: Lude.Maybe (Lude.NonEmpty ConformancePackComplianceSummary),
    -- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConformancePackComplianceSummaryResponse' with the minimum fields required to make a request.
--
-- * 'conformancePackComplianceSummaryList' - A list of @ConformancePackComplianceSummary@ objects.
-- * 'nextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkGetConformancePackComplianceSummaryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConformancePackComplianceSummaryResponse
mkGetConformancePackComplianceSummaryResponse pResponseStatus_ =
  GetConformancePackComplianceSummaryResponse'
    { conformancePackComplianceSummaryList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @ConformancePackComplianceSummary@ objects.
--
-- /Note:/ Consider using 'conformancePackComplianceSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcsrsConformancePackComplianceSummaryList :: Lens.Lens' GetConformancePackComplianceSummaryResponse (Lude.Maybe (Lude.NonEmpty ConformancePackComplianceSummary))
gcpcsrsConformancePackComplianceSummaryList = Lens.lens (conformancePackComplianceSummaryList :: GetConformancePackComplianceSummaryResponse -> Lude.Maybe (Lude.NonEmpty ConformancePackComplianceSummary)) (\s a -> s {conformancePackComplianceSummaryList = a} :: GetConformancePackComplianceSummaryResponse)
{-# DEPRECATED gcpcsrsConformancePackComplianceSummaryList "Use generic-lens or generic-optics with 'conformancePackComplianceSummaryList' instead." #-}

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcsrsNextToken :: Lens.Lens' GetConformancePackComplianceSummaryResponse (Lude.Maybe Lude.Text)
gcpcsrsNextToken = Lens.lens (nextToken :: GetConformancePackComplianceSummaryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetConformancePackComplianceSummaryResponse)
{-# DEPRECATED gcpcsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcsrsResponseStatus :: Lens.Lens' GetConformancePackComplianceSummaryResponse Lude.Int
gcpcsrsResponseStatus = Lens.lens (responseStatus :: GetConformancePackComplianceSummaryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConformancePackComplianceSummaryResponse)
{-# DEPRECATED gcpcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
