{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.DescribeReportDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AWS Cost and Usage reports available to this account.
--
-- This operation returns paginated results.
module Network.AWS.CostAndUsageReport.DescribeReportDefinitions
  ( -- * Creating a request
    DescribeReportDefinitions (..),
    mkDescribeReportDefinitions,

    -- ** Request lenses
    drdNextToken,
    drdMaxResults,

    -- * Destructuring the response
    DescribeReportDefinitionsResponse (..),
    mkDescribeReportDefinitionsResponse,

    -- ** Response lenses
    drdrsNextToken,
    drdrsReportDefinitions,
    drdrsResponseStatus,
  )
where

import Network.AWS.CostAndUsageReport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Requests a list of AWS Cost and Usage reports owned by the account.
--
-- /See:/ 'mkDescribeReportDefinitions' smart constructor.
data DescribeReportDefinitions = DescribeReportDefinitions'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReportDefinitions' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'maxResults' -
mkDescribeReportDefinitions ::
  DescribeReportDefinitions
mkDescribeReportDefinitions =
  DescribeReportDefinitions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdNextToken :: Lens.Lens' DescribeReportDefinitions (Lude.Maybe Lude.Text)
drdNextToken = Lens.lens (nextToken :: DescribeReportDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeReportDefinitions)
{-# DEPRECATED drdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdMaxResults :: Lens.Lens' DescribeReportDefinitions (Lude.Maybe Lude.Natural)
drdMaxResults = Lens.lens (maxResults :: DescribeReportDefinitions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeReportDefinitions)
{-# DEPRECATED drdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeReportDefinitions where
  page rq rs
    | Page.stop (rs Lens.^. drdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drdrsReportDefinitions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drdNextToken Lens..~ rs Lens.^. drdrsNextToken

instance Lude.AWSRequest DescribeReportDefinitions where
  type
    Rs DescribeReportDefinitions =
      DescribeReportDefinitionsResponse
  request = Req.postJSON costAndUsageReportService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReportDefinitionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ReportDefinitions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReportDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrigamiServiceGatewayService.DescribeReportDefinitions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeReportDefinitions where
  toJSON DescribeReportDefinitions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeReportDefinitions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReportDefinitions where
  toQuery = Lude.const Lude.mempty

-- | If the action is successful, the service sends back an HTTP 200 response.
--
-- /See:/ 'mkDescribeReportDefinitionsResponse' smart constructor.
data DescribeReportDefinitionsResponse = DescribeReportDefinitionsResponse'
  { nextToken :: Lude.Maybe Lude.Text,
    -- | A list of AWS Cost and Usage reports owned by the account.
    reportDefinitions :: Lude.Maybe [ReportDefinition],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReportDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'reportDefinitions' - A list of AWS Cost and Usage reports owned by the account.
-- * 'responseStatus' - The response status code.
mkDescribeReportDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReportDefinitionsResponse
mkDescribeReportDefinitionsResponse pResponseStatus_ =
  DescribeReportDefinitionsResponse'
    { nextToken = Lude.Nothing,
      reportDefinitions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdrsNextToken :: Lens.Lens' DescribeReportDefinitionsResponse (Lude.Maybe Lude.Text)
drdrsNextToken = Lens.lens (nextToken :: DescribeReportDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeReportDefinitionsResponse)
{-# DEPRECATED drdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of AWS Cost and Usage reports owned by the account.
--
-- /Note:/ Consider using 'reportDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdrsReportDefinitions :: Lens.Lens' DescribeReportDefinitionsResponse (Lude.Maybe [ReportDefinition])
drdrsReportDefinitions = Lens.lens (reportDefinitions :: DescribeReportDefinitionsResponse -> Lude.Maybe [ReportDefinition]) (\s a -> s {reportDefinitions = a} :: DescribeReportDefinitionsResponse)
{-# DEPRECATED drdrsReportDefinitions "Use generic-lens or generic-optics with 'reportDefinitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdrsResponseStatus :: Lens.Lens' DescribeReportDefinitionsResponse Lude.Int
drdrsResponseStatus = Lens.lens (responseStatus :: DescribeReportDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReportDefinitionsResponse)
{-# DEPRECATED drdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
