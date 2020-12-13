{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeRemediationExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a detailed view of a Remediation Execution for a set of resources including state, timestamps for when steps for the remediation execution occur, and any error messages for steps that have failed. When you specify the limit and the next token, you receive a paginated response.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeRemediationExecutionStatus
  ( -- * Creating a request
    DescribeRemediationExecutionStatus (..),
    mkDescribeRemediationExecutionStatus,

    -- ** Request lenses
    dresConfigRuleName,
    dresNextToken,
    dresLimit,
    dresResourceKeys,

    -- * Destructuring the response
    DescribeRemediationExecutionStatusResponse (..),
    mkDescribeRemediationExecutionStatusResponse,

    -- ** Response lenses
    dresrsRemediationExecutionStatuses,
    dresrsNextToken,
    dresrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRemediationExecutionStatus' smart constructor.
data DescribeRemediationExecutionStatus = DescribeRemediationExecutionStatus'
  { -- | A list of AWS Config rule names.
    configRuleName :: Lude.Text,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of RemediationExecutionStatuses returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Lude.Maybe Lude.Natural,
    -- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
    resourceKeys :: Lude.Maybe (Lude.NonEmpty ResourceKey)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRemediationExecutionStatus' with the minimum fields required to make a request.
--
-- * 'configRuleName' - A list of AWS Config rule names.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The maximum number of RemediationExecutionStatuses returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
-- * 'resourceKeys' - A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
mkDescribeRemediationExecutionStatus ::
  -- | 'configRuleName'
  Lude.Text ->
  DescribeRemediationExecutionStatus
mkDescribeRemediationExecutionStatus pConfigRuleName_ =
  DescribeRemediationExecutionStatus'
    { configRuleName =
        pConfigRuleName_,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      resourceKeys = Lude.Nothing
    }

-- | A list of AWS Config rule names.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresConfigRuleName :: Lens.Lens' DescribeRemediationExecutionStatus Lude.Text
dresConfigRuleName = Lens.lens (configRuleName :: DescribeRemediationExecutionStatus -> Lude.Text) (\s a -> s {configRuleName = a} :: DescribeRemediationExecutionStatus)
{-# DEPRECATED dresConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresNextToken :: Lens.Lens' DescribeRemediationExecutionStatus (Lude.Maybe Lude.Text)
dresNextToken = Lens.lens (nextToken :: DescribeRemediationExecutionStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeRemediationExecutionStatus)
{-# DEPRECATED dresNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of RemediationExecutionStatuses returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresLimit :: Lens.Lens' DescribeRemediationExecutionStatus (Lude.Maybe Lude.Natural)
dresLimit = Lens.lens (limit :: DescribeRemediationExecutionStatus -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeRemediationExecutionStatus)
{-# DEPRECATED dresLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresResourceKeys :: Lens.Lens' DescribeRemediationExecutionStatus (Lude.Maybe (Lude.NonEmpty ResourceKey))
dresResourceKeys = Lens.lens (resourceKeys :: DescribeRemediationExecutionStatus -> Lude.Maybe (Lude.NonEmpty ResourceKey)) (\s a -> s {resourceKeys = a} :: DescribeRemediationExecutionStatus)
{-# DEPRECATED dresResourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead." #-}

instance Page.AWSPager DescribeRemediationExecutionStatus where
  page rq rs
    | Page.stop (rs Lens.^. dresrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dresrsRemediationExecutionStatuses) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dresNextToken Lens..~ rs Lens.^. dresrsNextToken

instance Lude.AWSRequest DescribeRemediationExecutionStatus where
  type
    Rs DescribeRemediationExecutionStatus =
      DescribeRemediationExecutionStatusResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRemediationExecutionStatusResponse'
            Lude.<$> (x Lude..?> "RemediationExecutionStatuses" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRemediationExecutionStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeRemediationExecutionStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRemediationExecutionStatus where
  toJSON DescribeRemediationExecutionStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ConfigRuleName" Lude..= configRuleName),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("ResourceKeys" Lude..=) Lude.<$> resourceKeys
          ]
      )

instance Lude.ToPath DescribeRemediationExecutionStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRemediationExecutionStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeRemediationExecutionStatusResponse' smart constructor.
data DescribeRemediationExecutionStatusResponse = DescribeRemediationExecutionStatusResponse'
  { -- | Returns a list of remediation execution statuses objects.
    remediationExecutionStatuses :: Lude.Maybe [RemediationExecutionStatus],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRemediationExecutionStatusResponse' with the minimum fields required to make a request.
--
-- * 'remediationExecutionStatuses' - Returns a list of remediation execution statuses objects.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkDescribeRemediationExecutionStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRemediationExecutionStatusResponse
mkDescribeRemediationExecutionStatusResponse pResponseStatus_ =
  DescribeRemediationExecutionStatusResponse'
    { remediationExecutionStatuses =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of remediation execution statuses objects.
--
-- /Note:/ Consider using 'remediationExecutionStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresrsRemediationExecutionStatuses :: Lens.Lens' DescribeRemediationExecutionStatusResponse (Lude.Maybe [RemediationExecutionStatus])
dresrsRemediationExecutionStatuses = Lens.lens (remediationExecutionStatuses :: DescribeRemediationExecutionStatusResponse -> Lude.Maybe [RemediationExecutionStatus]) (\s a -> s {remediationExecutionStatuses = a} :: DescribeRemediationExecutionStatusResponse)
{-# DEPRECATED dresrsRemediationExecutionStatuses "Use generic-lens or generic-optics with 'remediationExecutionStatuses' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresrsNextToken :: Lens.Lens' DescribeRemediationExecutionStatusResponse (Lude.Maybe Lude.Text)
dresrsNextToken = Lens.lens (nextToken :: DescribeRemediationExecutionStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeRemediationExecutionStatusResponse)
{-# DEPRECATED dresrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dresrsResponseStatus :: Lens.Lens' DescribeRemediationExecutionStatusResponse Lude.Int
dresrsResponseStatus = Lens.lens (responseStatus :: DescribeRemediationExecutionStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRemediationExecutionStatusResponse)
{-# DEPRECATED dresrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
