{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConformancePackCompliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details for each rule in that conformance pack.
module Network.AWS.Config.DescribeConformancePackCompliance
  ( -- * Creating a request
    DescribeConformancePackCompliance (..),
    mkDescribeConformancePackCompliance,

    -- ** Request lenses
    dcpcFilters,
    dcpcConformancePackName,
    dcpcNextToken,
    dcpcLimit,

    -- * Destructuring the response
    DescribeConformancePackComplianceResponse (..),
    mkDescribeConformancePackComplianceResponse,

    -- ** Response lenses
    dcpcrsConformancePackRuleComplianceList,
    dcpcrsConformancePackName,
    dcpcrsNextToken,
    dcpcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeConformancePackCompliance' smart constructor.
data DescribeConformancePackCompliance = DescribeConformancePackCompliance'
  { -- | A @ConformancePackComplianceFilters@ object.
    filters :: Lude.Maybe ConformancePackComplianceFilters,
    -- | Name of the conformance pack.
    conformancePackName :: Lude.Text,
    -- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of AWS Config rules within a conformance pack are returned on each page.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConformancePackCompliance' with the minimum fields required to make a request.
--
-- * 'filters' - A @ConformancePackComplianceFilters@ object.
-- * 'conformancePackName' - Name of the conformance pack.
-- * 'nextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
-- * 'limit' - The maximum number of AWS Config rules within a conformance pack are returned on each page.
mkDescribeConformancePackCompliance ::
  -- | 'conformancePackName'
  Lude.Text ->
  DescribeConformancePackCompliance
mkDescribeConformancePackCompliance pConformancePackName_ =
  DescribeConformancePackCompliance'
    { filters = Lude.Nothing,
      conformancePackName = pConformancePackName_,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | A @ConformancePackComplianceFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcFilters :: Lens.Lens' DescribeConformancePackCompliance (Lude.Maybe ConformancePackComplianceFilters)
dcpcFilters = Lens.lens (filters :: DescribeConformancePackCompliance -> Lude.Maybe ConformancePackComplianceFilters) (\s a -> s {filters = a} :: DescribeConformancePackCompliance)
{-# DEPRECATED dcpcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcConformancePackName :: Lens.Lens' DescribeConformancePackCompliance Lude.Text
dcpcConformancePackName = Lens.lens (conformancePackName :: DescribeConformancePackCompliance -> Lude.Text) (\s a -> s {conformancePackName = a} :: DescribeConformancePackCompliance)
{-# DEPRECATED dcpcConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcNextToken :: Lens.Lens' DescribeConformancePackCompliance (Lude.Maybe Lude.Text)
dcpcNextToken = Lens.lens (nextToken :: DescribeConformancePackCompliance -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConformancePackCompliance)
{-# DEPRECATED dcpcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of AWS Config rules within a conformance pack are returned on each page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcLimit :: Lens.Lens' DescribeConformancePackCompliance (Lude.Maybe Lude.Natural)
dcpcLimit = Lens.lens (limit :: DescribeConformancePackCompliance -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeConformancePackCompliance)
{-# DEPRECATED dcpcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest DescribeConformancePackCompliance where
  type
    Rs DescribeConformancePackCompliance =
      DescribeConformancePackComplianceResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConformancePackComplianceResponse'
            Lude.<$> ( x Lude..?> "ConformancePackRuleComplianceList"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..:> "ConformancePackName")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConformancePackCompliance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeConformancePackCompliance" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConformancePackCompliance where
  toJSON DescribeConformancePackCompliance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            Lude.Just ("ConformancePackName" Lude..= conformancePackName),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeConformancePackCompliance where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConformancePackCompliance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeConformancePackComplianceResponse' smart constructor.
data DescribeConformancePackComplianceResponse = DescribeConformancePackComplianceResponse'
  { -- | Returns a list of @ConformancePackRuleCompliance@ objects.
    conformancePackRuleComplianceList :: [ConformancePackRuleCompliance],
    -- | Name of the conformance pack.
    conformancePackName :: Lude.Text,
    -- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConformancePackComplianceResponse' with the minimum fields required to make a request.
--
-- * 'conformancePackRuleComplianceList' - Returns a list of @ConformancePackRuleCompliance@ objects.
-- * 'conformancePackName' - Name of the conformance pack.
-- * 'nextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkDescribeConformancePackComplianceResponse ::
  -- | 'conformancePackName'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConformancePackComplianceResponse
mkDescribeConformancePackComplianceResponse
  pConformancePackName_
  pResponseStatus_ =
    DescribeConformancePackComplianceResponse'
      { conformancePackRuleComplianceList =
          Lude.mempty,
        conformancePackName = pConformancePackName_,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Returns a list of @ConformancePackRuleCompliance@ objects.
--
-- /Note:/ Consider using 'conformancePackRuleComplianceList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcrsConformancePackRuleComplianceList :: Lens.Lens' DescribeConformancePackComplianceResponse [ConformancePackRuleCompliance]
dcpcrsConformancePackRuleComplianceList = Lens.lens (conformancePackRuleComplianceList :: DescribeConformancePackComplianceResponse -> [ConformancePackRuleCompliance]) (\s a -> s {conformancePackRuleComplianceList = a} :: DescribeConformancePackComplianceResponse)
{-# DEPRECATED dcpcrsConformancePackRuleComplianceList "Use generic-lens or generic-optics with 'conformancePackRuleComplianceList' instead." #-}

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcrsConformancePackName :: Lens.Lens' DescribeConformancePackComplianceResponse Lude.Text
dcpcrsConformancePackName = Lens.lens (conformancePackName :: DescribeConformancePackComplianceResponse -> Lude.Text) (\s a -> s {conformancePackName = a} :: DescribeConformancePackComplianceResponse)
{-# DEPRECATED dcpcrsConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcrsNextToken :: Lens.Lens' DescribeConformancePackComplianceResponse (Lude.Maybe Lude.Text)
dcpcrsNextToken = Lens.lens (nextToken :: DescribeConformancePackComplianceResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConformancePackComplianceResponse)
{-# DEPRECATED dcpcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcrsResponseStatus :: Lens.Lens' DescribeConformancePackComplianceResponse Lude.Int
dcpcrsResponseStatus = Lens.lens (responseStatus :: DescribeConformancePackComplianceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConformancePackComplianceResponse)
{-# DEPRECATED dcpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
