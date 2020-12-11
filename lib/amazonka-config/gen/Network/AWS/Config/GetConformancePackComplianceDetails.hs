{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetConformancePackComplianceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details of a conformance pack for all AWS resources that are monitered by conformance pack.
module Network.AWS.Config.GetConformancePackComplianceDetails
  ( -- * Creating a request
    GetConformancePackComplianceDetails (..),
    mkGetConformancePackComplianceDetails,

    -- ** Request lenses
    gcpcdFilters,
    gcpcdNextToken,
    gcpcdLimit,
    gcpcdConformancePackName,

    -- * Destructuring the response
    GetConformancePackComplianceDetailsResponse (..),
    mkGetConformancePackComplianceDetailsResponse,

    -- ** Response lenses
    gcpcdrsNextToken,
    gcpcdrsConformancePackRuleEvaluationResults,
    gcpcdrsResponseStatus,
    gcpcdrsConformancePackName,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConformancePackComplianceDetails' smart constructor.
data GetConformancePackComplianceDetails = GetConformancePackComplianceDetails'
  { filters ::
      Lude.Maybe
        ConformancePackEvaluationFilters,
    nextToken ::
      Lude.Maybe
        Lude.Text,
    limit ::
      Lude.Maybe
        Lude.Natural,
    conformancePackName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConformancePackComplianceDetails' with the minimum fields required to make a request.
--
-- * 'conformancePackName' - Name of the conformance pack.
-- * 'filters' - A @ConformancePackEvaluationFilters@ object.
-- * 'limit' - The maximum number of evaluation results returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
-- * 'nextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
mkGetConformancePackComplianceDetails ::
  -- | 'conformancePackName'
  Lude.Text ->
  GetConformancePackComplianceDetails
mkGetConformancePackComplianceDetails pConformancePackName_ =
  GetConformancePackComplianceDetails'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      conformancePackName = pConformancePackName_
    }

-- | A @ConformancePackEvaluationFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdFilters :: Lens.Lens' GetConformancePackComplianceDetails (Lude.Maybe ConformancePackEvaluationFilters)
gcpcdFilters = Lens.lens (filters :: GetConformancePackComplianceDetails -> Lude.Maybe ConformancePackEvaluationFilters) (\s a -> s {filters = a} :: GetConformancePackComplianceDetails)
{-# DEPRECATED gcpcdFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdNextToken :: Lens.Lens' GetConformancePackComplianceDetails (Lude.Maybe Lude.Text)
gcpcdNextToken = Lens.lens (nextToken :: GetConformancePackComplianceDetails -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetConformancePackComplianceDetails)
{-# DEPRECATED gcpcdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of evaluation results returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdLimit :: Lens.Lens' GetConformancePackComplianceDetails (Lude.Maybe Lude.Natural)
gcpcdLimit = Lens.lens (limit :: GetConformancePackComplianceDetails -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetConformancePackComplianceDetails)
{-# DEPRECATED gcpcdLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdConformancePackName :: Lens.Lens' GetConformancePackComplianceDetails Lude.Text
gcpcdConformancePackName = Lens.lens (conformancePackName :: GetConformancePackComplianceDetails -> Lude.Text) (\s a -> s {conformancePackName = a} :: GetConformancePackComplianceDetails)
{-# DEPRECATED gcpcdConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

instance Lude.AWSRequest GetConformancePackComplianceDetails where
  type
    Rs GetConformancePackComplianceDetails =
      GetConformancePackComplianceDetailsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConformancePackComplianceDetailsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> ( x Lude..?> "ConformancePackRuleEvaluationResults"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "ConformancePackName")
      )

instance Lude.ToHeaders GetConformancePackComplianceDetails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetConformancePackComplianceDetails" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetConformancePackComplianceDetails where
  toJSON GetConformancePackComplianceDetails' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("ConformancePackName" Lude..= conformancePackName)
          ]
      )

instance Lude.ToPath GetConformancePackComplianceDetails where
  toPath = Lude.const "/"

instance Lude.ToQuery GetConformancePackComplianceDetails where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetConformancePackComplianceDetailsResponse' smart constructor.
data GetConformancePackComplianceDetailsResponse = GetConformancePackComplianceDetailsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    conformancePackRuleEvaluationResults ::
      Lude.Maybe
        [ConformancePackEvaluationResult],
    responseStatus ::
      Lude.Int,
    conformancePackName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConformancePackComplianceDetailsResponse' with the minimum fields required to make a request.
--
-- * 'conformancePackName' - Name of the conformance pack.
-- * 'conformancePackRuleEvaluationResults' - Returns a list of @ConformancePackEvaluationResult@ objects.
-- * 'nextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkGetConformancePackComplianceDetailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'conformancePackName'
  Lude.Text ->
  GetConformancePackComplianceDetailsResponse
mkGetConformancePackComplianceDetailsResponse
  pResponseStatus_
  pConformancePackName_ =
    GetConformancePackComplianceDetailsResponse'
      { nextToken =
          Lude.Nothing,
        conformancePackRuleEvaluationResults =
          Lude.Nothing,
        responseStatus = pResponseStatus_,
        conformancePackName = pConformancePackName_
      }

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdrsNextToken :: Lens.Lens' GetConformancePackComplianceDetailsResponse (Lude.Maybe Lude.Text)
gcpcdrsNextToken = Lens.lens (nextToken :: GetConformancePackComplianceDetailsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetConformancePackComplianceDetailsResponse)
{-# DEPRECATED gcpcdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a list of @ConformancePackEvaluationResult@ objects.
--
-- /Note:/ Consider using 'conformancePackRuleEvaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdrsConformancePackRuleEvaluationResults :: Lens.Lens' GetConformancePackComplianceDetailsResponse (Lude.Maybe [ConformancePackEvaluationResult])
gcpcdrsConformancePackRuleEvaluationResults = Lens.lens (conformancePackRuleEvaluationResults :: GetConformancePackComplianceDetailsResponse -> Lude.Maybe [ConformancePackEvaluationResult]) (\s a -> s {conformancePackRuleEvaluationResults = a} :: GetConformancePackComplianceDetailsResponse)
{-# DEPRECATED gcpcdrsConformancePackRuleEvaluationResults "Use generic-lens or generic-optics with 'conformancePackRuleEvaluationResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdrsResponseStatus :: Lens.Lens' GetConformancePackComplianceDetailsResponse Lude.Int
gcpcdrsResponseStatus = Lens.lens (responseStatus :: GetConformancePackComplianceDetailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConformancePackComplianceDetailsResponse)
{-# DEPRECATED gcpcdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdrsConformancePackName :: Lens.Lens' GetConformancePackComplianceDetailsResponse Lude.Text
gcpcdrsConformancePackName = Lens.lens (conformancePackName :: GetConformancePackComplianceDetailsResponse -> Lude.Text) (\s a -> s {conformancePackName = a} :: GetConformancePackComplianceDetailsResponse)
{-# DEPRECATED gcpcdrsConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}
