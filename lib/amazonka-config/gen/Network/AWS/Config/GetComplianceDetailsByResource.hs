{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetComplianceDetailsByResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evaluation results for the specified AWS resource. The results indicate which AWS Config rules were used to evaluate the resource, when each rule was last used, and whether the resource complies with each rule.
--
-- This operation returns paginated results.
module Network.AWS.Config.GetComplianceDetailsByResource
  ( -- * Creating a request
    GetComplianceDetailsByResource (..),
    mkGetComplianceDetailsByResource,

    -- ** Request lenses
    gcdbrComplianceTypes,
    gcdbrNextToken,
    gcdbrResourceType,
    gcdbrResourceId,

    -- * Destructuring the response
    GetComplianceDetailsByResourceResponse (..),
    mkGetComplianceDetailsByResourceResponse,

    -- ** Response lenses
    gcdbrrsEvaluationResults,
    gcdbrrsNextToken,
    gcdbrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkGetComplianceDetailsByResource' smart constructor.
data GetComplianceDetailsByResource = GetComplianceDetailsByResource'
  { complianceTypes ::
      Lude.Maybe [ComplianceType],
    nextToken ::
      Lude.Maybe Lude.Text,
    resourceType :: Lude.Text,
    resourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetComplianceDetailsByResource' with the minimum fields required to make a request.
--
-- * 'complianceTypes' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ .
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'resourceId' - The ID of the AWS resource for which you want compliance information.
-- * 'resourceType' - The type of the AWS resource for which you want compliance information.
mkGetComplianceDetailsByResource ::
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  GetComplianceDetailsByResource
mkGetComplianceDetailsByResource pResourceType_ pResourceId_ =
  GetComplianceDetailsByResource'
    { complianceTypes = Lude.Nothing,
      nextToken = Lude.Nothing,
      resourceType = pResourceType_,
      resourceId = pResourceId_
    }

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ .
--
-- /Note:/ Consider using 'complianceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrComplianceTypes :: Lens.Lens' GetComplianceDetailsByResource (Lude.Maybe [ComplianceType])
gcdbrComplianceTypes = Lens.lens (complianceTypes :: GetComplianceDetailsByResource -> Lude.Maybe [ComplianceType]) (\s a -> s {complianceTypes = a} :: GetComplianceDetailsByResource)
{-# DEPRECATED gcdbrComplianceTypes "Use generic-lens or generic-optics with 'complianceTypes' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrNextToken :: Lens.Lens' GetComplianceDetailsByResource (Lude.Maybe Lude.Text)
gcdbrNextToken = Lens.lens (nextToken :: GetComplianceDetailsByResource -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetComplianceDetailsByResource)
{-# DEPRECATED gcdbrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of the AWS resource for which you want compliance information.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrResourceType :: Lens.Lens' GetComplianceDetailsByResource Lude.Text
gcdbrResourceType = Lens.lens (resourceType :: GetComplianceDetailsByResource -> Lude.Text) (\s a -> s {resourceType = a} :: GetComplianceDetailsByResource)
{-# DEPRECATED gcdbrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the AWS resource for which you want compliance information.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrResourceId :: Lens.Lens' GetComplianceDetailsByResource Lude.Text
gcdbrResourceId = Lens.lens (resourceId :: GetComplianceDetailsByResource -> Lude.Text) (\s a -> s {resourceId = a} :: GetComplianceDetailsByResource)
{-# DEPRECATED gcdbrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Page.AWSPager GetComplianceDetailsByResource where
  page rq rs
    | Page.stop (rs Lens.^. gcdbrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gcdbrrsEvaluationResults) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gcdbrNextToken Lens..~ rs Lens.^. gcdbrrsNextToken

instance Lude.AWSRequest GetComplianceDetailsByResource where
  type
    Rs GetComplianceDetailsByResource =
      GetComplianceDetailsByResourceResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetComplianceDetailsByResourceResponse'
            Lude.<$> (x Lude..?> "EvaluationResults" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetComplianceDetailsByResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetComplianceDetailsByResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetComplianceDetailsByResource where
  toJSON GetComplianceDetailsByResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ComplianceTypes" Lude..=) Lude.<$> complianceTypes,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ResourceType" Lude..= resourceType),
            Lude.Just ("ResourceId" Lude..= resourceId)
          ]
      )

instance Lude.ToPath GetComplianceDetailsByResource where
  toPath = Lude.const "/"

instance Lude.ToQuery GetComplianceDetailsByResource where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkGetComplianceDetailsByResourceResponse' smart constructor.
data GetComplianceDetailsByResourceResponse = GetComplianceDetailsByResourceResponse'
  { evaluationResults ::
      Lude.Maybe
        [EvaluationResult],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'GetComplianceDetailsByResourceResponse' with the minimum fields required to make a request.
--
-- * 'evaluationResults' - Indicates whether the specified AWS resource complies each AWS Config rule.
-- * 'nextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkGetComplianceDetailsByResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetComplianceDetailsByResourceResponse
mkGetComplianceDetailsByResourceResponse pResponseStatus_ =
  GetComplianceDetailsByResourceResponse'
    { evaluationResults =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether the specified AWS resource complies each AWS Config rule.
--
-- /Note:/ Consider using 'evaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrrsEvaluationResults :: Lens.Lens' GetComplianceDetailsByResourceResponse (Lude.Maybe [EvaluationResult])
gcdbrrsEvaluationResults = Lens.lens (evaluationResults :: GetComplianceDetailsByResourceResponse -> Lude.Maybe [EvaluationResult]) (\s a -> s {evaluationResults = a} :: GetComplianceDetailsByResourceResponse)
{-# DEPRECATED gcdbrrsEvaluationResults "Use generic-lens or generic-optics with 'evaluationResults' instead." #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrrsNextToken :: Lens.Lens' GetComplianceDetailsByResourceResponse (Lude.Maybe Lude.Text)
gcdbrrsNextToken = Lens.lens (nextToken :: GetComplianceDetailsByResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetComplianceDetailsByResourceResponse)
{-# DEPRECATED gcdbrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrrsResponseStatus :: Lens.Lens' GetComplianceDetailsByResourceResponse Lude.Int
gcdbrrsResponseStatus = Lens.lens (responseStatus :: GetComplianceDetailsByResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetComplianceDetailsByResourceResponse)
{-# DEPRECATED gcdbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
