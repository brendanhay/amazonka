{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetRightsizingRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates recommendations that help you save cost by identifying idle and underutilized Amazon EC2 instances.
--
-- Recommendations are generated to either downsize or terminate instances, along with providing savings detail and metrics. For details on calculation and function, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/ce-rightsizing.html Optimizing Your Cost with Rightsizing Recommendations> in the /AWS Billing and Cost Management User Guide/ .
module Network.AWS.CostExplorer.GetRightsizingRecommendation
  ( -- * Creating a request
    GetRightsizingRecommendation (..),
    mkGetRightsizingRecommendation,

    -- ** Request lenses
    grrNextPageToken,
    grrConfiguration,
    grrFilter,
    grrPageSize,
    grrService,

    -- * Destructuring the response
    GetRightsizingRecommendationResponse (..),
    mkGetRightsizingRecommendationResponse,

    -- ** Response lenses
    grrrsSummary,
    grrrsNextPageToken,
    grrrsRightsizingRecommendations,
    grrrsMetadata,
    grrrsConfiguration,
    grrrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRightsizingRecommendation' smart constructor.
data GetRightsizingRecommendation = GetRightsizingRecommendation'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    configuration ::
      Lude.Maybe
        RightsizingRecommendationConfiguration,
    filter :: Lude.Maybe Expression,
    pageSize ::
      Lude.Maybe Lude.Natural,
    service :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRightsizingRecommendation' with the minimum fields required to make a request.
--
-- * 'configuration' - Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
-- * 'filter' - Undocumented field.
-- * 'nextPageToken' - The pagination token that indicates the next set of results that you want to retrieve.
-- * 'pageSize' - The number of recommendations that you want returned in a single response object.
-- * 'service' - The specific service that you want recommendations for. The only valid value for @GetRightsizingRecommendation@ is "@AmazonEC2@ ".
mkGetRightsizingRecommendation ::
  -- | 'service'
  Lude.Text ->
  GetRightsizingRecommendation
mkGetRightsizingRecommendation pService_ =
  GetRightsizingRecommendation'
    { nextPageToken = Lude.Nothing,
      configuration = Lude.Nothing,
      filter = Lude.Nothing,
      pageSize = Lude.Nothing,
      service = pService_
    }

-- | The pagination token that indicates the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrNextPageToken :: Lens.Lens' GetRightsizingRecommendation (Lude.Maybe Lude.Text)
grrNextPageToken = Lens.lens (nextPageToken :: GetRightsizingRecommendation -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetRightsizingRecommendation)
{-# DEPRECATED grrNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrConfiguration :: Lens.Lens' GetRightsizingRecommendation (Lude.Maybe RightsizingRecommendationConfiguration)
grrConfiguration = Lens.lens (configuration :: GetRightsizingRecommendation -> Lude.Maybe RightsizingRecommendationConfiguration) (\s a -> s {configuration = a} :: GetRightsizingRecommendation)
{-# DEPRECATED grrConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrFilter :: Lens.Lens' GetRightsizingRecommendation (Lude.Maybe Expression)
grrFilter = Lens.lens (filter :: GetRightsizingRecommendation -> Lude.Maybe Expression) (\s a -> s {filter = a} :: GetRightsizingRecommendation)
{-# DEPRECATED grrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The number of recommendations that you want returned in a single response object.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrPageSize :: Lens.Lens' GetRightsizingRecommendation (Lude.Maybe Lude.Natural)
grrPageSize = Lens.lens (pageSize :: GetRightsizingRecommendation -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: GetRightsizingRecommendation)
{-# DEPRECATED grrPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The specific service that you want recommendations for. The only valid value for @GetRightsizingRecommendation@ is "@AmazonEC2@ ".
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrService :: Lens.Lens' GetRightsizingRecommendation Lude.Text
grrService = Lens.lens (service :: GetRightsizingRecommendation -> Lude.Text) (\s a -> s {service = a} :: GetRightsizingRecommendation)
{-# DEPRECATED grrService "Use generic-lens or generic-optics with 'service' instead." #-}

instance Lude.AWSRequest GetRightsizingRecommendation where
  type
    Rs GetRightsizingRecommendation =
      GetRightsizingRecommendationResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRightsizingRecommendationResponse'
            Lude.<$> (x Lude..?> "Summary")
            Lude.<*> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "RightsizingRecommendations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Metadata")
            Lude.<*> (x Lude..?> "Configuration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRightsizingRecommendation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.GetRightsizingRecommendation" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRightsizingRecommendation where
  toJSON GetRightsizingRecommendation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("Configuration" Lude..=) Lude.<$> configuration,
            ("Filter" Lude..=) Lude.<$> filter,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            Lude.Just ("Service" Lude..= service)
          ]
      )

instance Lude.ToPath GetRightsizingRecommendation where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRightsizingRecommendation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRightsizingRecommendationResponse' smart constructor.
data GetRightsizingRecommendationResponse = GetRightsizingRecommendationResponse'
  { summary ::
      Lude.Maybe
        RightsizingRecommendationSummary,
    nextPageToken ::
      Lude.Maybe
        Lude.Text,
    rightsizingRecommendations ::
      Lude.Maybe
        [RightsizingRecommendation],
    metadata ::
      Lude.Maybe
        RightsizingRecommendationMetadata,
    configuration ::
      Lude.Maybe
        RightsizingRecommendationConfiguration,
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

-- | Creates a value of 'GetRightsizingRecommendationResponse' with the minimum fields required to make a request.
--
-- * 'configuration' - Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
-- * 'metadata' - Information regarding this specific recommendation set.
-- * 'nextPageToken' - The token to retrieve the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'rightsizingRecommendations' - Recommendations to rightsize resources.
-- * 'summary' - Summary of this recommendation set.
mkGetRightsizingRecommendationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRightsizingRecommendationResponse
mkGetRightsizingRecommendationResponse pResponseStatus_ =
  GetRightsizingRecommendationResponse'
    { summary = Lude.Nothing,
      nextPageToken = Lude.Nothing,
      rightsizingRecommendations = Lude.Nothing,
      metadata = Lude.Nothing,
      configuration = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Summary of this recommendation set.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsSummary :: Lens.Lens' GetRightsizingRecommendationResponse (Lude.Maybe RightsizingRecommendationSummary)
grrrsSummary = Lens.lens (summary :: GetRightsizingRecommendationResponse -> Lude.Maybe RightsizingRecommendationSummary) (\s a -> s {summary = a} :: GetRightsizingRecommendationResponse)
{-# DEPRECATED grrrsSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsNextPageToken :: Lens.Lens' GetRightsizingRecommendationResponse (Lude.Maybe Lude.Text)
grrrsNextPageToken = Lens.lens (nextPageToken :: GetRightsizingRecommendationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetRightsizingRecommendationResponse)
{-# DEPRECATED grrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Recommendations to rightsize resources.
--
-- /Note:/ Consider using 'rightsizingRecommendations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsRightsizingRecommendations :: Lens.Lens' GetRightsizingRecommendationResponse (Lude.Maybe [RightsizingRecommendation])
grrrsRightsizingRecommendations = Lens.lens (rightsizingRecommendations :: GetRightsizingRecommendationResponse -> Lude.Maybe [RightsizingRecommendation]) (\s a -> s {rightsizingRecommendations = a} :: GetRightsizingRecommendationResponse)
{-# DEPRECATED grrrsRightsizingRecommendations "Use generic-lens or generic-optics with 'rightsizingRecommendations' instead." #-}

-- | Information regarding this specific recommendation set.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsMetadata :: Lens.Lens' GetRightsizingRecommendationResponse (Lude.Maybe RightsizingRecommendationMetadata)
grrrsMetadata = Lens.lens (metadata :: GetRightsizingRecommendationResponse -> Lude.Maybe RightsizingRecommendationMetadata) (\s a -> s {metadata = a} :: GetRightsizingRecommendationResponse)
{-# DEPRECATED grrrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsConfiguration :: Lens.Lens' GetRightsizingRecommendationResponse (Lude.Maybe RightsizingRecommendationConfiguration)
grrrsConfiguration = Lens.lens (configuration :: GetRightsizingRecommendationResponse -> Lude.Maybe RightsizingRecommendationConfiguration) (\s a -> s {configuration = a} :: GetRightsizingRecommendationResponse)
{-# DEPRECATED grrrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetRightsizingRecommendationResponse Lude.Int
grrrsResponseStatus = Lens.lens (responseStatus :: GetRightsizingRecommendationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRightsizingRecommendationResponse)
{-# DEPRECATED grrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
