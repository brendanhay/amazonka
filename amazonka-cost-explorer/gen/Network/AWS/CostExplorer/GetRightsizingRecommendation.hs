{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetRightsizingRecommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates recommendations that help you save cost by identifying idle and
-- underutilized Amazon EC2 instances.
--
-- Recommendations are generated to either downsize or terminate instances,
-- along with providing savings detail and metrics. For details on
-- calculation and function, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/ce-rightsizing.html Optimizing Your Cost with Rightsizing Recommendations>
-- in the /AWS Billing and Cost Management User Guide/.
module Network.AWS.CostExplorer.GetRightsizingRecommendation
  ( -- * Creating a Request
    GetRightsizingRecommendation (..),
    newGetRightsizingRecommendation,

    -- * Request Lenses
    getRightsizingRecommendation_pageSize,
    getRightsizingRecommendation_configuration,
    getRightsizingRecommendation_nextPageToken,
    getRightsizingRecommendation_filter,
    getRightsizingRecommendation_service,

    -- * Destructuring the Response
    GetRightsizingRecommendationResponse (..),
    newGetRightsizingRecommendationResponse,

    -- * Response Lenses
    getRightsizingRecommendationResponse_configuration,
    getRightsizingRecommendationResponse_metadata,
    getRightsizingRecommendationResponse_nextPageToken,
    getRightsizingRecommendationResponse_summary,
    getRightsizingRecommendationResponse_rightsizingRecommendations,
    getRightsizingRecommendationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRightsizingRecommendation' smart constructor.
data GetRightsizingRecommendation = GetRightsizingRecommendation'
  { -- | The number of recommendations that you want returned in a single
    -- response object.
    pageSize :: Core.Maybe Core.Natural,
    -- | Enables you to customize recommendations across two attributes. You can
    -- choose to view recommendations for instances within the same instance
    -- families or across different instance families. You can also choose to
    -- view your estimated savings associated with recommendations with
    -- consideration of existing Savings Plans or RI benefits, or neither.
    configuration :: Core.Maybe RightsizingRecommendationConfiguration,
    -- | The pagination token that indicates the next set of results that you
    -- want to retrieve.
    nextPageToken :: Core.Maybe Core.Text,
    filter' :: Core.Maybe Expression,
    -- | The specific service that you want recommendations for. The only valid
    -- value for @GetRightsizingRecommendation@ is \"@AmazonEC2@\".
    service :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRightsizingRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getRightsizingRecommendation_pageSize' - The number of recommendations that you want returned in a single
-- response object.
--
-- 'configuration', 'getRightsizingRecommendation_configuration' - Enables you to customize recommendations across two attributes. You can
-- choose to view recommendations for instances within the same instance
-- families or across different instance families. You can also choose to
-- view your estimated savings associated with recommendations with
-- consideration of existing Savings Plans or RI benefits, or neither.
--
-- 'nextPageToken', 'getRightsizingRecommendation_nextPageToken' - The pagination token that indicates the next set of results that you
-- want to retrieve.
--
-- 'filter'', 'getRightsizingRecommendation_filter' - Undocumented member.
--
-- 'service', 'getRightsizingRecommendation_service' - The specific service that you want recommendations for. The only valid
-- value for @GetRightsizingRecommendation@ is \"@AmazonEC2@\".
newGetRightsizingRecommendation ::
  -- | 'service'
  Core.Text ->
  GetRightsizingRecommendation
newGetRightsizingRecommendation pService_ =
  GetRightsizingRecommendation'
    { pageSize =
        Core.Nothing,
      configuration = Core.Nothing,
      nextPageToken = Core.Nothing,
      filter' = Core.Nothing,
      service = pService_
    }

-- | The number of recommendations that you want returned in a single
-- response object.
getRightsizingRecommendation_pageSize :: Lens.Lens' GetRightsizingRecommendation (Core.Maybe Core.Natural)
getRightsizingRecommendation_pageSize = Lens.lens (\GetRightsizingRecommendation' {pageSize} -> pageSize) (\s@GetRightsizingRecommendation' {} a -> s {pageSize = a} :: GetRightsizingRecommendation)

-- | Enables you to customize recommendations across two attributes. You can
-- choose to view recommendations for instances within the same instance
-- families or across different instance families. You can also choose to
-- view your estimated savings associated with recommendations with
-- consideration of existing Savings Plans or RI benefits, or neither.
getRightsizingRecommendation_configuration :: Lens.Lens' GetRightsizingRecommendation (Core.Maybe RightsizingRecommendationConfiguration)
getRightsizingRecommendation_configuration = Lens.lens (\GetRightsizingRecommendation' {configuration} -> configuration) (\s@GetRightsizingRecommendation' {} a -> s {configuration = a} :: GetRightsizingRecommendation)

-- | The pagination token that indicates the next set of results that you
-- want to retrieve.
getRightsizingRecommendation_nextPageToken :: Lens.Lens' GetRightsizingRecommendation (Core.Maybe Core.Text)
getRightsizingRecommendation_nextPageToken = Lens.lens (\GetRightsizingRecommendation' {nextPageToken} -> nextPageToken) (\s@GetRightsizingRecommendation' {} a -> s {nextPageToken = a} :: GetRightsizingRecommendation)

-- | Undocumented member.
getRightsizingRecommendation_filter :: Lens.Lens' GetRightsizingRecommendation (Core.Maybe Expression)
getRightsizingRecommendation_filter = Lens.lens (\GetRightsizingRecommendation' {filter'} -> filter') (\s@GetRightsizingRecommendation' {} a -> s {filter' = a} :: GetRightsizingRecommendation)

-- | The specific service that you want recommendations for. The only valid
-- value for @GetRightsizingRecommendation@ is \"@AmazonEC2@\".
getRightsizingRecommendation_service :: Lens.Lens' GetRightsizingRecommendation Core.Text
getRightsizingRecommendation_service = Lens.lens (\GetRightsizingRecommendation' {service} -> service) (\s@GetRightsizingRecommendation' {} a -> s {service = a} :: GetRightsizingRecommendation)

instance Core.AWSRequest GetRightsizingRecommendation where
  type
    AWSResponse GetRightsizingRecommendation =
      GetRightsizingRecommendationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRightsizingRecommendationResponse'
            Core.<$> (x Core..?> "Configuration")
            Core.<*> (x Core..?> "Metadata")
            Core.<*> (x Core..?> "NextPageToken")
            Core.<*> (x Core..?> "Summary")
            Core.<*> ( x Core..?> "RightsizingRecommendations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRightsizingRecommendation

instance Core.NFData GetRightsizingRecommendation

instance Core.ToHeaders GetRightsizingRecommendation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetRightsizingRecommendation" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRightsizingRecommendation where
  toJSON GetRightsizingRecommendation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("Configuration" Core..=) Core.<$> configuration,
            ("NextPageToken" Core..=) Core.<$> nextPageToken,
            ("Filter" Core..=) Core.<$> filter',
            Core.Just ("Service" Core..= service)
          ]
      )

instance Core.ToPath GetRightsizingRecommendation where
  toPath = Core.const "/"

instance Core.ToQuery GetRightsizingRecommendation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRightsizingRecommendationResponse' smart constructor.
data GetRightsizingRecommendationResponse = GetRightsizingRecommendationResponse'
  { -- | Enables you to customize recommendations across two attributes. You can
    -- choose to view recommendations for instances within the same instance
    -- families or across different instance families. You can also choose to
    -- view your estimated savings associated with recommendations with
    -- consideration of existing Savings Plans or RI benefits, or neither.
    configuration :: Core.Maybe RightsizingRecommendationConfiguration,
    -- | Information regarding this specific recommendation set.
    metadata :: Core.Maybe RightsizingRecommendationMetadata,
    -- | The token to retrieve the next set of results.
    nextPageToken :: Core.Maybe Core.Text,
    -- | Summary of this recommendation set.
    summary :: Core.Maybe RightsizingRecommendationSummary,
    -- | Recommendations to rightsize resources.
    rightsizingRecommendations :: Core.Maybe [RightsizingRecommendation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRightsizingRecommendationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'getRightsizingRecommendationResponse_configuration' - Enables you to customize recommendations across two attributes. You can
-- choose to view recommendations for instances within the same instance
-- families or across different instance families. You can also choose to
-- view your estimated savings associated with recommendations with
-- consideration of existing Savings Plans or RI benefits, or neither.
--
-- 'metadata', 'getRightsizingRecommendationResponse_metadata' - Information regarding this specific recommendation set.
--
-- 'nextPageToken', 'getRightsizingRecommendationResponse_nextPageToken' - The token to retrieve the next set of results.
--
-- 'summary', 'getRightsizingRecommendationResponse_summary' - Summary of this recommendation set.
--
-- 'rightsizingRecommendations', 'getRightsizingRecommendationResponse_rightsizingRecommendations' - Recommendations to rightsize resources.
--
-- 'httpStatus', 'getRightsizingRecommendationResponse_httpStatus' - The response's http status code.
newGetRightsizingRecommendationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRightsizingRecommendationResponse
newGetRightsizingRecommendationResponse pHttpStatus_ =
  GetRightsizingRecommendationResponse'
    { configuration =
        Core.Nothing,
      metadata = Core.Nothing,
      nextPageToken = Core.Nothing,
      summary = Core.Nothing,
      rightsizingRecommendations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Enables you to customize recommendations across two attributes. You can
-- choose to view recommendations for instances within the same instance
-- families or across different instance families. You can also choose to
-- view your estimated savings associated with recommendations with
-- consideration of existing Savings Plans or RI benefits, or neither.
getRightsizingRecommendationResponse_configuration :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe RightsizingRecommendationConfiguration)
getRightsizingRecommendationResponse_configuration = Lens.lens (\GetRightsizingRecommendationResponse' {configuration} -> configuration) (\s@GetRightsizingRecommendationResponse' {} a -> s {configuration = a} :: GetRightsizingRecommendationResponse)

-- | Information regarding this specific recommendation set.
getRightsizingRecommendationResponse_metadata :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe RightsizingRecommendationMetadata)
getRightsizingRecommendationResponse_metadata = Lens.lens (\GetRightsizingRecommendationResponse' {metadata} -> metadata) (\s@GetRightsizingRecommendationResponse' {} a -> s {metadata = a} :: GetRightsizingRecommendationResponse)

-- | The token to retrieve the next set of results.
getRightsizingRecommendationResponse_nextPageToken :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe Core.Text)
getRightsizingRecommendationResponse_nextPageToken = Lens.lens (\GetRightsizingRecommendationResponse' {nextPageToken} -> nextPageToken) (\s@GetRightsizingRecommendationResponse' {} a -> s {nextPageToken = a} :: GetRightsizingRecommendationResponse)

-- | Summary of this recommendation set.
getRightsizingRecommendationResponse_summary :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe RightsizingRecommendationSummary)
getRightsizingRecommendationResponse_summary = Lens.lens (\GetRightsizingRecommendationResponse' {summary} -> summary) (\s@GetRightsizingRecommendationResponse' {} a -> s {summary = a} :: GetRightsizingRecommendationResponse)

-- | Recommendations to rightsize resources.
getRightsizingRecommendationResponse_rightsizingRecommendations :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe [RightsizingRecommendation])
getRightsizingRecommendationResponse_rightsizingRecommendations = Lens.lens (\GetRightsizingRecommendationResponse' {rightsizingRecommendations} -> rightsizingRecommendations) (\s@GetRightsizingRecommendationResponse' {} a -> s {rightsizingRecommendations = a} :: GetRightsizingRecommendationResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getRightsizingRecommendationResponse_httpStatus :: Lens.Lens' GetRightsizingRecommendationResponse Core.Int
getRightsizingRecommendationResponse_httpStatus = Lens.lens (\GetRightsizingRecommendationResponse' {httpStatus} -> httpStatus) (\s@GetRightsizingRecommendationResponse' {} a -> s {httpStatus = a} :: GetRightsizingRecommendationResponse)

instance
  Core.NFData
    GetRightsizingRecommendationResponse
