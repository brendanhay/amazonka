{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRightsizingRecommendation' smart constructor.
data GetRightsizingRecommendation = GetRightsizingRecommendation'
  { -- | The number of recommendations that you want returned in a single
    -- response object.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | Enables you to customize recommendations across two attributes. You can
    -- choose to view recommendations for instances within the same instance
    -- families or across different instance families. You can also choose to
    -- view your estimated savings associated with recommendations with
    -- consideration of existing Savings Plans or RI benefits, or neither.
    configuration :: Prelude.Maybe RightsizingRecommendationConfiguration,
    -- | The pagination token that indicates the next set of results that you
    -- want to retrieve.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    filter' :: Prelude.Maybe Expression,
    -- | The specific service that you want recommendations for. The only valid
    -- value for @GetRightsizingRecommendation@ is \"@AmazonEC2@\".
    service :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetRightsizingRecommendation
newGetRightsizingRecommendation pService_ =
  GetRightsizingRecommendation'
    { pageSize =
        Prelude.Nothing,
      configuration = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      filter' = Prelude.Nothing,
      service = pService_
    }

-- | The number of recommendations that you want returned in a single
-- response object.
getRightsizingRecommendation_pageSize :: Lens.Lens' GetRightsizingRecommendation (Prelude.Maybe Prelude.Natural)
getRightsizingRecommendation_pageSize = Lens.lens (\GetRightsizingRecommendation' {pageSize} -> pageSize) (\s@GetRightsizingRecommendation' {} a -> s {pageSize = a} :: GetRightsizingRecommendation)

-- | Enables you to customize recommendations across two attributes. You can
-- choose to view recommendations for instances within the same instance
-- families or across different instance families. You can also choose to
-- view your estimated savings associated with recommendations with
-- consideration of existing Savings Plans or RI benefits, or neither.
getRightsizingRecommendation_configuration :: Lens.Lens' GetRightsizingRecommendation (Prelude.Maybe RightsizingRecommendationConfiguration)
getRightsizingRecommendation_configuration = Lens.lens (\GetRightsizingRecommendation' {configuration} -> configuration) (\s@GetRightsizingRecommendation' {} a -> s {configuration = a} :: GetRightsizingRecommendation)

-- | The pagination token that indicates the next set of results that you
-- want to retrieve.
getRightsizingRecommendation_nextPageToken :: Lens.Lens' GetRightsizingRecommendation (Prelude.Maybe Prelude.Text)
getRightsizingRecommendation_nextPageToken = Lens.lens (\GetRightsizingRecommendation' {nextPageToken} -> nextPageToken) (\s@GetRightsizingRecommendation' {} a -> s {nextPageToken = a} :: GetRightsizingRecommendation)

-- | Undocumented member.
getRightsizingRecommendation_filter :: Lens.Lens' GetRightsizingRecommendation (Prelude.Maybe Expression)
getRightsizingRecommendation_filter = Lens.lens (\GetRightsizingRecommendation' {filter'} -> filter') (\s@GetRightsizingRecommendation' {} a -> s {filter' = a} :: GetRightsizingRecommendation)

-- | The specific service that you want recommendations for. The only valid
-- value for @GetRightsizingRecommendation@ is \"@AmazonEC2@\".
getRightsizingRecommendation_service :: Lens.Lens' GetRightsizingRecommendation Prelude.Text
getRightsizingRecommendation_service = Lens.lens (\GetRightsizingRecommendation' {service} -> service) (\s@GetRightsizingRecommendation' {} a -> s {service = a} :: GetRightsizingRecommendation)

instance
  Prelude.AWSRequest
    GetRightsizingRecommendation
  where
  type
    Rs GetRightsizingRecommendation =
      GetRightsizingRecommendationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRightsizingRecommendationResponse'
            Prelude.<$> (x Prelude..?> "Configuration")
            Prelude.<*> (x Prelude..?> "Metadata")
            Prelude.<*> (x Prelude..?> "NextPageToken")
            Prelude.<*> (x Prelude..?> "Summary")
            Prelude.<*> ( x Prelude..?> "RightsizingRecommendations"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRightsizingRecommendation

instance Prelude.NFData GetRightsizingRecommendation

instance
  Prelude.ToHeaders
    GetRightsizingRecommendation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSInsightsIndexService.GetRightsizingRecommendation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetRightsizingRecommendation where
  toJSON GetRightsizingRecommendation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PageSize" Prelude..=) Prelude.<$> pageSize,
            ("Configuration" Prelude..=)
              Prelude.<$> configuration,
            ("NextPageToken" Prelude..=)
              Prelude.<$> nextPageToken,
            ("Filter" Prelude..=) Prelude.<$> filter',
            Prelude.Just ("Service" Prelude..= service)
          ]
      )

instance Prelude.ToPath GetRightsizingRecommendation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetRightsizingRecommendation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRightsizingRecommendationResponse' smart constructor.
data GetRightsizingRecommendationResponse = GetRightsizingRecommendationResponse'
  { -- | Enables you to customize recommendations across two attributes. You can
    -- choose to view recommendations for instances within the same instance
    -- families or across different instance families. You can also choose to
    -- view your estimated savings associated with recommendations with
    -- consideration of existing Savings Plans or RI benefits, or neither.
    configuration :: Prelude.Maybe RightsizingRecommendationConfiguration,
    -- | Information regarding this specific recommendation set.
    metadata :: Prelude.Maybe RightsizingRecommendationMetadata,
    -- | The token to retrieve the next set of results.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Summary of this recommendation set.
    summary :: Prelude.Maybe RightsizingRecommendationSummary,
    -- | Recommendations to rightsize resources.
    rightsizingRecommendations :: Prelude.Maybe [RightsizingRecommendation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetRightsizingRecommendationResponse
newGetRightsizingRecommendationResponse pHttpStatus_ =
  GetRightsizingRecommendationResponse'
    { configuration =
        Prelude.Nothing,
      metadata = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      summary = Prelude.Nothing,
      rightsizingRecommendations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Enables you to customize recommendations across two attributes. You can
-- choose to view recommendations for instances within the same instance
-- families or across different instance families. You can also choose to
-- view your estimated savings associated with recommendations with
-- consideration of existing Savings Plans or RI benefits, or neither.
getRightsizingRecommendationResponse_configuration :: Lens.Lens' GetRightsizingRecommendationResponse (Prelude.Maybe RightsizingRecommendationConfiguration)
getRightsizingRecommendationResponse_configuration = Lens.lens (\GetRightsizingRecommendationResponse' {configuration} -> configuration) (\s@GetRightsizingRecommendationResponse' {} a -> s {configuration = a} :: GetRightsizingRecommendationResponse)

-- | Information regarding this specific recommendation set.
getRightsizingRecommendationResponse_metadata :: Lens.Lens' GetRightsizingRecommendationResponse (Prelude.Maybe RightsizingRecommendationMetadata)
getRightsizingRecommendationResponse_metadata = Lens.lens (\GetRightsizingRecommendationResponse' {metadata} -> metadata) (\s@GetRightsizingRecommendationResponse' {} a -> s {metadata = a} :: GetRightsizingRecommendationResponse)

-- | The token to retrieve the next set of results.
getRightsizingRecommendationResponse_nextPageToken :: Lens.Lens' GetRightsizingRecommendationResponse (Prelude.Maybe Prelude.Text)
getRightsizingRecommendationResponse_nextPageToken = Lens.lens (\GetRightsizingRecommendationResponse' {nextPageToken} -> nextPageToken) (\s@GetRightsizingRecommendationResponse' {} a -> s {nextPageToken = a} :: GetRightsizingRecommendationResponse)

-- | Summary of this recommendation set.
getRightsizingRecommendationResponse_summary :: Lens.Lens' GetRightsizingRecommendationResponse (Prelude.Maybe RightsizingRecommendationSummary)
getRightsizingRecommendationResponse_summary = Lens.lens (\GetRightsizingRecommendationResponse' {summary} -> summary) (\s@GetRightsizingRecommendationResponse' {} a -> s {summary = a} :: GetRightsizingRecommendationResponse)

-- | Recommendations to rightsize resources.
getRightsizingRecommendationResponse_rightsizingRecommendations :: Lens.Lens' GetRightsizingRecommendationResponse (Prelude.Maybe [RightsizingRecommendation])
getRightsizingRecommendationResponse_rightsizingRecommendations = Lens.lens (\GetRightsizingRecommendationResponse' {rightsizingRecommendations} -> rightsizingRecommendations) (\s@GetRightsizingRecommendationResponse' {} a -> s {rightsizingRecommendations = a} :: GetRightsizingRecommendationResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getRightsizingRecommendationResponse_httpStatus :: Lens.Lens' GetRightsizingRecommendationResponse Prelude.Int
getRightsizingRecommendationResponse_httpStatus = Lens.lens (\GetRightsizingRecommendationResponse' {httpStatus} -> httpStatus) (\s@GetRightsizingRecommendationResponse' {} a -> s {httpStatus = a} :: GetRightsizingRecommendationResponse)

instance
  Prelude.NFData
    GetRightsizingRecommendationResponse
