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
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisionedProductPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the resource changes for the specified plan.
module Network.AWS.ServiceCatalog.DescribeProvisionedProductPlan
  ( -- * Creating a Request
    DescribeProvisionedProductPlan (..),
    newDescribeProvisionedProductPlan,

    -- * Request Lenses
    describeProvisionedProductPlan_pageSize,
    describeProvisionedProductPlan_pageToken,
    describeProvisionedProductPlan_acceptLanguage,
    describeProvisionedProductPlan_planId,

    -- * Destructuring the Response
    DescribeProvisionedProductPlanResponse (..),
    newDescribeProvisionedProductPlanResponse,

    -- * Response Lenses
    describeProvisionedProductPlanResponse_resourceChanges,
    describeProvisionedProductPlanResponse_nextPageToken,
    describeProvisionedProductPlanResponse_provisionedProductPlanDetails,
    describeProvisionedProductPlanResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribeProvisionedProductPlan' smart constructor.
data DescribeProvisionedProductPlan = DescribeProvisionedProductPlan'
  { -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The plan identifier.
    planId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProvisionedProductPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'describeProvisionedProductPlan_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'describeProvisionedProductPlan_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'acceptLanguage', 'describeProvisionedProductPlan_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'planId', 'describeProvisionedProductPlan_planId' - The plan identifier.
newDescribeProvisionedProductPlan ::
  -- | 'planId'
  Prelude.Text ->
  DescribeProvisionedProductPlan
newDescribeProvisionedProductPlan pPlanId_ =
  DescribeProvisionedProductPlan'
    { pageSize =
        Prelude.Nothing,
      pageToken = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      planId = pPlanId_
    }

-- | The maximum number of items to return with this call.
describeProvisionedProductPlan_pageSize :: Lens.Lens' DescribeProvisionedProductPlan (Prelude.Maybe Prelude.Natural)
describeProvisionedProductPlan_pageSize = Lens.lens (\DescribeProvisionedProductPlan' {pageSize} -> pageSize) (\s@DescribeProvisionedProductPlan' {} a -> s {pageSize = a} :: DescribeProvisionedProductPlan)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
describeProvisionedProductPlan_pageToken :: Lens.Lens' DescribeProvisionedProductPlan (Prelude.Maybe Prelude.Text)
describeProvisionedProductPlan_pageToken = Lens.lens (\DescribeProvisionedProductPlan' {pageToken} -> pageToken) (\s@DescribeProvisionedProductPlan' {} a -> s {pageToken = a} :: DescribeProvisionedProductPlan)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProvisionedProductPlan_acceptLanguage :: Lens.Lens' DescribeProvisionedProductPlan (Prelude.Maybe Prelude.Text)
describeProvisionedProductPlan_acceptLanguage = Lens.lens (\DescribeProvisionedProductPlan' {acceptLanguage} -> acceptLanguage) (\s@DescribeProvisionedProductPlan' {} a -> s {acceptLanguage = a} :: DescribeProvisionedProductPlan)

-- | The plan identifier.
describeProvisionedProductPlan_planId :: Lens.Lens' DescribeProvisionedProductPlan Prelude.Text
describeProvisionedProductPlan_planId = Lens.lens (\DescribeProvisionedProductPlan' {planId} -> planId) (\s@DescribeProvisionedProductPlan' {} a -> s {planId = a} :: DescribeProvisionedProductPlan)

instance
  Core.AWSRequest
    DescribeProvisionedProductPlan
  where
  type
    AWSResponse DescribeProvisionedProductPlan =
      DescribeProvisionedProductPlanResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisionedProductPlanResponse'
            Prelude.<$> ( x Core..?> "ResourceChanges"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextPageToken")
            Prelude.<*> (x Core..?> "ProvisionedProductPlanDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeProvisionedProductPlan

instance
  Prelude.NFData
    DescribeProvisionedProductPlan

instance
  Core.ToHeaders
    DescribeProvisionedProductPlan
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeProvisionedProductPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeProvisionedProductPlan where
  toJSON DescribeProvisionedProductPlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PageSize" Core..=) Prelude.<$> pageSize,
            ("PageToken" Core..=) Prelude.<$> pageToken,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PlanId" Core..= planId)
          ]
      )

instance Core.ToPath DescribeProvisionedProductPlan where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeProvisionedProductPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProvisionedProductPlanResponse' smart constructor.
data DescribeProvisionedProductPlanResponse = DescribeProvisionedProductPlanResponse'
  { -- | Information about the resource changes that will occur when the plan is
    -- executed.
    resourceChanges :: Prelude.Maybe [ResourceChange],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the plan.
    provisionedProductPlanDetails :: Prelude.Maybe ProvisionedProductPlanDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProvisionedProductPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceChanges', 'describeProvisionedProductPlanResponse_resourceChanges' - Information about the resource changes that will occur when the plan is
-- executed.
--
-- 'nextPageToken', 'describeProvisionedProductPlanResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'provisionedProductPlanDetails', 'describeProvisionedProductPlanResponse_provisionedProductPlanDetails' - Information about the plan.
--
-- 'httpStatus', 'describeProvisionedProductPlanResponse_httpStatus' - The response's http status code.
newDescribeProvisionedProductPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProvisionedProductPlanResponse
newDescribeProvisionedProductPlanResponse
  pHttpStatus_ =
    DescribeProvisionedProductPlanResponse'
      { resourceChanges =
          Prelude.Nothing,
        nextPageToken = Prelude.Nothing,
        provisionedProductPlanDetails =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the resource changes that will occur when the plan is
-- executed.
describeProvisionedProductPlanResponse_resourceChanges :: Lens.Lens' DescribeProvisionedProductPlanResponse (Prelude.Maybe [ResourceChange])
describeProvisionedProductPlanResponse_resourceChanges = Lens.lens (\DescribeProvisionedProductPlanResponse' {resourceChanges} -> resourceChanges) (\s@DescribeProvisionedProductPlanResponse' {} a -> s {resourceChanges = a} :: DescribeProvisionedProductPlanResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
describeProvisionedProductPlanResponse_nextPageToken :: Lens.Lens' DescribeProvisionedProductPlanResponse (Prelude.Maybe Prelude.Text)
describeProvisionedProductPlanResponse_nextPageToken = Lens.lens (\DescribeProvisionedProductPlanResponse' {nextPageToken} -> nextPageToken) (\s@DescribeProvisionedProductPlanResponse' {} a -> s {nextPageToken = a} :: DescribeProvisionedProductPlanResponse)

-- | Information about the plan.
describeProvisionedProductPlanResponse_provisionedProductPlanDetails :: Lens.Lens' DescribeProvisionedProductPlanResponse (Prelude.Maybe ProvisionedProductPlanDetails)
describeProvisionedProductPlanResponse_provisionedProductPlanDetails = Lens.lens (\DescribeProvisionedProductPlanResponse' {provisionedProductPlanDetails} -> provisionedProductPlanDetails) (\s@DescribeProvisionedProductPlanResponse' {} a -> s {provisionedProductPlanDetails = a} :: DescribeProvisionedProductPlanResponse)

-- | The response's http status code.
describeProvisionedProductPlanResponse_httpStatus :: Lens.Lens' DescribeProvisionedProductPlanResponse Prelude.Int
describeProvisionedProductPlanResponse_httpStatus = Lens.lens (\DescribeProvisionedProductPlanResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisionedProductPlanResponse' {} a -> s {httpStatus = a} :: DescribeProvisionedProductPlanResponse)

instance
  Prelude.NFData
    DescribeProvisionedProductPlanResponse
