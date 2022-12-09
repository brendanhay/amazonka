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
-- Module      : Amazonka.ServiceCatalog.DescribeProvisionedProductPlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the resource changes for the specified plan.
module Amazonka.ServiceCatalog.DescribeProvisionedProductPlan
  ( -- * Creating a Request
    DescribeProvisionedProductPlan (..),
    newDescribeProvisionedProductPlan,

    -- * Request Lenses
    describeProvisionedProductPlan_acceptLanguage,
    describeProvisionedProductPlan_pageSize,
    describeProvisionedProductPlan_pageToken,
    describeProvisionedProductPlan_planId,

    -- * Destructuring the Response
    DescribeProvisionedProductPlanResponse (..),
    newDescribeProvisionedProductPlanResponse,

    -- * Response Lenses
    describeProvisionedProductPlanResponse_nextPageToken,
    describeProvisionedProductPlanResponse_provisionedProductPlanDetails,
    describeProvisionedProductPlanResponse_resourceChanges,
    describeProvisionedProductPlanResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribeProvisionedProductPlan' smart constructor.
data DescribeProvisionedProductPlan = DescribeProvisionedProductPlan'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
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
-- 'acceptLanguage', 'describeProvisionedProductPlan_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'pageSize', 'describeProvisionedProductPlan_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'describeProvisionedProductPlan_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'planId', 'describeProvisionedProductPlan_planId' - The plan identifier.
newDescribeProvisionedProductPlan ::
  -- | 'planId'
  Prelude.Text ->
  DescribeProvisionedProductPlan
newDescribeProvisionedProductPlan pPlanId_ =
  DescribeProvisionedProductPlan'
    { acceptLanguage =
        Prelude.Nothing,
      pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      planId = pPlanId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProvisionedProductPlan_acceptLanguage :: Lens.Lens' DescribeProvisionedProductPlan (Prelude.Maybe Prelude.Text)
describeProvisionedProductPlan_acceptLanguage = Lens.lens (\DescribeProvisionedProductPlan' {acceptLanguage} -> acceptLanguage) (\s@DescribeProvisionedProductPlan' {} a -> s {acceptLanguage = a} :: DescribeProvisionedProductPlan)

-- | The maximum number of items to return with this call.
describeProvisionedProductPlan_pageSize :: Lens.Lens' DescribeProvisionedProductPlan (Prelude.Maybe Prelude.Natural)
describeProvisionedProductPlan_pageSize = Lens.lens (\DescribeProvisionedProductPlan' {pageSize} -> pageSize) (\s@DescribeProvisionedProductPlan' {} a -> s {pageSize = a} :: DescribeProvisionedProductPlan)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
describeProvisionedProductPlan_pageToken :: Lens.Lens' DescribeProvisionedProductPlan (Prelude.Maybe Prelude.Text)
describeProvisionedProductPlan_pageToken = Lens.lens (\DescribeProvisionedProductPlan' {pageToken} -> pageToken) (\s@DescribeProvisionedProductPlan' {} a -> s {pageToken = a} :: DescribeProvisionedProductPlan)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisionedProductPlanResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> (x Data..?> "ProvisionedProductPlanDetails")
            Prelude.<*> ( x Data..?> "ResourceChanges"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeProvisionedProductPlan
  where
  hashWithSalt
    _salt
    DescribeProvisionedProductPlan' {..} =
      _salt `Prelude.hashWithSalt` acceptLanguage
        `Prelude.hashWithSalt` pageSize
        `Prelude.hashWithSalt` pageToken
        `Prelude.hashWithSalt` planId

instance
  Prelude.NFData
    DescribeProvisionedProductPlan
  where
  rnf DescribeProvisionedProductPlan' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf planId

instance
  Data.ToHeaders
    DescribeProvisionedProductPlan
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DescribeProvisionedProductPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeProvisionedProductPlan where
  toJSON DescribeProvisionedProductPlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("PageToken" Data..=) Prelude.<$> pageToken,
            Prelude.Just ("PlanId" Data..= planId)
          ]
      )

instance Data.ToPath DescribeProvisionedProductPlan where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeProvisionedProductPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProvisionedProductPlanResponse' smart constructor.
data DescribeProvisionedProductPlanResponse = DescribeProvisionedProductPlanResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the plan.
    provisionedProductPlanDetails :: Prelude.Maybe ProvisionedProductPlanDetails,
    -- | Information about the resource changes that will occur when the plan is
    -- executed.
    resourceChanges :: Prelude.Maybe [ResourceChange],
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
-- 'nextPageToken', 'describeProvisionedProductPlanResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'provisionedProductPlanDetails', 'describeProvisionedProductPlanResponse_provisionedProductPlanDetails' - Information about the plan.
--
-- 'resourceChanges', 'describeProvisionedProductPlanResponse_resourceChanges' - Information about the resource changes that will occur when the plan is
-- executed.
--
-- 'httpStatus', 'describeProvisionedProductPlanResponse_httpStatus' - The response's http status code.
newDescribeProvisionedProductPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProvisionedProductPlanResponse
newDescribeProvisionedProductPlanResponse
  pHttpStatus_ =
    DescribeProvisionedProductPlanResponse'
      { nextPageToken =
          Prelude.Nothing,
        provisionedProductPlanDetails =
          Prelude.Nothing,
        resourceChanges = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
describeProvisionedProductPlanResponse_nextPageToken :: Lens.Lens' DescribeProvisionedProductPlanResponse (Prelude.Maybe Prelude.Text)
describeProvisionedProductPlanResponse_nextPageToken = Lens.lens (\DescribeProvisionedProductPlanResponse' {nextPageToken} -> nextPageToken) (\s@DescribeProvisionedProductPlanResponse' {} a -> s {nextPageToken = a} :: DescribeProvisionedProductPlanResponse)

-- | Information about the plan.
describeProvisionedProductPlanResponse_provisionedProductPlanDetails :: Lens.Lens' DescribeProvisionedProductPlanResponse (Prelude.Maybe ProvisionedProductPlanDetails)
describeProvisionedProductPlanResponse_provisionedProductPlanDetails = Lens.lens (\DescribeProvisionedProductPlanResponse' {provisionedProductPlanDetails} -> provisionedProductPlanDetails) (\s@DescribeProvisionedProductPlanResponse' {} a -> s {provisionedProductPlanDetails = a} :: DescribeProvisionedProductPlanResponse)

-- | Information about the resource changes that will occur when the plan is
-- executed.
describeProvisionedProductPlanResponse_resourceChanges :: Lens.Lens' DescribeProvisionedProductPlanResponse (Prelude.Maybe [ResourceChange])
describeProvisionedProductPlanResponse_resourceChanges = Lens.lens (\DescribeProvisionedProductPlanResponse' {resourceChanges} -> resourceChanges) (\s@DescribeProvisionedProductPlanResponse' {} a -> s {resourceChanges = a} :: DescribeProvisionedProductPlanResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeProvisionedProductPlanResponse_httpStatus :: Lens.Lens' DescribeProvisionedProductPlanResponse Prelude.Int
describeProvisionedProductPlanResponse_httpStatus = Lens.lens (\DescribeProvisionedProductPlanResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisionedProductPlanResponse' {} a -> s {httpStatus = a} :: DescribeProvisionedProductPlanResponse)

instance
  Prelude.NFData
    DescribeProvisionedProductPlanResponse
  where
  rnf DescribeProvisionedProductPlanResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf provisionedProductPlanDetails
      `Prelude.seq` Prelude.rnf resourceChanges
      `Prelude.seq` Prelude.rnf httpStatus
