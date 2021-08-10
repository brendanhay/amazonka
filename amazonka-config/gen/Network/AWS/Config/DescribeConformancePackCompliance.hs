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
-- Module      : Network.AWS.Config.DescribeConformancePackCompliance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details for each rule in that conformance pack.
--
-- You must provide exact rule names.
module Network.AWS.Config.DescribeConformancePackCompliance
  ( -- * Creating a Request
    DescribeConformancePackCompliance (..),
    newDescribeConformancePackCompliance,

    -- * Request Lenses
    describeConformancePackCompliance_nextToken,
    describeConformancePackCompliance_filters,
    describeConformancePackCompliance_limit,
    describeConformancePackCompliance_conformancePackName,

    -- * Destructuring the Response
    DescribeConformancePackComplianceResponse (..),
    newDescribeConformancePackComplianceResponse,

    -- * Response Lenses
    describeConformancePackComplianceResponse_nextToken,
    describeConformancePackComplianceResponse_httpStatus,
    describeConformancePackComplianceResponse_conformancePackName,
    describeConformancePackComplianceResponse_conformancePackRuleComplianceList,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeConformancePackCompliance' smart constructor.
data DescribeConformancePackCompliance = DescribeConformancePackCompliance'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A @ConformancePackComplianceFilters@ object.
    filters :: Prelude.Maybe ConformancePackComplianceFilters,
    -- | The maximum number of AWS Config rules within a conformance pack are
    -- returned on each page.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Name of the conformance pack.
    conformancePackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConformancePackCompliance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConformancePackCompliance_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'filters', 'describeConformancePackCompliance_filters' - A @ConformancePackComplianceFilters@ object.
--
-- 'limit', 'describeConformancePackCompliance_limit' - The maximum number of AWS Config rules within a conformance pack are
-- returned on each page.
--
-- 'conformancePackName', 'describeConformancePackCompliance_conformancePackName' - Name of the conformance pack.
newDescribeConformancePackCompliance ::
  -- | 'conformancePackName'
  Prelude.Text ->
  DescribeConformancePackCompliance
newDescribeConformancePackCompliance
  pConformancePackName_ =
    DescribeConformancePackCompliance'
      { nextToken =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        limit = Prelude.Nothing,
        conformancePackName =
          pConformancePackName_
      }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePackCompliance_nextToken :: Lens.Lens' DescribeConformancePackCompliance (Prelude.Maybe Prelude.Text)
describeConformancePackCompliance_nextToken = Lens.lens (\DescribeConformancePackCompliance' {nextToken} -> nextToken) (\s@DescribeConformancePackCompliance' {} a -> s {nextToken = a} :: DescribeConformancePackCompliance)

-- | A @ConformancePackComplianceFilters@ object.
describeConformancePackCompliance_filters :: Lens.Lens' DescribeConformancePackCompliance (Prelude.Maybe ConformancePackComplianceFilters)
describeConformancePackCompliance_filters = Lens.lens (\DescribeConformancePackCompliance' {filters} -> filters) (\s@DescribeConformancePackCompliance' {} a -> s {filters = a} :: DescribeConformancePackCompliance)

-- | The maximum number of AWS Config rules within a conformance pack are
-- returned on each page.
describeConformancePackCompliance_limit :: Lens.Lens' DescribeConformancePackCompliance (Prelude.Maybe Prelude.Natural)
describeConformancePackCompliance_limit = Lens.lens (\DescribeConformancePackCompliance' {limit} -> limit) (\s@DescribeConformancePackCompliance' {} a -> s {limit = a} :: DescribeConformancePackCompliance)

-- | Name of the conformance pack.
describeConformancePackCompliance_conformancePackName :: Lens.Lens' DescribeConformancePackCompliance Prelude.Text
describeConformancePackCompliance_conformancePackName = Lens.lens (\DescribeConformancePackCompliance' {conformancePackName} -> conformancePackName) (\s@DescribeConformancePackCompliance' {} a -> s {conformancePackName = a} :: DescribeConformancePackCompliance)

instance
  Core.AWSRequest
    DescribeConformancePackCompliance
  where
  type
    AWSResponse DescribeConformancePackCompliance =
      DescribeConformancePackComplianceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConformancePackComplianceResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Core..:> "ConformancePackName")
              Prelude.<*> ( x Core..?> "ConformancePackRuleComplianceList"
                              Core..!@ Prelude.mempty
                          )
      )

instance
  Prelude.Hashable
    DescribeConformancePackCompliance

instance
  Prelude.NFData
    DescribeConformancePackCompliance

instance
  Core.ToHeaders
    DescribeConformancePackCompliance
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeConformancePackCompliance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeConformancePackCompliance
  where
  toJSON DescribeConformancePackCompliance' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just
              ("ConformancePackName" Core..= conformancePackName)
          ]
      )

instance
  Core.ToPath
    DescribeConformancePackCompliance
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeConformancePackCompliance
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConformancePackComplianceResponse' smart constructor.
data DescribeConformancePackComplianceResponse = DescribeConformancePackComplianceResponse'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Name of the conformance pack.
    conformancePackName :: Prelude.Text,
    -- | Returns a list of @ConformancePackRuleCompliance@ objects.
    conformancePackRuleComplianceList :: [ConformancePackRuleCompliance]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConformancePackComplianceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConformancePackComplianceResponse_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'httpStatus', 'describeConformancePackComplianceResponse_httpStatus' - The response's http status code.
--
-- 'conformancePackName', 'describeConformancePackComplianceResponse_conformancePackName' - Name of the conformance pack.
--
-- 'conformancePackRuleComplianceList', 'describeConformancePackComplianceResponse_conformancePackRuleComplianceList' - Returns a list of @ConformancePackRuleCompliance@ objects.
newDescribeConformancePackComplianceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'conformancePackName'
  Prelude.Text ->
  DescribeConformancePackComplianceResponse
newDescribeConformancePackComplianceResponse
  pHttpStatus_
  pConformancePackName_ =
    DescribeConformancePackComplianceResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        conformancePackName =
          pConformancePackName_,
        conformancePackRuleComplianceList =
          Prelude.mempty
      }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePackComplianceResponse_nextToken :: Lens.Lens' DescribeConformancePackComplianceResponse (Prelude.Maybe Prelude.Text)
describeConformancePackComplianceResponse_nextToken = Lens.lens (\DescribeConformancePackComplianceResponse' {nextToken} -> nextToken) (\s@DescribeConformancePackComplianceResponse' {} a -> s {nextToken = a} :: DescribeConformancePackComplianceResponse)

-- | The response's http status code.
describeConformancePackComplianceResponse_httpStatus :: Lens.Lens' DescribeConformancePackComplianceResponse Prelude.Int
describeConformancePackComplianceResponse_httpStatus = Lens.lens (\DescribeConformancePackComplianceResponse' {httpStatus} -> httpStatus) (\s@DescribeConformancePackComplianceResponse' {} a -> s {httpStatus = a} :: DescribeConformancePackComplianceResponse)

-- | Name of the conformance pack.
describeConformancePackComplianceResponse_conformancePackName :: Lens.Lens' DescribeConformancePackComplianceResponse Prelude.Text
describeConformancePackComplianceResponse_conformancePackName = Lens.lens (\DescribeConformancePackComplianceResponse' {conformancePackName} -> conformancePackName) (\s@DescribeConformancePackComplianceResponse' {} a -> s {conformancePackName = a} :: DescribeConformancePackComplianceResponse)

-- | Returns a list of @ConformancePackRuleCompliance@ objects.
describeConformancePackComplianceResponse_conformancePackRuleComplianceList :: Lens.Lens' DescribeConformancePackComplianceResponse [ConformancePackRuleCompliance]
describeConformancePackComplianceResponse_conformancePackRuleComplianceList = Lens.lens (\DescribeConformancePackComplianceResponse' {conformancePackRuleComplianceList} -> conformancePackRuleComplianceList) (\s@DescribeConformancePackComplianceResponse' {} a -> s {conformancePackRuleComplianceList = a} :: DescribeConformancePackComplianceResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    DescribeConformancePackComplianceResponse
