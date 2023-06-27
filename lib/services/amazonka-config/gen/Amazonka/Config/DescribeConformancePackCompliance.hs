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
-- Module      : Amazonka.Config.DescribeConformancePackCompliance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details for each rule in that conformance pack.
--
-- You must provide exact rule names.
module Amazonka.Config.DescribeConformancePackCompliance
  ( -- * Creating a Request
    DescribeConformancePackCompliance (..),
    newDescribeConformancePackCompliance,

    -- * Request Lenses
    describeConformancePackCompliance_filters,
    describeConformancePackCompliance_limit,
    describeConformancePackCompliance_nextToken,
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConformancePackCompliance' smart constructor.
data DescribeConformancePackCompliance = DescribeConformancePackCompliance'
  { -- | A @ConformancePackComplianceFilters@ object.
    filters :: Prelude.Maybe ConformancePackComplianceFilters,
    -- | The maximum number of Config rules within a conformance pack are
    -- returned on each page.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'filters', 'describeConformancePackCompliance_filters' - A @ConformancePackComplianceFilters@ object.
--
-- 'limit', 'describeConformancePackCompliance_limit' - The maximum number of Config rules within a conformance pack are
-- returned on each page.
--
-- 'nextToken', 'describeConformancePackCompliance_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'conformancePackName', 'describeConformancePackCompliance_conformancePackName' - Name of the conformance pack.
newDescribeConformancePackCompliance ::
  -- | 'conformancePackName'
  Prelude.Text ->
  DescribeConformancePackCompliance
newDescribeConformancePackCompliance
  pConformancePackName_ =
    DescribeConformancePackCompliance'
      { filters =
          Prelude.Nothing,
        limit = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        conformancePackName =
          pConformancePackName_
      }

-- | A @ConformancePackComplianceFilters@ object.
describeConformancePackCompliance_filters :: Lens.Lens' DescribeConformancePackCompliance (Prelude.Maybe ConformancePackComplianceFilters)
describeConformancePackCompliance_filters = Lens.lens (\DescribeConformancePackCompliance' {filters} -> filters) (\s@DescribeConformancePackCompliance' {} a -> s {filters = a} :: DescribeConformancePackCompliance)

-- | The maximum number of Config rules within a conformance pack are
-- returned on each page.
describeConformancePackCompliance_limit :: Lens.Lens' DescribeConformancePackCompliance (Prelude.Maybe Prelude.Natural)
describeConformancePackCompliance_limit = Lens.lens (\DescribeConformancePackCompliance' {limit} -> limit) (\s@DescribeConformancePackCompliance' {} a -> s {limit = a} :: DescribeConformancePackCompliance)

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePackCompliance_nextToken :: Lens.Lens' DescribeConformancePackCompliance (Prelude.Maybe Prelude.Text)
describeConformancePackCompliance_nextToken = Lens.lens (\DescribeConformancePackCompliance' {nextToken} -> nextToken) (\s@DescribeConformancePackCompliance' {} a -> s {nextToken = a} :: DescribeConformancePackCompliance)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConformancePackComplianceResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ConformancePackName")
            Prelude.<*> ( x
                            Data..?> "ConformancePackRuleComplianceList"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    DescribeConformancePackCompliance
  where
  hashWithSalt
    _salt
    DescribeConformancePackCompliance' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` conformancePackName

instance
  Prelude.NFData
    DescribeConformancePackCompliance
  where
  rnf DescribeConformancePackCompliance' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf conformancePackName

instance
  Data.ToHeaders
    DescribeConformancePackCompliance
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeConformancePackCompliance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeConformancePackCompliance
  where
  toJSON DescribeConformancePackCompliance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("ConformancePackName" Data..= conformancePackName)
          ]
      )

instance
  Data.ToPath
    DescribeConformancePackCompliance
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
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
describeConformancePackComplianceResponse_conformancePackRuleComplianceList = Lens.lens (\DescribeConformancePackComplianceResponse' {conformancePackRuleComplianceList} -> conformancePackRuleComplianceList) (\s@DescribeConformancePackComplianceResponse' {} a -> s {conformancePackRuleComplianceList = a} :: DescribeConformancePackComplianceResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeConformancePackComplianceResponse
  where
  rnf DescribeConformancePackComplianceResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf conformancePackName
      `Prelude.seq` Prelude.rnf conformancePackRuleComplianceList
