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
-- Module      : Amazonka.DynamoDB.DescribeContributorInsights
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about contributor insights, for a given table or
-- global secondary index.
module Amazonka.DynamoDB.DescribeContributorInsights
  ( -- * Creating a Request
    DescribeContributorInsights (..),
    newDescribeContributorInsights,

    -- * Request Lenses
    describeContributorInsights_indexName,
    describeContributorInsights_tableName,

    -- * Destructuring the Response
    DescribeContributorInsightsResponse (..),
    newDescribeContributorInsightsResponse,

    -- * Response Lenses
    describeContributorInsightsResponse_contributorInsightsRuleList,
    describeContributorInsightsResponse_contributorInsightsStatus,
    describeContributorInsightsResponse_failureException,
    describeContributorInsightsResponse_indexName,
    describeContributorInsightsResponse_lastUpdateDateTime,
    describeContributorInsightsResponse_tableName,
    describeContributorInsightsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeContributorInsights' smart constructor.
data DescribeContributorInsights = DescribeContributorInsights'
  { -- | The name of the global secondary index to describe, if applicable.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The name of the table to describe.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContributorInsights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'describeContributorInsights_indexName' - The name of the global secondary index to describe, if applicable.
--
-- 'tableName', 'describeContributorInsights_tableName' - The name of the table to describe.
newDescribeContributorInsights ::
  -- | 'tableName'
  Prelude.Text ->
  DescribeContributorInsights
newDescribeContributorInsights pTableName_ =
  DescribeContributorInsights'
    { indexName =
        Prelude.Nothing,
      tableName = pTableName_
    }

-- | The name of the global secondary index to describe, if applicable.
describeContributorInsights_indexName :: Lens.Lens' DescribeContributorInsights (Prelude.Maybe Prelude.Text)
describeContributorInsights_indexName = Lens.lens (\DescribeContributorInsights' {indexName} -> indexName) (\s@DescribeContributorInsights' {} a -> s {indexName = a} :: DescribeContributorInsights)

-- | The name of the table to describe.
describeContributorInsights_tableName :: Lens.Lens' DescribeContributorInsights Prelude.Text
describeContributorInsights_tableName = Lens.lens (\DescribeContributorInsights' {tableName} -> tableName) (\s@DescribeContributorInsights' {} a -> s {tableName = a} :: DescribeContributorInsights)

instance Core.AWSRequest DescribeContributorInsights where
  type
    AWSResponse DescribeContributorInsights =
      DescribeContributorInsightsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContributorInsightsResponse'
            Prelude.<$> ( x
                            Data..?> "ContributorInsightsRuleList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ContributorInsightsStatus")
            Prelude.<*> (x Data..?> "FailureException")
            Prelude.<*> (x Data..?> "IndexName")
            Prelude.<*> (x Data..?> "LastUpdateDateTime")
            Prelude.<*> (x Data..?> "TableName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContributorInsights where
  hashWithSalt _salt DescribeContributorInsights' {..} =
    _salt
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData DescribeContributorInsights where
  rnf DescribeContributorInsights' {..} =
    Prelude.rnf indexName `Prelude.seq`
      Prelude.rnf tableName

instance Data.ToHeaders DescribeContributorInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.DescribeContributorInsights" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeContributorInsights where
  toJSON DescribeContributorInsights' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IndexName" Data..=) Prelude.<$> indexName,
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )

instance Data.ToPath DescribeContributorInsights where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeContributorInsights where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeContributorInsightsResponse' smart constructor.
data DescribeContributorInsightsResponse = DescribeContributorInsightsResponse'
  { -- | List of names of the associated contributor insights rules.
    contributorInsightsRuleList :: Prelude.Maybe [Prelude.Text],
    -- | Current status of contributor insights.
    contributorInsightsStatus :: Prelude.Maybe ContributorInsightsStatus,
    -- | Returns information about the last failure that was encountered.
    --
    -- The most common exceptions for a FAILED status are:
    --
    -- -   LimitExceededException - Per-account Amazon CloudWatch Contributor
    --     Insights rule limit reached. Please disable Contributor Insights for
    --     other tables\/indexes OR disable Contributor Insights rules before
    --     retrying.
    --
    -- -   AccessDeniedException - Amazon CloudWatch Contributor Insights rules
    --     cannot be modified due to insufficient permissions.
    --
    -- -   AccessDeniedException - Failed to create service-linked role for
    --     Contributor Insights due to insufficient permissions.
    --
    -- -   InternalServerError - Failed to create Amazon CloudWatch Contributor
    --     Insights rules. Please retry request.
    failureException :: Prelude.Maybe FailureException,
    -- | The name of the global secondary index being described.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of the last time the status was changed.
    lastUpdateDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the table being described.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContributorInsightsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contributorInsightsRuleList', 'describeContributorInsightsResponse_contributorInsightsRuleList' - List of names of the associated contributor insights rules.
--
-- 'contributorInsightsStatus', 'describeContributorInsightsResponse_contributorInsightsStatus' - Current status of contributor insights.
--
-- 'failureException', 'describeContributorInsightsResponse_failureException' - Returns information about the last failure that was encountered.
--
-- The most common exceptions for a FAILED status are:
--
-- -   LimitExceededException - Per-account Amazon CloudWatch Contributor
--     Insights rule limit reached. Please disable Contributor Insights for
--     other tables\/indexes OR disable Contributor Insights rules before
--     retrying.
--
-- -   AccessDeniedException - Amazon CloudWatch Contributor Insights rules
--     cannot be modified due to insufficient permissions.
--
-- -   AccessDeniedException - Failed to create service-linked role for
--     Contributor Insights due to insufficient permissions.
--
-- -   InternalServerError - Failed to create Amazon CloudWatch Contributor
--     Insights rules. Please retry request.
--
-- 'indexName', 'describeContributorInsightsResponse_indexName' - The name of the global secondary index being described.
--
-- 'lastUpdateDateTime', 'describeContributorInsightsResponse_lastUpdateDateTime' - Timestamp of the last time the status was changed.
--
-- 'tableName', 'describeContributorInsightsResponse_tableName' - The name of the table being described.
--
-- 'httpStatus', 'describeContributorInsightsResponse_httpStatus' - The response's http status code.
newDescribeContributorInsightsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeContributorInsightsResponse
newDescribeContributorInsightsResponse pHttpStatus_ =
  DescribeContributorInsightsResponse'
    { contributorInsightsRuleList =
        Prelude.Nothing,
      contributorInsightsStatus =
        Prelude.Nothing,
      failureException = Prelude.Nothing,
      indexName = Prelude.Nothing,
      lastUpdateDateTime = Prelude.Nothing,
      tableName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of names of the associated contributor insights rules.
describeContributorInsightsResponse_contributorInsightsRuleList :: Lens.Lens' DescribeContributorInsightsResponse (Prelude.Maybe [Prelude.Text])
describeContributorInsightsResponse_contributorInsightsRuleList = Lens.lens (\DescribeContributorInsightsResponse' {contributorInsightsRuleList} -> contributorInsightsRuleList) (\s@DescribeContributorInsightsResponse' {} a -> s {contributorInsightsRuleList = a} :: DescribeContributorInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Current status of contributor insights.
describeContributorInsightsResponse_contributorInsightsStatus :: Lens.Lens' DescribeContributorInsightsResponse (Prelude.Maybe ContributorInsightsStatus)
describeContributorInsightsResponse_contributorInsightsStatus = Lens.lens (\DescribeContributorInsightsResponse' {contributorInsightsStatus} -> contributorInsightsStatus) (\s@DescribeContributorInsightsResponse' {} a -> s {contributorInsightsStatus = a} :: DescribeContributorInsightsResponse)

-- | Returns information about the last failure that was encountered.
--
-- The most common exceptions for a FAILED status are:
--
-- -   LimitExceededException - Per-account Amazon CloudWatch Contributor
--     Insights rule limit reached. Please disable Contributor Insights for
--     other tables\/indexes OR disable Contributor Insights rules before
--     retrying.
--
-- -   AccessDeniedException - Amazon CloudWatch Contributor Insights rules
--     cannot be modified due to insufficient permissions.
--
-- -   AccessDeniedException - Failed to create service-linked role for
--     Contributor Insights due to insufficient permissions.
--
-- -   InternalServerError - Failed to create Amazon CloudWatch Contributor
--     Insights rules. Please retry request.
describeContributorInsightsResponse_failureException :: Lens.Lens' DescribeContributorInsightsResponse (Prelude.Maybe FailureException)
describeContributorInsightsResponse_failureException = Lens.lens (\DescribeContributorInsightsResponse' {failureException} -> failureException) (\s@DescribeContributorInsightsResponse' {} a -> s {failureException = a} :: DescribeContributorInsightsResponse)

-- | The name of the global secondary index being described.
describeContributorInsightsResponse_indexName :: Lens.Lens' DescribeContributorInsightsResponse (Prelude.Maybe Prelude.Text)
describeContributorInsightsResponse_indexName = Lens.lens (\DescribeContributorInsightsResponse' {indexName} -> indexName) (\s@DescribeContributorInsightsResponse' {} a -> s {indexName = a} :: DescribeContributorInsightsResponse)

-- | Timestamp of the last time the status was changed.
describeContributorInsightsResponse_lastUpdateDateTime :: Lens.Lens' DescribeContributorInsightsResponse (Prelude.Maybe Prelude.UTCTime)
describeContributorInsightsResponse_lastUpdateDateTime = Lens.lens (\DescribeContributorInsightsResponse' {lastUpdateDateTime} -> lastUpdateDateTime) (\s@DescribeContributorInsightsResponse' {} a -> s {lastUpdateDateTime = a} :: DescribeContributorInsightsResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the table being described.
describeContributorInsightsResponse_tableName :: Lens.Lens' DescribeContributorInsightsResponse (Prelude.Maybe Prelude.Text)
describeContributorInsightsResponse_tableName = Lens.lens (\DescribeContributorInsightsResponse' {tableName} -> tableName) (\s@DescribeContributorInsightsResponse' {} a -> s {tableName = a} :: DescribeContributorInsightsResponse)

-- | The response's http status code.
describeContributorInsightsResponse_httpStatus :: Lens.Lens' DescribeContributorInsightsResponse Prelude.Int
describeContributorInsightsResponse_httpStatus = Lens.lens (\DescribeContributorInsightsResponse' {httpStatus} -> httpStatus) (\s@DescribeContributorInsightsResponse' {} a -> s {httpStatus = a} :: DescribeContributorInsightsResponse)

instance
  Prelude.NFData
    DescribeContributorInsightsResponse
  where
  rnf DescribeContributorInsightsResponse' {..} =
    Prelude.rnf contributorInsightsRuleList `Prelude.seq`
      Prelude.rnf contributorInsightsStatus `Prelude.seq`
        Prelude.rnf failureException `Prelude.seq`
          Prelude.rnf indexName `Prelude.seq`
            Prelude.rnf lastUpdateDateTime `Prelude.seq`
              Prelude.rnf tableName `Prelude.seq`
                Prelude.rnf httpStatus
