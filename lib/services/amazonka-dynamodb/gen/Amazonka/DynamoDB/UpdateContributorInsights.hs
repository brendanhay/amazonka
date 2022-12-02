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
-- Module      : Amazonka.DynamoDB.UpdateContributorInsights
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status for contributor insights for a specific table or
-- index. CloudWatch Contributor Insights for DynamoDB graphs display the
-- partition key and (if applicable) sort key of frequently accessed items
-- and frequently throttled items in plaintext. If you require the use of
-- Amazon Web Services Key Management Service (KMS) to encrypt this table’s
-- partition key and sort key data with an Amazon Web Services managed key
-- or customer managed key, you should not enable CloudWatch Contributor
-- Insights for DynamoDB for this table.
module Amazonka.DynamoDB.UpdateContributorInsights
  ( -- * Creating a Request
    UpdateContributorInsights (..),
    newUpdateContributorInsights,

    -- * Request Lenses
    updateContributorInsights_indexName,
    updateContributorInsights_tableName,
    updateContributorInsights_contributorInsightsAction,

    -- * Destructuring the Response
    UpdateContributorInsightsResponse (..),
    newUpdateContributorInsightsResponse,

    -- * Response Lenses
    updateContributorInsightsResponse_contributorInsightsStatus,
    updateContributorInsightsResponse_tableName,
    updateContributorInsightsResponse_indexName,
    updateContributorInsightsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContributorInsights' smart constructor.
data UpdateContributorInsights = UpdateContributorInsights'
  { -- | The global secondary index name, if applicable.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The name of the table.
    tableName :: Prelude.Text,
    -- | Represents the contributor insights action.
    contributorInsightsAction :: ContributorInsightsAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContributorInsights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'updateContributorInsights_indexName' - The global secondary index name, if applicable.
--
-- 'tableName', 'updateContributorInsights_tableName' - The name of the table.
--
-- 'contributorInsightsAction', 'updateContributorInsights_contributorInsightsAction' - Represents the contributor insights action.
newUpdateContributorInsights ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'contributorInsightsAction'
  ContributorInsightsAction ->
  UpdateContributorInsights
newUpdateContributorInsights
  pTableName_
  pContributorInsightsAction_ =
    UpdateContributorInsights'
      { indexName =
          Prelude.Nothing,
        tableName = pTableName_,
        contributorInsightsAction =
          pContributorInsightsAction_
      }

-- | The global secondary index name, if applicable.
updateContributorInsights_indexName :: Lens.Lens' UpdateContributorInsights (Prelude.Maybe Prelude.Text)
updateContributorInsights_indexName = Lens.lens (\UpdateContributorInsights' {indexName} -> indexName) (\s@UpdateContributorInsights' {} a -> s {indexName = a} :: UpdateContributorInsights)

-- | The name of the table.
updateContributorInsights_tableName :: Lens.Lens' UpdateContributorInsights Prelude.Text
updateContributorInsights_tableName = Lens.lens (\UpdateContributorInsights' {tableName} -> tableName) (\s@UpdateContributorInsights' {} a -> s {tableName = a} :: UpdateContributorInsights)

-- | Represents the contributor insights action.
updateContributorInsights_contributorInsightsAction :: Lens.Lens' UpdateContributorInsights ContributorInsightsAction
updateContributorInsights_contributorInsightsAction = Lens.lens (\UpdateContributorInsights' {contributorInsightsAction} -> contributorInsightsAction) (\s@UpdateContributorInsights' {} a -> s {contributorInsightsAction = a} :: UpdateContributorInsights)

instance Core.AWSRequest UpdateContributorInsights where
  type
    AWSResponse UpdateContributorInsights =
      UpdateContributorInsightsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContributorInsightsResponse'
            Prelude.<$> (x Data..?> "ContributorInsightsStatus")
            Prelude.<*> (x Data..?> "TableName")
            Prelude.<*> (x Data..?> "IndexName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContributorInsights where
  hashWithSalt _salt UpdateContributorInsights' {..} =
    _salt `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` contributorInsightsAction

instance Prelude.NFData UpdateContributorInsights where
  rnf UpdateContributorInsights' {..} =
    Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf contributorInsightsAction

instance Data.ToHeaders UpdateContributorInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.UpdateContributorInsights" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateContributorInsights where
  toJSON UpdateContributorInsights' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IndexName" Data..=) Prelude.<$> indexName,
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ( "ContributorInsightsAction"
                  Data..= contributorInsightsAction
              )
          ]
      )

instance Data.ToPath UpdateContributorInsights where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateContributorInsights where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContributorInsightsResponse' smart constructor.
data UpdateContributorInsightsResponse = UpdateContributorInsightsResponse'
  { -- | The status of contributor insights
    contributorInsightsStatus :: Prelude.Maybe ContributorInsightsStatus,
    -- | The name of the table.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The name of the global secondary index, if applicable.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContributorInsightsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contributorInsightsStatus', 'updateContributorInsightsResponse_contributorInsightsStatus' - The status of contributor insights
--
-- 'tableName', 'updateContributorInsightsResponse_tableName' - The name of the table.
--
-- 'indexName', 'updateContributorInsightsResponse_indexName' - The name of the global secondary index, if applicable.
--
-- 'httpStatus', 'updateContributorInsightsResponse_httpStatus' - The response's http status code.
newUpdateContributorInsightsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateContributorInsightsResponse
newUpdateContributorInsightsResponse pHttpStatus_ =
  UpdateContributorInsightsResponse'
    { contributorInsightsStatus =
        Prelude.Nothing,
      tableName = Prelude.Nothing,
      indexName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of contributor insights
updateContributorInsightsResponse_contributorInsightsStatus :: Lens.Lens' UpdateContributorInsightsResponse (Prelude.Maybe ContributorInsightsStatus)
updateContributorInsightsResponse_contributorInsightsStatus = Lens.lens (\UpdateContributorInsightsResponse' {contributorInsightsStatus} -> contributorInsightsStatus) (\s@UpdateContributorInsightsResponse' {} a -> s {contributorInsightsStatus = a} :: UpdateContributorInsightsResponse)

-- | The name of the table.
updateContributorInsightsResponse_tableName :: Lens.Lens' UpdateContributorInsightsResponse (Prelude.Maybe Prelude.Text)
updateContributorInsightsResponse_tableName = Lens.lens (\UpdateContributorInsightsResponse' {tableName} -> tableName) (\s@UpdateContributorInsightsResponse' {} a -> s {tableName = a} :: UpdateContributorInsightsResponse)

-- | The name of the global secondary index, if applicable.
updateContributorInsightsResponse_indexName :: Lens.Lens' UpdateContributorInsightsResponse (Prelude.Maybe Prelude.Text)
updateContributorInsightsResponse_indexName = Lens.lens (\UpdateContributorInsightsResponse' {indexName} -> indexName) (\s@UpdateContributorInsightsResponse' {} a -> s {indexName = a} :: UpdateContributorInsightsResponse)

-- | The response's http status code.
updateContributorInsightsResponse_httpStatus :: Lens.Lens' UpdateContributorInsightsResponse Prelude.Int
updateContributorInsightsResponse_httpStatus = Lens.lens (\UpdateContributorInsightsResponse' {httpStatus} -> httpStatus) (\s@UpdateContributorInsightsResponse' {} a -> s {httpStatus = a} :: UpdateContributorInsightsResponse)

instance
  Prelude.NFData
    UpdateContributorInsightsResponse
  where
  rnf UpdateContributorInsightsResponse' {..} =
    Prelude.rnf contributorInsightsStatus
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf httpStatus
