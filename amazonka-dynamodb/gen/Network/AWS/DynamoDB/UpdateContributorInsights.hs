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
-- Module      : Network.AWS.DynamoDB.UpdateContributorInsights
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status for contributor insights for a specific table or
-- index.
module Network.AWS.DynamoDB.UpdateContributorInsights
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
    updateContributorInsightsResponse_tableName,
    updateContributorInsightsResponse_indexName,
    updateContributorInsightsResponse_contributorInsightsStatus,
    updateContributorInsightsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateContributorInsights' smart constructor.
data UpdateContributorInsights = UpdateContributorInsights'
  { -- | The global secondary index name, if applicable.
    indexName :: Core.Maybe Core.Text,
    -- | The name of the table.
    tableName :: Core.Text,
    -- | Represents the contributor insights action.
    contributorInsightsAction :: ContributorInsightsAction
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'contributorInsightsAction'
  ContributorInsightsAction ->
  UpdateContributorInsights
newUpdateContributorInsights
  pTableName_
  pContributorInsightsAction_ =
    UpdateContributorInsights'
      { indexName =
          Core.Nothing,
        tableName = pTableName_,
        contributorInsightsAction =
          pContributorInsightsAction_
      }

-- | The global secondary index name, if applicable.
updateContributorInsights_indexName :: Lens.Lens' UpdateContributorInsights (Core.Maybe Core.Text)
updateContributorInsights_indexName = Lens.lens (\UpdateContributorInsights' {indexName} -> indexName) (\s@UpdateContributorInsights' {} a -> s {indexName = a} :: UpdateContributorInsights)

-- | The name of the table.
updateContributorInsights_tableName :: Lens.Lens' UpdateContributorInsights Core.Text
updateContributorInsights_tableName = Lens.lens (\UpdateContributorInsights' {tableName} -> tableName) (\s@UpdateContributorInsights' {} a -> s {tableName = a} :: UpdateContributorInsights)

-- | Represents the contributor insights action.
updateContributorInsights_contributorInsightsAction :: Lens.Lens' UpdateContributorInsights ContributorInsightsAction
updateContributorInsights_contributorInsightsAction = Lens.lens (\UpdateContributorInsights' {contributorInsightsAction} -> contributorInsightsAction) (\s@UpdateContributorInsights' {} a -> s {contributorInsightsAction = a} :: UpdateContributorInsights)

instance Core.AWSRequest UpdateContributorInsights where
  type
    AWSResponse UpdateContributorInsights =
      UpdateContributorInsightsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContributorInsightsResponse'
            Core.<$> (x Core..?> "TableName")
            Core.<*> (x Core..?> "IndexName")
            Core.<*> (x Core..?> "ContributorInsightsStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateContributorInsights

instance Core.NFData UpdateContributorInsights

instance Core.ToHeaders UpdateContributorInsights where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.UpdateContributorInsights" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateContributorInsights where
  toJSON UpdateContributorInsights' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IndexName" Core..=) Core.<$> indexName,
            Core.Just ("TableName" Core..= tableName),
            Core.Just
              ( "ContributorInsightsAction"
                  Core..= contributorInsightsAction
              )
          ]
      )

instance Core.ToPath UpdateContributorInsights where
  toPath = Core.const "/"

instance Core.ToQuery UpdateContributorInsights where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateContributorInsightsResponse' smart constructor.
data UpdateContributorInsightsResponse = UpdateContributorInsightsResponse'
  { -- | The name of the table.
    tableName :: Core.Maybe Core.Text,
    -- | The name of the global secondary index, if applicable.
    indexName :: Core.Maybe Core.Text,
    -- | The status of contributor insights
    contributorInsightsStatus :: Core.Maybe ContributorInsightsStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateContributorInsightsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'updateContributorInsightsResponse_tableName' - The name of the table.
--
-- 'indexName', 'updateContributorInsightsResponse_indexName' - The name of the global secondary index, if applicable.
--
-- 'contributorInsightsStatus', 'updateContributorInsightsResponse_contributorInsightsStatus' - The status of contributor insights
--
-- 'httpStatus', 'updateContributorInsightsResponse_httpStatus' - The response's http status code.
newUpdateContributorInsightsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateContributorInsightsResponse
newUpdateContributorInsightsResponse pHttpStatus_ =
  UpdateContributorInsightsResponse'
    { tableName =
        Core.Nothing,
      indexName = Core.Nothing,
      contributorInsightsStatus = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the table.
updateContributorInsightsResponse_tableName :: Lens.Lens' UpdateContributorInsightsResponse (Core.Maybe Core.Text)
updateContributorInsightsResponse_tableName = Lens.lens (\UpdateContributorInsightsResponse' {tableName} -> tableName) (\s@UpdateContributorInsightsResponse' {} a -> s {tableName = a} :: UpdateContributorInsightsResponse)

-- | The name of the global secondary index, if applicable.
updateContributorInsightsResponse_indexName :: Lens.Lens' UpdateContributorInsightsResponse (Core.Maybe Core.Text)
updateContributorInsightsResponse_indexName = Lens.lens (\UpdateContributorInsightsResponse' {indexName} -> indexName) (\s@UpdateContributorInsightsResponse' {} a -> s {indexName = a} :: UpdateContributorInsightsResponse)

-- | The status of contributor insights
updateContributorInsightsResponse_contributorInsightsStatus :: Lens.Lens' UpdateContributorInsightsResponse (Core.Maybe ContributorInsightsStatus)
updateContributorInsightsResponse_contributorInsightsStatus = Lens.lens (\UpdateContributorInsightsResponse' {contributorInsightsStatus} -> contributorInsightsStatus) (\s@UpdateContributorInsightsResponse' {} a -> s {contributorInsightsStatus = a} :: UpdateContributorInsightsResponse)

-- | The response's http status code.
updateContributorInsightsResponse_httpStatus :: Lens.Lens' UpdateContributorInsightsResponse Core.Int
updateContributorInsightsResponse_httpStatus = Lens.lens (\UpdateContributorInsightsResponse' {httpStatus} -> httpStatus) (\s@UpdateContributorInsightsResponse' {} a -> s {httpStatus = a} :: UpdateContributorInsightsResponse)

instance
  Core.NFData
    UpdateContributorInsightsResponse
