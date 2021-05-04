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

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateContributorInsights' smart constructor.
data UpdateContributorInsights = UpdateContributorInsights'
  { -- | The global secondary index name, if applicable.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The name of the table.
    tableName :: Prelude.Text,
    -- | Represents the contributor insights action.
    contributorInsightsAction :: ContributorInsightsAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest UpdateContributorInsights where
  type
    Rs UpdateContributorInsights =
      UpdateContributorInsightsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContributorInsightsResponse'
            Prelude.<$> (x Prelude..?> "TableName")
            Prelude.<*> (x Prelude..?> "IndexName")
            Prelude.<*> (x Prelude..?> "ContributorInsightsStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContributorInsights

instance Prelude.NFData UpdateContributorInsights

instance Prelude.ToHeaders UpdateContributorInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DynamoDB_20120810.UpdateContributorInsights" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateContributorInsights where
  toJSON UpdateContributorInsights' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IndexName" Prelude..=) Prelude.<$> indexName,
            Prelude.Just ("TableName" Prelude..= tableName),
            Prelude.Just
              ( "ContributorInsightsAction"
                  Prelude..= contributorInsightsAction
              )
          ]
      )

instance Prelude.ToPath UpdateContributorInsights where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateContributorInsights where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContributorInsightsResponse' smart constructor.
data UpdateContributorInsightsResponse = UpdateContributorInsightsResponse'
  { -- | The name of the table.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The name of the global secondary index, if applicable.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The status of contributor insights
    contributorInsightsStatus :: Prelude.Maybe ContributorInsightsStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateContributorInsightsResponse
newUpdateContributorInsightsResponse pHttpStatus_ =
  UpdateContributorInsightsResponse'
    { tableName =
        Prelude.Nothing,
      indexName = Prelude.Nothing,
      contributorInsightsStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the table.
updateContributorInsightsResponse_tableName :: Lens.Lens' UpdateContributorInsightsResponse (Prelude.Maybe Prelude.Text)
updateContributorInsightsResponse_tableName = Lens.lens (\UpdateContributorInsightsResponse' {tableName} -> tableName) (\s@UpdateContributorInsightsResponse' {} a -> s {tableName = a} :: UpdateContributorInsightsResponse)

-- | The name of the global secondary index, if applicable.
updateContributorInsightsResponse_indexName :: Lens.Lens' UpdateContributorInsightsResponse (Prelude.Maybe Prelude.Text)
updateContributorInsightsResponse_indexName = Lens.lens (\UpdateContributorInsightsResponse' {indexName} -> indexName) (\s@UpdateContributorInsightsResponse' {} a -> s {indexName = a} :: UpdateContributorInsightsResponse)

-- | The status of contributor insights
updateContributorInsightsResponse_contributorInsightsStatus :: Lens.Lens' UpdateContributorInsightsResponse (Prelude.Maybe ContributorInsightsStatus)
updateContributorInsightsResponse_contributorInsightsStatus = Lens.lens (\UpdateContributorInsightsResponse' {contributorInsightsStatus} -> contributorInsightsStatus) (\s@UpdateContributorInsightsResponse' {} a -> s {contributorInsightsStatus = a} :: UpdateContributorInsightsResponse)

-- | The response's http status code.
updateContributorInsightsResponse_httpStatus :: Lens.Lens' UpdateContributorInsightsResponse Prelude.Int
updateContributorInsightsResponse_httpStatus = Lens.lens (\UpdateContributorInsightsResponse' {httpStatus} -> httpStatus) (\s@UpdateContributorInsightsResponse' {} a -> s {httpStatus = a} :: UpdateContributorInsightsResponse)

instance
  Prelude.NFData
    UpdateContributorInsightsResponse
