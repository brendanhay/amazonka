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
-- Module      : Network.AWS.CloudWatchLogs.DeleteQueryDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a saved CloudWatch Logs Insights query definition. A query
-- definition contains details about a saved CloudWatch Logs Insights
-- query.
--
-- Each @DeleteQueryDefinition@ operation can delete one query definition.
--
-- You must have the @logs:DeleteQueryDefinition@ permission to be able to
-- perform this operation.
module Network.AWS.CloudWatchLogs.DeleteQueryDefinition
  ( -- * Creating a Request
    DeleteQueryDefinition (..),
    newDeleteQueryDefinition,

    -- * Request Lenses
    deleteQueryDefinition_queryDefinitionId,

    -- * Destructuring the Response
    DeleteQueryDefinitionResponse (..),
    newDeleteQueryDefinitionResponse,

    -- * Response Lenses
    deleteQueryDefinitionResponse_success,
    deleteQueryDefinitionResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteQueryDefinition' smart constructor.
data DeleteQueryDefinition = DeleteQueryDefinition'
  { -- | The ID of the query definition that you want to delete. You can use
    -- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>
    -- to retrieve the IDs of your saved query definitions.
    queryDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteQueryDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryDefinitionId', 'deleteQueryDefinition_queryDefinitionId' - The ID of the query definition that you want to delete. You can use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>
-- to retrieve the IDs of your saved query definitions.
newDeleteQueryDefinition ::
  -- | 'queryDefinitionId'
  Prelude.Text ->
  DeleteQueryDefinition
newDeleteQueryDefinition pQueryDefinitionId_ =
  DeleteQueryDefinition'
    { queryDefinitionId =
        pQueryDefinitionId_
    }

-- | The ID of the query definition that you want to delete. You can use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>
-- to retrieve the IDs of your saved query definitions.
deleteQueryDefinition_queryDefinitionId :: Lens.Lens' DeleteQueryDefinition Prelude.Text
deleteQueryDefinition_queryDefinitionId = Lens.lens (\DeleteQueryDefinition' {queryDefinitionId} -> queryDefinitionId) (\s@DeleteQueryDefinition' {} a -> s {queryDefinitionId = a} :: DeleteQueryDefinition)

instance Prelude.AWSRequest DeleteQueryDefinition where
  type
    Rs DeleteQueryDefinition =
      DeleteQueryDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteQueryDefinitionResponse'
            Prelude.<$> (x Prelude..?> "success")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteQueryDefinition

instance Prelude.NFData DeleteQueryDefinition

instance Prelude.ToHeaders DeleteQueryDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Logs_20140328.DeleteQueryDefinition" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteQueryDefinition where
  toJSON DeleteQueryDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("queryDefinitionId" Prelude..= queryDefinitionId)
          ]
      )

instance Prelude.ToPath DeleteQueryDefinition where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteQueryDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteQueryDefinitionResponse' smart constructor.
data DeleteQueryDefinitionResponse = DeleteQueryDefinitionResponse'
  { -- | A value of TRUE indicates that the operation succeeded. FALSE indicates
    -- that the operation failed.
    success :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteQueryDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'success', 'deleteQueryDefinitionResponse_success' - A value of TRUE indicates that the operation succeeded. FALSE indicates
-- that the operation failed.
--
-- 'httpStatus', 'deleteQueryDefinitionResponse_httpStatus' - The response's http status code.
newDeleteQueryDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteQueryDefinitionResponse
newDeleteQueryDefinitionResponse pHttpStatus_ =
  DeleteQueryDefinitionResponse'
    { success =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value of TRUE indicates that the operation succeeded. FALSE indicates
-- that the operation failed.
deleteQueryDefinitionResponse_success :: Lens.Lens' DeleteQueryDefinitionResponse (Prelude.Maybe Prelude.Bool)
deleteQueryDefinitionResponse_success = Lens.lens (\DeleteQueryDefinitionResponse' {success} -> success) (\s@DeleteQueryDefinitionResponse' {} a -> s {success = a} :: DeleteQueryDefinitionResponse)

-- | The response's http status code.
deleteQueryDefinitionResponse_httpStatus :: Lens.Lens' DeleteQueryDefinitionResponse Prelude.Int
deleteQueryDefinitionResponse_httpStatus = Lens.lens (\DeleteQueryDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteQueryDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteQueryDefinitionResponse)

instance Prelude.NFData DeleteQueryDefinitionResponse
