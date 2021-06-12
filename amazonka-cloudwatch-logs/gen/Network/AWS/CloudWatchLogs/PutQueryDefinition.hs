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
-- Module      : Network.AWS.CloudWatchLogs.PutQueryDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a query definition for CloudWatch Logs Insights. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AnalyzingLogData.html Analyzing Log Data with CloudWatch Logs Insights>.
--
-- To update a query definition, specify its @queryDefinitionId@ in your
-- request. The values of @name@, @queryString@, and @logGroupNames@ are
-- changed to the values that you specify in your update operation. No
-- current values are retained from the current query definition. For
-- example, if you update a current query definition that includes log
-- groups, and you don\'t specify the @logGroupNames@ parameter in your
-- update operation, the query definition changes to contain no log groups.
--
-- You must have the @logs:PutQueryDefinition@ permission to be able to
-- perform this operation.
module Network.AWS.CloudWatchLogs.PutQueryDefinition
  ( -- * Creating a Request
    PutQueryDefinition (..),
    newPutQueryDefinition,

    -- * Request Lenses
    putQueryDefinition_logGroupNames,
    putQueryDefinition_queryDefinitionId,
    putQueryDefinition_name,
    putQueryDefinition_queryString,

    -- * Destructuring the Response
    PutQueryDefinitionResponse (..),
    newPutQueryDefinitionResponse,

    -- * Response Lenses
    putQueryDefinitionResponse_queryDefinitionId,
    putQueryDefinitionResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutQueryDefinition' smart constructor.
data PutQueryDefinition = PutQueryDefinition'
  { -- | Use this parameter to include specific log groups as part of your query
    -- definition.
    --
    -- If you are updating a query definition and you omit this parameter, then
    -- the updated definition will contain no log groups.
    logGroupNames :: Core.Maybe [Core.Text],
    -- | If you are updating a query definition, use this parameter to specify
    -- the ID of the query definition that you want to update. You can use
    -- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>
    -- to retrieve the IDs of your saved query definitions.
    --
    -- If you are creating a query definition, do not specify this parameter.
    -- CloudWatch generates a unique ID for the new query definition and
    -- include it in the response to this operation.
    queryDefinitionId :: Core.Maybe Core.Text,
    -- | A name for the query definition. If you are saving a lot of query
    -- definitions, we recommend that you name them so that you can easily find
    -- the ones you want by using the first part of the name as a filter in the
    -- @queryDefinitionNamePrefix@ parameter of
    -- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>.
    name :: Core.Text,
    -- | The query string to use for this definition. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
    queryString :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutQueryDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupNames', 'putQueryDefinition_logGroupNames' - Use this parameter to include specific log groups as part of your query
-- definition.
--
-- If you are updating a query definition and you omit this parameter, then
-- the updated definition will contain no log groups.
--
-- 'queryDefinitionId', 'putQueryDefinition_queryDefinitionId' - If you are updating a query definition, use this parameter to specify
-- the ID of the query definition that you want to update. You can use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>
-- to retrieve the IDs of your saved query definitions.
--
-- If you are creating a query definition, do not specify this parameter.
-- CloudWatch generates a unique ID for the new query definition and
-- include it in the response to this operation.
--
-- 'name', 'putQueryDefinition_name' - A name for the query definition. If you are saving a lot of query
-- definitions, we recommend that you name them so that you can easily find
-- the ones you want by using the first part of the name as a filter in the
-- @queryDefinitionNamePrefix@ parameter of
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>.
--
-- 'queryString', 'putQueryDefinition_queryString' - The query string to use for this definition. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
newPutQueryDefinition ::
  -- | 'name'
  Core.Text ->
  -- | 'queryString'
  Core.Text ->
  PutQueryDefinition
newPutQueryDefinition pName_ pQueryString_ =
  PutQueryDefinition'
    { logGroupNames = Core.Nothing,
      queryDefinitionId = Core.Nothing,
      name = pName_,
      queryString = pQueryString_
    }

-- | Use this parameter to include specific log groups as part of your query
-- definition.
--
-- If you are updating a query definition and you omit this parameter, then
-- the updated definition will contain no log groups.
putQueryDefinition_logGroupNames :: Lens.Lens' PutQueryDefinition (Core.Maybe [Core.Text])
putQueryDefinition_logGroupNames = Lens.lens (\PutQueryDefinition' {logGroupNames} -> logGroupNames) (\s@PutQueryDefinition' {} a -> s {logGroupNames = a} :: PutQueryDefinition) Core.. Lens.mapping Lens._Coerce

-- | If you are updating a query definition, use this parameter to specify
-- the ID of the query definition that you want to update. You can use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>
-- to retrieve the IDs of your saved query definitions.
--
-- If you are creating a query definition, do not specify this parameter.
-- CloudWatch generates a unique ID for the new query definition and
-- include it in the response to this operation.
putQueryDefinition_queryDefinitionId :: Lens.Lens' PutQueryDefinition (Core.Maybe Core.Text)
putQueryDefinition_queryDefinitionId = Lens.lens (\PutQueryDefinition' {queryDefinitionId} -> queryDefinitionId) (\s@PutQueryDefinition' {} a -> s {queryDefinitionId = a} :: PutQueryDefinition)

-- | A name for the query definition. If you are saving a lot of query
-- definitions, we recommend that you name them so that you can easily find
-- the ones you want by using the first part of the name as a filter in the
-- @queryDefinitionNamePrefix@ parameter of
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>.
putQueryDefinition_name :: Lens.Lens' PutQueryDefinition Core.Text
putQueryDefinition_name = Lens.lens (\PutQueryDefinition' {name} -> name) (\s@PutQueryDefinition' {} a -> s {name = a} :: PutQueryDefinition)

-- | The query string to use for this definition. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
putQueryDefinition_queryString :: Lens.Lens' PutQueryDefinition Core.Text
putQueryDefinition_queryString = Lens.lens (\PutQueryDefinition' {queryString} -> queryString) (\s@PutQueryDefinition' {} a -> s {queryString = a} :: PutQueryDefinition)

instance Core.AWSRequest PutQueryDefinition where
  type
    AWSResponse PutQueryDefinition =
      PutQueryDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutQueryDefinitionResponse'
            Core.<$> (x Core..?> "queryDefinitionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutQueryDefinition

instance Core.NFData PutQueryDefinition

instance Core.ToHeaders PutQueryDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.PutQueryDefinition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutQueryDefinition where
  toJSON PutQueryDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("logGroupNames" Core..=) Core.<$> logGroupNames,
            ("queryDefinitionId" Core..=)
              Core.<$> queryDefinitionId,
            Core.Just ("name" Core..= name),
            Core.Just ("queryString" Core..= queryString)
          ]
      )

instance Core.ToPath PutQueryDefinition where
  toPath = Core.const "/"

instance Core.ToQuery PutQueryDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutQueryDefinitionResponse' smart constructor.
data PutQueryDefinitionResponse = PutQueryDefinitionResponse'
  { -- | The ID of the query definition.
    queryDefinitionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutQueryDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryDefinitionId', 'putQueryDefinitionResponse_queryDefinitionId' - The ID of the query definition.
--
-- 'httpStatus', 'putQueryDefinitionResponse_httpStatus' - The response's http status code.
newPutQueryDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutQueryDefinitionResponse
newPutQueryDefinitionResponse pHttpStatus_ =
  PutQueryDefinitionResponse'
    { queryDefinitionId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the query definition.
putQueryDefinitionResponse_queryDefinitionId :: Lens.Lens' PutQueryDefinitionResponse (Core.Maybe Core.Text)
putQueryDefinitionResponse_queryDefinitionId = Lens.lens (\PutQueryDefinitionResponse' {queryDefinitionId} -> queryDefinitionId) (\s@PutQueryDefinitionResponse' {} a -> s {queryDefinitionId = a} :: PutQueryDefinitionResponse)

-- | The response's http status code.
putQueryDefinitionResponse_httpStatus :: Lens.Lens' PutQueryDefinitionResponse Core.Int
putQueryDefinitionResponse_httpStatus = Lens.lens (\PutQueryDefinitionResponse' {httpStatus} -> httpStatus) (\s@PutQueryDefinitionResponse' {} a -> s {httpStatus = a} :: PutQueryDefinitionResponse)

instance Core.NFData PutQueryDefinitionResponse
