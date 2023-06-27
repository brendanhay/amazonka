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
-- Module      : Amazonka.CloudWatchLogs.PutQueryDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- example, imagine updating a current query definition that includes log
-- groups. If you don\'t specify the @logGroupNames@ parameter in your
-- update operation, the query definition changes to contain no log groups.
--
-- You must have the @logs:PutQueryDefinition@ permission to be able to
-- perform this operation.
module Amazonka.CloudWatchLogs.PutQueryDefinition
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutQueryDefinition' smart constructor.
data PutQueryDefinition = PutQueryDefinition'
  { -- | Use this parameter to include specific log groups as part of your query
    -- definition.
    --
    -- If you are updating a query definition and you omit this parameter, then
    -- the updated definition will contain no log groups.
    logGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | If you are updating a query definition, use this parameter to specify
    -- the ID of the query definition that you want to update. You can use
    -- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>
    -- to retrieve the IDs of your saved query definitions.
    --
    -- If you are creating a query definition, do not specify this parameter.
    -- CloudWatch generates a unique ID for the new query definition and
    -- include it in the response to this operation.
    queryDefinitionId :: Prelude.Maybe Prelude.Text,
    -- | A name for the query definition. If you are saving numerous query
    -- definitions, we recommend that you name them. This way, you can find the
    -- ones you want by using the first part of the name as a filter in the
    -- @queryDefinitionNamePrefix@ parameter of
    -- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>.
    name :: Prelude.Text,
    -- | The query string to use for this definition. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
    queryString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'name', 'putQueryDefinition_name' - A name for the query definition. If you are saving numerous query
-- definitions, we recommend that you name them. This way, you can find the
-- ones you want by using the first part of the name as a filter in the
-- @queryDefinitionNamePrefix@ parameter of
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>.
--
-- 'queryString', 'putQueryDefinition_queryString' - The query string to use for this definition. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
newPutQueryDefinition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'queryString'
  Prelude.Text ->
  PutQueryDefinition
newPutQueryDefinition pName_ pQueryString_ =
  PutQueryDefinition'
    { logGroupNames =
        Prelude.Nothing,
      queryDefinitionId = Prelude.Nothing,
      name = pName_,
      queryString = pQueryString_
    }

-- | Use this parameter to include specific log groups as part of your query
-- definition.
--
-- If you are updating a query definition and you omit this parameter, then
-- the updated definition will contain no log groups.
putQueryDefinition_logGroupNames :: Lens.Lens' PutQueryDefinition (Prelude.Maybe [Prelude.Text])
putQueryDefinition_logGroupNames = Lens.lens (\PutQueryDefinition' {logGroupNames} -> logGroupNames) (\s@PutQueryDefinition' {} a -> s {logGroupNames = a} :: PutQueryDefinition) Prelude.. Lens.mapping Lens.coerced

-- | If you are updating a query definition, use this parameter to specify
-- the ID of the query definition that you want to update. You can use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>
-- to retrieve the IDs of your saved query definitions.
--
-- If you are creating a query definition, do not specify this parameter.
-- CloudWatch generates a unique ID for the new query definition and
-- include it in the response to this operation.
putQueryDefinition_queryDefinitionId :: Lens.Lens' PutQueryDefinition (Prelude.Maybe Prelude.Text)
putQueryDefinition_queryDefinitionId = Lens.lens (\PutQueryDefinition' {queryDefinitionId} -> queryDefinitionId) (\s@PutQueryDefinition' {} a -> s {queryDefinitionId = a} :: PutQueryDefinition)

-- | A name for the query definition. If you are saving numerous query
-- definitions, we recommend that you name them. This way, you can find the
-- ones you want by using the first part of the name as a filter in the
-- @queryDefinitionNamePrefix@ parameter of
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions>.
putQueryDefinition_name :: Lens.Lens' PutQueryDefinition Prelude.Text
putQueryDefinition_name = Lens.lens (\PutQueryDefinition' {name} -> name) (\s@PutQueryDefinition' {} a -> s {name = a} :: PutQueryDefinition)

-- | The query string to use for this definition. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax>.
putQueryDefinition_queryString :: Lens.Lens' PutQueryDefinition Prelude.Text
putQueryDefinition_queryString = Lens.lens (\PutQueryDefinition' {queryString} -> queryString) (\s@PutQueryDefinition' {} a -> s {queryString = a} :: PutQueryDefinition)

instance Core.AWSRequest PutQueryDefinition where
  type
    AWSResponse PutQueryDefinition =
      PutQueryDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutQueryDefinitionResponse'
            Prelude.<$> (x Data..?> "queryDefinitionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutQueryDefinition where
  hashWithSalt _salt PutQueryDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` logGroupNames
      `Prelude.hashWithSalt` queryDefinitionId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData PutQueryDefinition where
  rnf PutQueryDefinition' {..} =
    Prelude.rnf logGroupNames
      `Prelude.seq` Prelude.rnf queryDefinitionId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf queryString

instance Data.ToHeaders PutQueryDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.PutQueryDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutQueryDefinition where
  toJSON PutQueryDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("logGroupNames" Data..=) Prelude.<$> logGroupNames,
            ("queryDefinitionId" Data..=)
              Prelude.<$> queryDefinitionId,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("queryString" Data..= queryString)
          ]
      )

instance Data.ToPath PutQueryDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery PutQueryDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutQueryDefinitionResponse' smart constructor.
data PutQueryDefinitionResponse = PutQueryDefinitionResponse'
  { -- | The ID of the query definition.
    queryDefinitionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutQueryDefinitionResponse
newPutQueryDefinitionResponse pHttpStatus_ =
  PutQueryDefinitionResponse'
    { queryDefinitionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the query definition.
putQueryDefinitionResponse_queryDefinitionId :: Lens.Lens' PutQueryDefinitionResponse (Prelude.Maybe Prelude.Text)
putQueryDefinitionResponse_queryDefinitionId = Lens.lens (\PutQueryDefinitionResponse' {queryDefinitionId} -> queryDefinitionId) (\s@PutQueryDefinitionResponse' {} a -> s {queryDefinitionId = a} :: PutQueryDefinitionResponse)

-- | The response's http status code.
putQueryDefinitionResponse_httpStatus :: Lens.Lens' PutQueryDefinitionResponse Prelude.Int
putQueryDefinitionResponse_httpStatus = Lens.lens (\PutQueryDefinitionResponse' {httpStatus} -> httpStatus) (\s@PutQueryDefinitionResponse' {} a -> s {httpStatus = a} :: PutQueryDefinitionResponse)

instance Prelude.NFData PutQueryDefinitionResponse where
  rnf PutQueryDefinitionResponse' {..} =
    Prelude.rnf queryDefinitionId
      `Prelude.seq` Prelude.rnf httpStatus
