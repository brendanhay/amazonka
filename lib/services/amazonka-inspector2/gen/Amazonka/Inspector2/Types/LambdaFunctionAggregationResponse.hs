{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Inspector2.Types.LambdaFunctionAggregationResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.LambdaFunctionAggregationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | A response that contains the results of an AWS Lambda function finding
-- aggregation.
--
-- /See:/ 'newLambdaFunctionAggregationResponse' smart constructor.
data LambdaFunctionAggregationResponse = LambdaFunctionAggregationResponse'
  { -- | The ID of the AWS account that owns the AWS Lambda function.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The AWS Lambda function names included in the aggregation results.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The tags included in the aggregation results.
    lambdaTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date that the AWS Lambda function included in the aggregation
    -- results was last changed.
    lastModifiedAt :: Prelude.Maybe Data.POSIX,
    -- | The runtimes included in the aggregation results.
    runtime :: Prelude.Maybe Prelude.Text,
    severityCounts :: Prelude.Maybe SeverityCounts,
    -- | The resource IDs included in the aggregation results.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionAggregationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'lambdaFunctionAggregationResponse_accountId' - The ID of the AWS account that owns the AWS Lambda function.
--
-- 'functionName', 'lambdaFunctionAggregationResponse_functionName' - The AWS Lambda function names included in the aggregation results.
--
-- 'lambdaTags', 'lambdaFunctionAggregationResponse_lambdaTags' - The tags included in the aggregation results.
--
-- 'lastModifiedAt', 'lambdaFunctionAggregationResponse_lastModifiedAt' - The date that the AWS Lambda function included in the aggregation
-- results was last changed.
--
-- 'runtime', 'lambdaFunctionAggregationResponse_runtime' - The runtimes included in the aggregation results.
--
-- 'severityCounts', 'lambdaFunctionAggregationResponse_severityCounts' - Undocumented member.
--
-- 'resourceId', 'lambdaFunctionAggregationResponse_resourceId' - The resource IDs included in the aggregation results.
newLambdaFunctionAggregationResponse ::
  -- | 'resourceId'
  Prelude.Text ->
  LambdaFunctionAggregationResponse
newLambdaFunctionAggregationResponse pResourceId_ =
  LambdaFunctionAggregationResponse'
    { accountId =
        Prelude.Nothing,
      functionName = Prelude.Nothing,
      lambdaTags = Prelude.Nothing,
      lastModifiedAt = Prelude.Nothing,
      runtime = Prelude.Nothing,
      severityCounts = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | The ID of the AWS account that owns the AWS Lambda function.
lambdaFunctionAggregationResponse_accountId :: Lens.Lens' LambdaFunctionAggregationResponse (Prelude.Maybe Prelude.Text)
lambdaFunctionAggregationResponse_accountId = Lens.lens (\LambdaFunctionAggregationResponse' {accountId} -> accountId) (\s@LambdaFunctionAggregationResponse' {} a -> s {accountId = a} :: LambdaFunctionAggregationResponse)

-- | The AWS Lambda function names included in the aggregation results.
lambdaFunctionAggregationResponse_functionName :: Lens.Lens' LambdaFunctionAggregationResponse (Prelude.Maybe Prelude.Text)
lambdaFunctionAggregationResponse_functionName = Lens.lens (\LambdaFunctionAggregationResponse' {functionName} -> functionName) (\s@LambdaFunctionAggregationResponse' {} a -> s {functionName = a} :: LambdaFunctionAggregationResponse)

-- | The tags included in the aggregation results.
lambdaFunctionAggregationResponse_lambdaTags :: Lens.Lens' LambdaFunctionAggregationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
lambdaFunctionAggregationResponse_lambdaTags = Lens.lens (\LambdaFunctionAggregationResponse' {lambdaTags} -> lambdaTags) (\s@LambdaFunctionAggregationResponse' {} a -> s {lambdaTags = a} :: LambdaFunctionAggregationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date that the AWS Lambda function included in the aggregation
-- results was last changed.
lambdaFunctionAggregationResponse_lastModifiedAt :: Lens.Lens' LambdaFunctionAggregationResponse (Prelude.Maybe Prelude.UTCTime)
lambdaFunctionAggregationResponse_lastModifiedAt = Lens.lens (\LambdaFunctionAggregationResponse' {lastModifiedAt} -> lastModifiedAt) (\s@LambdaFunctionAggregationResponse' {} a -> s {lastModifiedAt = a} :: LambdaFunctionAggregationResponse) Prelude.. Lens.mapping Data._Time

-- | The runtimes included in the aggregation results.
lambdaFunctionAggregationResponse_runtime :: Lens.Lens' LambdaFunctionAggregationResponse (Prelude.Maybe Prelude.Text)
lambdaFunctionAggregationResponse_runtime = Lens.lens (\LambdaFunctionAggregationResponse' {runtime} -> runtime) (\s@LambdaFunctionAggregationResponse' {} a -> s {runtime = a} :: LambdaFunctionAggregationResponse)

-- | Undocumented member.
lambdaFunctionAggregationResponse_severityCounts :: Lens.Lens' LambdaFunctionAggregationResponse (Prelude.Maybe SeverityCounts)
lambdaFunctionAggregationResponse_severityCounts = Lens.lens (\LambdaFunctionAggregationResponse' {severityCounts} -> severityCounts) (\s@LambdaFunctionAggregationResponse' {} a -> s {severityCounts = a} :: LambdaFunctionAggregationResponse)

-- | The resource IDs included in the aggregation results.
lambdaFunctionAggregationResponse_resourceId :: Lens.Lens' LambdaFunctionAggregationResponse Prelude.Text
lambdaFunctionAggregationResponse_resourceId = Lens.lens (\LambdaFunctionAggregationResponse' {resourceId} -> resourceId) (\s@LambdaFunctionAggregationResponse' {} a -> s {resourceId = a} :: LambdaFunctionAggregationResponse)

instance
  Data.FromJSON
    LambdaFunctionAggregationResponse
  where
  parseJSON =
    Data.withObject
      "LambdaFunctionAggregationResponse"
      ( \x ->
          LambdaFunctionAggregationResponse'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "functionName")
            Prelude.<*> (x Data..:? "lambdaTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "lastModifiedAt")
            Prelude.<*> (x Data..:? "runtime")
            Prelude.<*> (x Data..:? "severityCounts")
            Prelude.<*> (x Data..: "resourceId")
      )

instance
  Prelude.Hashable
    LambdaFunctionAggregationResponse
  where
  hashWithSalt
    _salt
    LambdaFunctionAggregationResponse' {..} =
      _salt `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` functionName
        `Prelude.hashWithSalt` lambdaTags
        `Prelude.hashWithSalt` lastModifiedAt
        `Prelude.hashWithSalt` runtime
        `Prelude.hashWithSalt` severityCounts
        `Prelude.hashWithSalt` resourceId

instance
  Prelude.NFData
    LambdaFunctionAggregationResponse
  where
  rnf LambdaFunctionAggregationResponse' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf lambdaTags
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf severityCounts
      `Prelude.seq` Prelude.rnf resourceId
