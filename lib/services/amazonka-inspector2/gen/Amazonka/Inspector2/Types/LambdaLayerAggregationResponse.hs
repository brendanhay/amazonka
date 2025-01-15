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
-- Module      : Amazonka.Inspector2.Types.LambdaLayerAggregationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.LambdaLayerAggregationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | A response that contains the results of an AWS Lambda function layer
-- finding aggregation.
--
-- /See:/ 'newLambdaLayerAggregationResponse' smart constructor.
data LambdaLayerAggregationResponse = LambdaLayerAggregationResponse'
  { severityCounts :: Prelude.Maybe SeverityCounts,
    -- | The account ID of the AWS Lambda function layer.
    accountId :: Prelude.Text,
    -- | The names of the AWS Lambda functions associated with the layers.
    functionName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Lambda function layer.
    layerArn :: Prelude.Text,
    -- | The Resource ID of the AWS Lambda function layer.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaLayerAggregationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'severityCounts', 'lambdaLayerAggregationResponse_severityCounts' - Undocumented member.
--
-- 'accountId', 'lambdaLayerAggregationResponse_accountId' - The account ID of the AWS Lambda function layer.
--
-- 'functionName', 'lambdaLayerAggregationResponse_functionName' - The names of the AWS Lambda functions associated with the layers.
--
-- 'layerArn', 'lambdaLayerAggregationResponse_layerArn' - The Amazon Resource Name (ARN) of the AWS Lambda function layer.
--
-- 'resourceId', 'lambdaLayerAggregationResponse_resourceId' - The Resource ID of the AWS Lambda function layer.
newLambdaLayerAggregationResponse ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'functionName'
  Prelude.Text ->
  -- | 'layerArn'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  LambdaLayerAggregationResponse
newLambdaLayerAggregationResponse
  pAccountId_
  pFunctionName_
  pLayerArn_
  pResourceId_ =
    LambdaLayerAggregationResponse'
      { severityCounts =
          Prelude.Nothing,
        accountId = pAccountId_,
        functionName = pFunctionName_,
        layerArn = pLayerArn_,
        resourceId = pResourceId_
      }

-- | Undocumented member.
lambdaLayerAggregationResponse_severityCounts :: Lens.Lens' LambdaLayerAggregationResponse (Prelude.Maybe SeverityCounts)
lambdaLayerAggregationResponse_severityCounts = Lens.lens (\LambdaLayerAggregationResponse' {severityCounts} -> severityCounts) (\s@LambdaLayerAggregationResponse' {} a -> s {severityCounts = a} :: LambdaLayerAggregationResponse)

-- | The account ID of the AWS Lambda function layer.
lambdaLayerAggregationResponse_accountId :: Lens.Lens' LambdaLayerAggregationResponse Prelude.Text
lambdaLayerAggregationResponse_accountId = Lens.lens (\LambdaLayerAggregationResponse' {accountId} -> accountId) (\s@LambdaLayerAggregationResponse' {} a -> s {accountId = a} :: LambdaLayerAggregationResponse)

-- | The names of the AWS Lambda functions associated with the layers.
lambdaLayerAggregationResponse_functionName :: Lens.Lens' LambdaLayerAggregationResponse Prelude.Text
lambdaLayerAggregationResponse_functionName = Lens.lens (\LambdaLayerAggregationResponse' {functionName} -> functionName) (\s@LambdaLayerAggregationResponse' {} a -> s {functionName = a} :: LambdaLayerAggregationResponse)

-- | The Amazon Resource Name (ARN) of the AWS Lambda function layer.
lambdaLayerAggregationResponse_layerArn :: Lens.Lens' LambdaLayerAggregationResponse Prelude.Text
lambdaLayerAggregationResponse_layerArn = Lens.lens (\LambdaLayerAggregationResponse' {layerArn} -> layerArn) (\s@LambdaLayerAggregationResponse' {} a -> s {layerArn = a} :: LambdaLayerAggregationResponse)

-- | The Resource ID of the AWS Lambda function layer.
lambdaLayerAggregationResponse_resourceId :: Lens.Lens' LambdaLayerAggregationResponse Prelude.Text
lambdaLayerAggregationResponse_resourceId = Lens.lens (\LambdaLayerAggregationResponse' {resourceId} -> resourceId) (\s@LambdaLayerAggregationResponse' {} a -> s {resourceId = a} :: LambdaLayerAggregationResponse)

instance Data.FromJSON LambdaLayerAggregationResponse where
  parseJSON =
    Data.withObject
      "LambdaLayerAggregationResponse"
      ( \x ->
          LambdaLayerAggregationResponse'
            Prelude.<$> (x Data..:? "severityCounts")
            Prelude.<*> (x Data..: "accountId")
            Prelude.<*> (x Data..: "functionName")
            Prelude.<*> (x Data..: "layerArn")
            Prelude.<*> (x Data..: "resourceId")
      )

instance
  Prelude.Hashable
    LambdaLayerAggregationResponse
  where
  hashWithSalt
    _salt
    LambdaLayerAggregationResponse' {..} =
      _salt
        `Prelude.hashWithSalt` severityCounts
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` functionName
        `Prelude.hashWithSalt` layerArn
        `Prelude.hashWithSalt` resourceId

instance
  Prelude.NFData
    LambdaLayerAggregationResponse
  where
  rnf LambdaLayerAggregationResponse' {..} =
    Prelude.rnf severityCounts `Prelude.seq`
      Prelude.rnf accountId `Prelude.seq`
        Prelude.rnf functionName `Prelude.seq`
          Prelude.rnf layerArn `Prelude.seq`
            Prelude.rnf resourceId
