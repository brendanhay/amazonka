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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.LambdaOutputUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.LambdaOutputUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When you update an SQL-based Kinesis Data Analytics application\'s
-- output configuration using the UpdateApplication operation, provides
-- information about an Amazon Lambda function that is configured as the
-- destination.
--
-- /See:/ 'newLambdaOutputUpdate' smart constructor.
data LambdaOutputUpdate = LambdaOutputUpdate'
  { -- | The Amazon Resource Name (ARN) of the destination Amazon Lambda
    -- function.
    --
    -- To specify an earlier version of the Lambda function than the latest,
    -- include the Lambda function version in the Lambda function ARN. For more
    -- information about Lambda ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: Amazon Lambda>
    resourceARNUpdate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaOutputUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARNUpdate', 'lambdaOutputUpdate_resourceARNUpdate' - The Amazon Resource Name (ARN) of the destination Amazon Lambda
-- function.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: Amazon Lambda>
newLambdaOutputUpdate ::
  -- | 'resourceARNUpdate'
  Prelude.Text ->
  LambdaOutputUpdate
newLambdaOutputUpdate pResourceARNUpdate_ =
  LambdaOutputUpdate'
    { resourceARNUpdate =
        pResourceARNUpdate_
    }

-- | The Amazon Resource Name (ARN) of the destination Amazon Lambda
-- function.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: Amazon Lambda>
lambdaOutputUpdate_resourceARNUpdate :: Lens.Lens' LambdaOutputUpdate Prelude.Text
lambdaOutputUpdate_resourceARNUpdate = Lens.lens (\LambdaOutputUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@LambdaOutputUpdate' {} a -> s {resourceARNUpdate = a} :: LambdaOutputUpdate)

instance Prelude.Hashable LambdaOutputUpdate where
  hashWithSalt _salt LambdaOutputUpdate' {..} =
    _salt `Prelude.hashWithSalt` resourceARNUpdate

instance Prelude.NFData LambdaOutputUpdate where
  rnf LambdaOutputUpdate' {..} =
    Prelude.rnf resourceARNUpdate

instance Data.ToJSON LambdaOutputUpdate where
  toJSON LambdaOutputUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceARNUpdate" Data..= resourceARNUpdate)
          ]
      )
