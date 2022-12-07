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
-- Module      : Amazonka.KinesisAnalytics.Types.LambdaOutputUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.LambdaOutputUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When updating an output configuration using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication>
-- operation, provides information about an AWS Lambda function configured
-- as the destination.
--
-- /See:/ 'newLambdaOutputUpdate' smart constructor.
data LambdaOutputUpdate = LambdaOutputUpdate'
  { -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
    -- the destination function on your behalf. You need to grant the necessary
    -- permissions to this role.
    roleARNUpdate :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the destination Lambda function.
    --
    -- To specify an earlier version of the Lambda function than the latest,
    -- include the Lambda function version in the Lambda function ARN. For more
    -- information about Lambda ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
    resourceARNUpdate :: Prelude.Maybe Prelude.Text
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
-- 'roleARNUpdate', 'lambdaOutputUpdate_roleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
-- the destination function on your behalf. You need to grant the necessary
-- permissions to this role.
--
-- 'resourceARNUpdate', 'lambdaOutputUpdate_resourceARNUpdate' - Amazon Resource Name (ARN) of the destination Lambda function.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
newLambdaOutputUpdate ::
  LambdaOutputUpdate
newLambdaOutputUpdate =
  LambdaOutputUpdate'
    { roleARNUpdate =
        Prelude.Nothing,
      resourceARNUpdate = Prelude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
-- the destination function on your behalf. You need to grant the necessary
-- permissions to this role.
lambdaOutputUpdate_roleARNUpdate :: Lens.Lens' LambdaOutputUpdate (Prelude.Maybe Prelude.Text)
lambdaOutputUpdate_roleARNUpdate = Lens.lens (\LambdaOutputUpdate' {roleARNUpdate} -> roleARNUpdate) (\s@LambdaOutputUpdate' {} a -> s {roleARNUpdate = a} :: LambdaOutputUpdate)

-- | Amazon Resource Name (ARN) of the destination Lambda function.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
lambdaOutputUpdate_resourceARNUpdate :: Lens.Lens' LambdaOutputUpdate (Prelude.Maybe Prelude.Text)
lambdaOutputUpdate_resourceARNUpdate = Lens.lens (\LambdaOutputUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@LambdaOutputUpdate' {} a -> s {resourceARNUpdate = a} :: LambdaOutputUpdate)

instance Prelude.Hashable LambdaOutputUpdate where
  hashWithSalt _salt LambdaOutputUpdate' {..} =
    _salt `Prelude.hashWithSalt` roleARNUpdate
      `Prelude.hashWithSalt` resourceARNUpdate

instance Prelude.NFData LambdaOutputUpdate where
  rnf LambdaOutputUpdate' {..} =
    Prelude.rnf roleARNUpdate
      `Prelude.seq` Prelude.rnf resourceARNUpdate

instance Data.ToJSON LambdaOutputUpdate where
  toJSON LambdaOutputUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoleARNUpdate" Data..=) Prelude.<$> roleARNUpdate,
            ("ResourceARNUpdate" Data..=)
              Prelude.<$> resourceARNUpdate
          ]
      )
