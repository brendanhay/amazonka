{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KinesisAnalytics.Types.LambdaOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.LambdaOutput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | When configuring application output, identifies an AWS Lambda function
-- as the destination. You provide the function Amazon Resource Name (ARN)
-- and also an IAM role ARN that Amazon Kinesis Analytics can use to write
-- to the function on your behalf.
--
-- /See:/ 'newLambdaOutput' smart constructor.
data LambdaOutput = LambdaOutput'
  { -- | Amazon Resource Name (ARN) of the destination Lambda function to write
    -- to.
    --
    -- To specify an earlier version of the Lambda function than the latest,
    -- include the Lambda function version in the Lambda function ARN. For more
    -- information about Lambda ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
    resourceARN :: Prelude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
    -- the destination function on your behalf. You need to grant the necessary
    -- permissions to this role.
    roleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LambdaOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'lambdaOutput_resourceARN' - Amazon Resource Name (ARN) of the destination Lambda function to write
-- to.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
--
-- 'roleARN', 'lambdaOutput_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
-- the destination function on your behalf. You need to grant the necessary
-- permissions to this role.
newLambdaOutput ::
  -- | 'resourceARN'
  Prelude.Text ->
  -- | 'roleARN'
  Prelude.Text ->
  LambdaOutput
newLambdaOutput pResourceARN_ pRoleARN_ =
  LambdaOutput'
    { resourceARN = pResourceARN_,
      roleARN = pRoleARN_
    }

-- | Amazon Resource Name (ARN) of the destination Lambda function to write
-- to.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
lambdaOutput_resourceARN :: Lens.Lens' LambdaOutput Prelude.Text
lambdaOutput_resourceARN = Lens.lens (\LambdaOutput' {resourceARN} -> resourceARN) (\s@LambdaOutput' {} a -> s {resourceARN = a} :: LambdaOutput)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
-- the destination function on your behalf. You need to grant the necessary
-- permissions to this role.
lambdaOutput_roleARN :: Lens.Lens' LambdaOutput Prelude.Text
lambdaOutput_roleARN = Lens.lens (\LambdaOutput' {roleARN} -> roleARN) (\s@LambdaOutput' {} a -> s {roleARN = a} :: LambdaOutput)

instance Prelude.Hashable LambdaOutput

instance Prelude.NFData LambdaOutput

instance Prelude.ToJSON LambdaOutput where
  toJSON LambdaOutput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceARN" Prelude..= resourceARN),
            Prelude.Just ("RoleARN" Prelude..= roleARN)
          ]
      )
