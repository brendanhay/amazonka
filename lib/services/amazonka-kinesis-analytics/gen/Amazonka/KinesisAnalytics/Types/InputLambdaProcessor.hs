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
-- Module      : Amazonka.KinesisAnalytics.Types.InputLambdaProcessor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.InputLambdaProcessor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used
-- to preprocess records in the stream, and the ARN of the IAM role that is
-- used to access the AWS Lambda function.
--
-- /See:/ 'newInputLambdaProcessor' smart constructor.
data InputLambdaProcessor = InputLambdaProcessor'
  { -- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function
    -- that operates on records in the stream.
    --
    -- To specify an earlier version of the Lambda function than the latest,
    -- include the Lambda function version in the Lambda function ARN. For more
    -- information about Lambda ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
    resourceARN :: Prelude.Text,
    -- | The ARN of the IAM role that is used to access the AWS Lambda function.
    roleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputLambdaProcessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'inputLambdaProcessor_resourceARN' - The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function
-- that operates on records in the stream.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
--
-- 'roleARN', 'inputLambdaProcessor_roleARN' - The ARN of the IAM role that is used to access the AWS Lambda function.
newInputLambdaProcessor ::
  -- | 'resourceARN'
  Prelude.Text ->
  -- | 'roleARN'
  Prelude.Text ->
  InputLambdaProcessor
newInputLambdaProcessor pResourceARN_ pRoleARN_ =
  InputLambdaProcessor'
    { resourceARN = pResourceARN_,
      roleARN = pRoleARN_
    }

-- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function
-- that operates on records in the stream.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
inputLambdaProcessor_resourceARN :: Lens.Lens' InputLambdaProcessor Prelude.Text
inputLambdaProcessor_resourceARN = Lens.lens (\InputLambdaProcessor' {resourceARN} -> resourceARN) (\s@InputLambdaProcessor' {} a -> s {resourceARN = a} :: InputLambdaProcessor)

-- | The ARN of the IAM role that is used to access the AWS Lambda function.
inputLambdaProcessor_roleARN :: Lens.Lens' InputLambdaProcessor Prelude.Text
inputLambdaProcessor_roleARN = Lens.lens (\InputLambdaProcessor' {roleARN} -> roleARN) (\s@InputLambdaProcessor' {} a -> s {roleARN = a} :: InputLambdaProcessor)

instance Prelude.Hashable InputLambdaProcessor where
  hashWithSalt _salt InputLambdaProcessor' {..} =
    _salt
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` roleARN

instance Prelude.NFData InputLambdaProcessor where
  rnf InputLambdaProcessor' {..} =
    Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf roleARN

instance Data.ToJSON InputLambdaProcessor where
  toJSON InputLambdaProcessor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceARN" Data..= resourceARN),
            Prelude.Just ("RoleARN" Data..= roleARN)
          ]
      )
