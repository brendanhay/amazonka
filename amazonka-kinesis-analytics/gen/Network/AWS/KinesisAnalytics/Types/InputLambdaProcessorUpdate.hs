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
-- Module      : Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents an update to the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor>
-- that is used to preprocess the records in the stream.
--
-- /See:/ 'newInputLambdaProcessorUpdate' smart constructor.
data InputLambdaProcessorUpdate = InputLambdaProcessorUpdate'
  { -- | The Amazon Resource Name (ARN) of the new
    -- <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used
    -- to preprocess the records in the stream.
    --
    -- To specify an earlier version of the Lambda function than the latest,
    -- include the Lambda function version in the Lambda function ARN. For more
    -- information about Lambda ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
    resourceARNUpdate :: Core.Maybe Core.Text,
    -- | The ARN of the new IAM role that is used to access the AWS Lambda
    -- function.
    roleARNUpdate :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputLambdaProcessorUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARNUpdate', 'inputLambdaProcessorUpdate_resourceARNUpdate' - The Amazon Resource Name (ARN) of the new
-- <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used
-- to preprocess the records in the stream.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
--
-- 'roleARNUpdate', 'inputLambdaProcessorUpdate_roleARNUpdate' - The ARN of the new IAM role that is used to access the AWS Lambda
-- function.
newInputLambdaProcessorUpdate ::
  InputLambdaProcessorUpdate
newInputLambdaProcessorUpdate =
  InputLambdaProcessorUpdate'
    { resourceARNUpdate =
        Core.Nothing,
      roleARNUpdate = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the new
-- <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used
-- to preprocess the records in the stream.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
inputLambdaProcessorUpdate_resourceARNUpdate :: Lens.Lens' InputLambdaProcessorUpdate (Core.Maybe Core.Text)
inputLambdaProcessorUpdate_resourceARNUpdate = Lens.lens (\InputLambdaProcessorUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@InputLambdaProcessorUpdate' {} a -> s {resourceARNUpdate = a} :: InputLambdaProcessorUpdate)

-- | The ARN of the new IAM role that is used to access the AWS Lambda
-- function.
inputLambdaProcessorUpdate_roleARNUpdate :: Lens.Lens' InputLambdaProcessorUpdate (Core.Maybe Core.Text)
inputLambdaProcessorUpdate_roleARNUpdate = Lens.lens (\InputLambdaProcessorUpdate' {roleARNUpdate} -> roleARNUpdate) (\s@InputLambdaProcessorUpdate' {} a -> s {roleARNUpdate = a} :: InputLambdaProcessorUpdate)

instance Core.Hashable InputLambdaProcessorUpdate

instance Core.NFData InputLambdaProcessorUpdate

instance Core.ToJSON InputLambdaProcessorUpdate where
  toJSON InputLambdaProcessorUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceARNUpdate" Core..=)
              Core.<$> resourceARNUpdate,
            ("RoleARNUpdate" Core..=) Core.<$> roleARNUpdate
          ]
      )
