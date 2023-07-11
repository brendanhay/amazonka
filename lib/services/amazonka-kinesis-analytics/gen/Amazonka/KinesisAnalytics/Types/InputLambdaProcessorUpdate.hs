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
-- Module      : Amazonka.KinesisAnalytics.Types.InputLambdaProcessorUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.InputLambdaProcessorUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    resourceARNUpdate :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the new IAM role that is used to access the AWS Lambda
    -- function.
    roleARNUpdate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      roleARNUpdate = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the new
-- <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used
-- to preprocess the records in the stream.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: AWS Lambda>
inputLambdaProcessorUpdate_resourceARNUpdate :: Lens.Lens' InputLambdaProcessorUpdate (Prelude.Maybe Prelude.Text)
inputLambdaProcessorUpdate_resourceARNUpdate = Lens.lens (\InputLambdaProcessorUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@InputLambdaProcessorUpdate' {} a -> s {resourceARNUpdate = a} :: InputLambdaProcessorUpdate)

-- | The ARN of the new IAM role that is used to access the AWS Lambda
-- function.
inputLambdaProcessorUpdate_roleARNUpdate :: Lens.Lens' InputLambdaProcessorUpdate (Prelude.Maybe Prelude.Text)
inputLambdaProcessorUpdate_roleARNUpdate = Lens.lens (\InputLambdaProcessorUpdate' {roleARNUpdate} -> roleARNUpdate) (\s@InputLambdaProcessorUpdate' {} a -> s {roleARNUpdate = a} :: InputLambdaProcessorUpdate)

instance Prelude.Hashable InputLambdaProcessorUpdate where
  hashWithSalt _salt InputLambdaProcessorUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` resourceARNUpdate
      `Prelude.hashWithSalt` roleARNUpdate

instance Prelude.NFData InputLambdaProcessorUpdate where
  rnf InputLambdaProcessorUpdate' {..} =
    Prelude.rnf resourceARNUpdate
      `Prelude.seq` Prelude.rnf roleARNUpdate

instance Data.ToJSON InputLambdaProcessorUpdate where
  toJSON InputLambdaProcessorUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceARNUpdate" Data..=)
              Prelude.<$> resourceARNUpdate,
            ("RoleARNUpdate" Data..=) Prelude.<$> roleARNUpdate
          ]
      )
