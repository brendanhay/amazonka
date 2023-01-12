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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.InputLambdaProcessorDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.InputLambdaProcessorDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, an object that
-- contains the Amazon Resource Name (ARN) of the Amazon Lambda function
-- that is used to preprocess records in the stream.
--
-- /See:/ 'newInputLambdaProcessorDescription' smart constructor.
data InputLambdaProcessorDescription = InputLambdaProcessorDescription'
  { -- | The ARN of the IAM role that is used to access the Amazon Lambda
    -- function.
    --
    -- Provided for backward compatibility. Applications that are created with
    -- the current API version have an application-level service execution role
    -- rather than a resource-level role.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon Lambda function that is used to preprocess the
    -- records in the stream.
    --
    -- To specify an earlier version of the Lambda function than the latest,
    -- include the Lambda function version in the Lambda function ARN. For more
    -- information about Lambda ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: Amazon Lambda>
    resourceARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputLambdaProcessorDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'inputLambdaProcessorDescription_roleARN' - The ARN of the IAM role that is used to access the Amazon Lambda
-- function.
--
-- Provided for backward compatibility. Applications that are created with
-- the current API version have an application-level service execution role
-- rather than a resource-level role.
--
-- 'resourceARN', 'inputLambdaProcessorDescription_resourceARN' - The ARN of the Amazon Lambda function that is used to preprocess the
-- records in the stream.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: Amazon Lambda>
newInputLambdaProcessorDescription ::
  -- | 'resourceARN'
  Prelude.Text ->
  InputLambdaProcessorDescription
newInputLambdaProcessorDescription pResourceARN_ =
  InputLambdaProcessorDescription'
    { roleARN =
        Prelude.Nothing,
      resourceARN = pResourceARN_
    }

-- | The ARN of the IAM role that is used to access the Amazon Lambda
-- function.
--
-- Provided for backward compatibility. Applications that are created with
-- the current API version have an application-level service execution role
-- rather than a resource-level role.
inputLambdaProcessorDescription_roleARN :: Lens.Lens' InputLambdaProcessorDescription (Prelude.Maybe Prelude.Text)
inputLambdaProcessorDescription_roleARN = Lens.lens (\InputLambdaProcessorDescription' {roleARN} -> roleARN) (\s@InputLambdaProcessorDescription' {} a -> s {roleARN = a} :: InputLambdaProcessorDescription)

-- | The ARN of the Amazon Lambda function that is used to preprocess the
-- records in the stream.
--
-- To specify an earlier version of the Lambda function than the latest,
-- include the Lambda function version in the Lambda function ARN. For more
-- information about Lambda ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-lambda Example ARNs: Amazon Lambda>
inputLambdaProcessorDescription_resourceARN :: Lens.Lens' InputLambdaProcessorDescription Prelude.Text
inputLambdaProcessorDescription_resourceARN = Lens.lens (\InputLambdaProcessorDescription' {resourceARN} -> resourceARN) (\s@InputLambdaProcessorDescription' {} a -> s {resourceARN = a} :: InputLambdaProcessorDescription)

instance
  Data.FromJSON
    InputLambdaProcessorDescription
  where
  parseJSON =
    Data.withObject
      "InputLambdaProcessorDescription"
      ( \x ->
          InputLambdaProcessorDescription'
            Prelude.<$> (x Data..:? "RoleARN")
            Prelude.<*> (x Data..: "ResourceARN")
      )

instance
  Prelude.Hashable
    InputLambdaProcessorDescription
  where
  hashWithSalt
    _salt
    InputLambdaProcessorDescription' {..} =
      _salt `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` resourceARN

instance
  Prelude.NFData
    InputLambdaProcessorDescription
  where
  rnf InputLambdaProcessorDescription' {..} =
    Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf resourceARN
