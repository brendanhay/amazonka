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
-- Module      : Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object that contains the Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used
-- to preprocess records in the stream, and the ARN of the IAM role that is
-- used to access the AWS Lambda expression.
--
-- /See:/ 'newInputLambdaProcessorDescription' smart constructor.
data InputLambdaProcessorDescription = InputLambdaProcessorDescription'
  { -- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function
    -- that is used to preprocess the records in the stream.
    resourceARN :: Core.Maybe Core.Text,
    -- | The ARN of the IAM role that is used to access the AWS Lambda function.
    roleARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputLambdaProcessorDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'inputLambdaProcessorDescription_resourceARN' - The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function
-- that is used to preprocess the records in the stream.
--
-- 'roleARN', 'inputLambdaProcessorDescription_roleARN' - The ARN of the IAM role that is used to access the AWS Lambda function.
newInputLambdaProcessorDescription ::
  InputLambdaProcessorDescription
newInputLambdaProcessorDescription =
  InputLambdaProcessorDescription'
    { resourceARN =
        Core.Nothing,
      roleARN = Core.Nothing
    }

-- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function
-- that is used to preprocess the records in the stream.
inputLambdaProcessorDescription_resourceARN :: Lens.Lens' InputLambdaProcessorDescription (Core.Maybe Core.Text)
inputLambdaProcessorDescription_resourceARN = Lens.lens (\InputLambdaProcessorDescription' {resourceARN} -> resourceARN) (\s@InputLambdaProcessorDescription' {} a -> s {resourceARN = a} :: InputLambdaProcessorDescription)

-- | The ARN of the IAM role that is used to access the AWS Lambda function.
inputLambdaProcessorDescription_roleARN :: Lens.Lens' InputLambdaProcessorDescription (Core.Maybe Core.Text)
inputLambdaProcessorDescription_roleARN = Lens.lens (\InputLambdaProcessorDescription' {roleARN} -> roleARN) (\s@InputLambdaProcessorDescription' {} a -> s {roleARN = a} :: InputLambdaProcessorDescription)

instance
  Core.FromJSON
    InputLambdaProcessorDescription
  where
  parseJSON =
    Core.withObject
      "InputLambdaProcessorDescription"
      ( \x ->
          InputLambdaProcessorDescription'
            Core.<$> (x Core..:? "ResourceARN")
            Core.<*> (x Core..:? "RoleARN")
      )

instance
  Core.Hashable
    InputLambdaProcessorDescription

instance Core.NFData InputLambdaProcessorDescription
