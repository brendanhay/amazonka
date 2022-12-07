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
-- Module      : Amazonka.KinesisAnalytics.Types.InputLambdaProcessorDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.InputLambdaProcessorDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used
-- to preprocess records in the stream, and the ARN of the IAM role that is
-- used to access the AWS Lambda expression.
--
-- /See:/ 'newInputLambdaProcessorDescription' smart constructor.
data InputLambdaProcessorDescription = InputLambdaProcessorDescription'
  { -- | The ARN of the IAM role that is used to access the AWS Lambda function.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function
    -- that is used to preprocess the records in the stream.
    resourceARN :: Prelude.Maybe Prelude.Text
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
-- 'roleARN', 'inputLambdaProcessorDescription_roleARN' - The ARN of the IAM role that is used to access the AWS Lambda function.
--
-- 'resourceARN', 'inputLambdaProcessorDescription_resourceARN' - The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function
-- that is used to preprocess the records in the stream.
newInputLambdaProcessorDescription ::
  InputLambdaProcessorDescription
newInputLambdaProcessorDescription =
  InputLambdaProcessorDescription'
    { roleARN =
        Prelude.Nothing,
      resourceARN = Prelude.Nothing
    }

-- | The ARN of the IAM role that is used to access the AWS Lambda function.
inputLambdaProcessorDescription_roleARN :: Lens.Lens' InputLambdaProcessorDescription (Prelude.Maybe Prelude.Text)
inputLambdaProcessorDescription_roleARN = Lens.lens (\InputLambdaProcessorDescription' {roleARN} -> roleARN) (\s@InputLambdaProcessorDescription' {} a -> s {roleARN = a} :: InputLambdaProcessorDescription)

-- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function
-- that is used to preprocess the records in the stream.
inputLambdaProcessorDescription_resourceARN :: Lens.Lens' InputLambdaProcessorDescription (Prelude.Maybe Prelude.Text)
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
            Prelude.<*> (x Data..:? "ResourceARN")
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
