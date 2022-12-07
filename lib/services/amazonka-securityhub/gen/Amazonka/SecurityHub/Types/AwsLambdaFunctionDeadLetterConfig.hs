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
-- Module      : Amazonka.SecurityHub.Types.AwsLambdaFunctionDeadLetterConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsLambdaFunctionDeadLetterConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The dead-letter queue for failed asynchronous invocations.
--
-- /See:/ 'newAwsLambdaFunctionDeadLetterConfig' smart constructor.
data AwsLambdaFunctionDeadLetterConfig = AwsLambdaFunctionDeadLetterConfig'
  { -- | The ARN of an SQS queue or SNS topic.
    targetArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsLambdaFunctionDeadLetterConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetArn', 'awsLambdaFunctionDeadLetterConfig_targetArn' - The ARN of an SQS queue or SNS topic.
newAwsLambdaFunctionDeadLetterConfig ::
  AwsLambdaFunctionDeadLetterConfig
newAwsLambdaFunctionDeadLetterConfig =
  AwsLambdaFunctionDeadLetterConfig'
    { targetArn =
        Prelude.Nothing
    }

-- | The ARN of an SQS queue or SNS topic.
awsLambdaFunctionDeadLetterConfig_targetArn :: Lens.Lens' AwsLambdaFunctionDeadLetterConfig (Prelude.Maybe Prelude.Text)
awsLambdaFunctionDeadLetterConfig_targetArn = Lens.lens (\AwsLambdaFunctionDeadLetterConfig' {targetArn} -> targetArn) (\s@AwsLambdaFunctionDeadLetterConfig' {} a -> s {targetArn = a} :: AwsLambdaFunctionDeadLetterConfig)

instance
  Data.FromJSON
    AwsLambdaFunctionDeadLetterConfig
  where
  parseJSON =
    Data.withObject
      "AwsLambdaFunctionDeadLetterConfig"
      ( \x ->
          AwsLambdaFunctionDeadLetterConfig'
            Prelude.<$> (x Data..:? "TargetArn")
      )

instance
  Prelude.Hashable
    AwsLambdaFunctionDeadLetterConfig
  where
  hashWithSalt
    _salt
    AwsLambdaFunctionDeadLetterConfig' {..} =
      _salt `Prelude.hashWithSalt` targetArn

instance
  Prelude.NFData
    AwsLambdaFunctionDeadLetterConfig
  where
  rnf AwsLambdaFunctionDeadLetterConfig' {..} =
    Prelude.rnf targetArn

instance
  Data.ToJSON
    AwsLambdaFunctionDeadLetterConfig
  where
  toJSON AwsLambdaFunctionDeadLetterConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("TargetArn" Data..=) Prelude.<$> targetArn]
      )
