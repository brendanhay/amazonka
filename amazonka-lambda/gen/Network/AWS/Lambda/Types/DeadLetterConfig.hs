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
-- Module      : Network.AWS.Lambda.Types.DeadLetterConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.DeadLetterConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq dead-letter queue>
-- for failed asynchronous invocations.
--
-- /See:/ 'newDeadLetterConfig' smart constructor.
data DeadLetterConfig = DeadLetterConfig'
  { -- | The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS
    -- topic.
    targetArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeadLetterConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetArn', 'deadLetterConfig_targetArn' - The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS
-- topic.
newDeadLetterConfig ::
  DeadLetterConfig
newDeadLetterConfig =
  DeadLetterConfig' {targetArn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS
-- topic.
deadLetterConfig_targetArn :: Lens.Lens' DeadLetterConfig (Prelude.Maybe Prelude.Text)
deadLetterConfig_targetArn = Lens.lens (\DeadLetterConfig' {targetArn} -> targetArn) (\s@DeadLetterConfig' {} a -> s {targetArn = a} :: DeadLetterConfig)

instance Prelude.FromJSON DeadLetterConfig where
  parseJSON =
    Prelude.withObject
      "DeadLetterConfig"
      ( \x ->
          DeadLetterConfig'
            Prelude.<$> (x Prelude..:? "TargetArn")
      )

instance Prelude.Hashable DeadLetterConfig

instance Prelude.NFData DeadLetterConfig

instance Prelude.ToJSON DeadLetterConfig where
  toJSON DeadLetterConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("TargetArn" Prelude..=) Prelude.<$> targetArn]
      )
