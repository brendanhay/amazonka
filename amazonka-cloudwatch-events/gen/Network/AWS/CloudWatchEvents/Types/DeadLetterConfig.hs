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
-- Module      : Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.DeadLetterConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A @DeadLetterConfig@ object that contains information about a
-- dead-letter queue configuration.
--
-- /See:/ 'newDeadLetterConfig' smart constructor.
data DeadLetterConfig = DeadLetterConfig'
  { -- | The ARN of the SQS queue specified as the target for the dead-letter
    -- queue.
    arn :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'deadLetterConfig_arn' - The ARN of the SQS queue specified as the target for the dead-letter
-- queue.
newDeadLetterConfig ::
  DeadLetterConfig
newDeadLetterConfig =
  DeadLetterConfig' {arn = Prelude.Nothing}

-- | The ARN of the SQS queue specified as the target for the dead-letter
-- queue.
deadLetterConfig_arn :: Lens.Lens' DeadLetterConfig (Prelude.Maybe Prelude.Text)
deadLetterConfig_arn = Lens.lens (\DeadLetterConfig' {arn} -> arn) (\s@DeadLetterConfig' {} a -> s {arn = a} :: DeadLetterConfig)

instance Prelude.FromJSON DeadLetterConfig where
  parseJSON =
    Prelude.withObject
      "DeadLetterConfig"
      ( \x ->
          DeadLetterConfig' Prelude.<$> (x Prelude..:? "Arn")
      )

instance Prelude.Hashable DeadLetterConfig

instance Prelude.NFData DeadLetterConfig

instance Prelude.ToJSON DeadLetterConfig where
  toJSON DeadLetterConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Arn" Prelude..=) Prelude.<$> arn]
      )
