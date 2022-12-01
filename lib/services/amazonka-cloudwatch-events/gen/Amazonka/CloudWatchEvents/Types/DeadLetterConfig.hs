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
-- Module      : Amazonka.CloudWatchEvents.Types.DeadLetterConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.DeadLetterConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A @DeadLetterConfig@ object that contains information about a
-- dead-letter queue configuration.
--
-- /See:/ 'newDeadLetterConfig' smart constructor.
data DeadLetterConfig = DeadLetterConfig'
  { -- | The ARN of the SQS queue specified as the target for the dead-letter
    -- queue.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.FromJSON DeadLetterConfig where
  parseJSON =
    Core.withObject
      "DeadLetterConfig"
      ( \x ->
          DeadLetterConfig' Prelude.<$> (x Core..:? "Arn")
      )

instance Prelude.Hashable DeadLetterConfig where
  hashWithSalt _salt DeadLetterConfig' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeadLetterConfig where
  rnf DeadLetterConfig' {..} = Prelude.rnf arn

instance Core.ToJSON DeadLetterConfig where
  toJSON DeadLetterConfig' {..} =
    Core.object
      (Prelude.catMaybes [("Arn" Core..=) Prelude.<$> arn])
