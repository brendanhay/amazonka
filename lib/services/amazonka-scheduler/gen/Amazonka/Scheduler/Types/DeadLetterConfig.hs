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
-- Module      : Amazonka.Scheduler.Types.DeadLetterConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.DeadLetterConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about an Amazon SQS queue that
-- EventBridge Scheduler uses as a dead-letter queue for your schedule. If
-- specified, EventBridge Scheduler delivers failed events that could not
-- be successfully delivered to a target to the queue.
--
-- /See:/ 'newDeadLetterConfig' smart constructor.
data DeadLetterConfig = DeadLetterConfig'
  { -- | The Amazon Resource Name (ARN) of the SQS queue specified as the
    -- destination for the dead-letter queue.
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
-- 'arn', 'deadLetterConfig_arn' - The Amazon Resource Name (ARN) of the SQS queue specified as the
-- destination for the dead-letter queue.
newDeadLetterConfig ::
  DeadLetterConfig
newDeadLetterConfig =
  DeadLetterConfig' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the SQS queue specified as the
-- destination for the dead-letter queue.
deadLetterConfig_arn :: Lens.Lens' DeadLetterConfig (Prelude.Maybe Prelude.Text)
deadLetterConfig_arn = Lens.lens (\DeadLetterConfig' {arn} -> arn) (\s@DeadLetterConfig' {} a -> s {arn = a} :: DeadLetterConfig)

instance Data.FromJSON DeadLetterConfig where
  parseJSON =
    Data.withObject
      "DeadLetterConfig"
      ( \x ->
          DeadLetterConfig' Prelude.<$> (x Data..:? "Arn")
      )

instance Prelude.Hashable DeadLetterConfig where
  hashWithSalt _salt DeadLetterConfig' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeadLetterConfig where
  rnf DeadLetterConfig' {..} = Prelude.rnf arn

instance Data.ToJSON DeadLetterConfig where
  toJSON DeadLetterConfig' {..} =
    Data.object
      (Prelude.catMaybes [("Arn" Data..=) Prelude.<$> arn])
