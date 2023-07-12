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
-- Module      : Amazonka.GlobalAccelerator.Types.AcceleratorEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.AcceleratorEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains a @Timestamp@ value and @Message@ for
-- changes that you make to an accelerator in Global Accelerator. Messages
-- stored here provide progress or error information when you update an
-- accelerator from IPv4 to dual-stack, or from dual-stack to IPv4. Global
-- Accelerator stores a maximum of ten event messages.
--
-- /See:/ 'newAcceleratorEvent' smart constructor.
data AcceleratorEvent = AcceleratorEvent'
  { -- | A string that contains an @Event@ message describing changes or errors
    -- when you update an accelerator in Global Accelerator from IPv4 to
    -- dual-stack, or dual-stack to IPv4.
    message :: Prelude.Maybe Prelude.Text,
    -- | A timestamp for when you update an accelerator in Global Accelerator
    -- from IPv4 to dual-stack, or dual-stack to IPv4.
    timestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceleratorEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'acceleratorEvent_message' - A string that contains an @Event@ message describing changes or errors
-- when you update an accelerator in Global Accelerator from IPv4 to
-- dual-stack, or dual-stack to IPv4.
--
-- 'timestamp', 'acceleratorEvent_timestamp' - A timestamp for when you update an accelerator in Global Accelerator
-- from IPv4 to dual-stack, or dual-stack to IPv4.
newAcceleratorEvent ::
  AcceleratorEvent
newAcceleratorEvent =
  AcceleratorEvent'
    { message = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | A string that contains an @Event@ message describing changes or errors
-- when you update an accelerator in Global Accelerator from IPv4 to
-- dual-stack, or dual-stack to IPv4.
acceleratorEvent_message :: Lens.Lens' AcceleratorEvent (Prelude.Maybe Prelude.Text)
acceleratorEvent_message = Lens.lens (\AcceleratorEvent' {message} -> message) (\s@AcceleratorEvent' {} a -> s {message = a} :: AcceleratorEvent)

-- | A timestamp for when you update an accelerator in Global Accelerator
-- from IPv4 to dual-stack, or dual-stack to IPv4.
acceleratorEvent_timestamp :: Lens.Lens' AcceleratorEvent (Prelude.Maybe Prelude.UTCTime)
acceleratorEvent_timestamp = Lens.lens (\AcceleratorEvent' {timestamp} -> timestamp) (\s@AcceleratorEvent' {} a -> s {timestamp = a} :: AcceleratorEvent) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AcceleratorEvent where
  parseJSON =
    Data.withObject
      "AcceleratorEvent"
      ( \x ->
          AcceleratorEvent'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable AcceleratorEvent where
  hashWithSalt _salt AcceleratorEvent' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData AcceleratorEvent where
  rnf AcceleratorEvent' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf timestamp
