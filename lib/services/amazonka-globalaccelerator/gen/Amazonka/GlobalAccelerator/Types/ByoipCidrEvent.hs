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
-- Module      : Amazonka.GlobalAccelerator.Types.ByoipCidrEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.ByoipCidrEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains a @Message@ and a @Timestamp@ value for
-- changes that you make in the status of an IP address range that you
-- bring to Global Accelerator through bring your own IP address (BYOIP).
--
-- /See:/ 'newByoipCidrEvent' smart constructor.
data ByoipCidrEvent = ByoipCidrEvent'
  { -- | A string that contains an @Event@ message describing changes that you
    -- make in the status of an IP address range that you bring to Global
    -- Accelerator through bring your own IP address (BYOIP).
    message :: Prelude.Maybe Prelude.Text,
    -- | A timestamp for when you make a status change for an IP address range
    -- that you bring to Global Accelerator through bring your own IP address
    -- (BYOIP).
    timestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ByoipCidrEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'byoipCidrEvent_message' - A string that contains an @Event@ message describing changes that you
-- make in the status of an IP address range that you bring to Global
-- Accelerator through bring your own IP address (BYOIP).
--
-- 'timestamp', 'byoipCidrEvent_timestamp' - A timestamp for when you make a status change for an IP address range
-- that you bring to Global Accelerator through bring your own IP address
-- (BYOIP).
newByoipCidrEvent ::
  ByoipCidrEvent
newByoipCidrEvent =
  ByoipCidrEvent'
    { message = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | A string that contains an @Event@ message describing changes that you
-- make in the status of an IP address range that you bring to Global
-- Accelerator through bring your own IP address (BYOIP).
byoipCidrEvent_message :: Lens.Lens' ByoipCidrEvent (Prelude.Maybe Prelude.Text)
byoipCidrEvent_message = Lens.lens (\ByoipCidrEvent' {message} -> message) (\s@ByoipCidrEvent' {} a -> s {message = a} :: ByoipCidrEvent)

-- | A timestamp for when you make a status change for an IP address range
-- that you bring to Global Accelerator through bring your own IP address
-- (BYOIP).
byoipCidrEvent_timestamp :: Lens.Lens' ByoipCidrEvent (Prelude.Maybe Prelude.UTCTime)
byoipCidrEvent_timestamp = Lens.lens (\ByoipCidrEvent' {timestamp} -> timestamp) (\s@ByoipCidrEvent' {} a -> s {timestamp = a} :: ByoipCidrEvent) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ByoipCidrEvent where
  parseJSON =
    Core.withObject
      "ByoipCidrEvent"
      ( \x ->
          ByoipCidrEvent'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Timestamp")
      )

instance Prelude.Hashable ByoipCidrEvent where
  hashWithSalt _salt ByoipCidrEvent' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData ByoipCidrEvent where
  rnf ByoipCidrEvent' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf timestamp
