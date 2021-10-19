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
-- Module      : Network.AWS.ECS.Types.ServiceEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ServiceEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details on an event associated with a service.
--
-- /See:/ 'newServiceEvent' smart constructor.
data ServiceEvent = ServiceEvent'
  { -- | The Unix timestamp for when the event was triggered.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The ID string of the event.
    id :: Prelude.Maybe Prelude.Text,
    -- | The event message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'serviceEvent_createdAt' - The Unix timestamp for when the event was triggered.
--
-- 'id', 'serviceEvent_id' - The ID string of the event.
--
-- 'message', 'serviceEvent_message' - The event message.
newServiceEvent ::
  ServiceEvent
newServiceEvent =
  ServiceEvent'
    { createdAt = Prelude.Nothing,
      id = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The Unix timestamp for when the event was triggered.
serviceEvent_createdAt :: Lens.Lens' ServiceEvent (Prelude.Maybe Prelude.UTCTime)
serviceEvent_createdAt = Lens.lens (\ServiceEvent' {createdAt} -> createdAt) (\s@ServiceEvent' {} a -> s {createdAt = a} :: ServiceEvent) Prelude.. Lens.mapping Core._Time

-- | The ID string of the event.
serviceEvent_id :: Lens.Lens' ServiceEvent (Prelude.Maybe Prelude.Text)
serviceEvent_id = Lens.lens (\ServiceEvent' {id} -> id) (\s@ServiceEvent' {} a -> s {id = a} :: ServiceEvent)

-- | The event message.
serviceEvent_message :: Lens.Lens' ServiceEvent (Prelude.Maybe Prelude.Text)
serviceEvent_message = Lens.lens (\ServiceEvent' {message} -> message) (\s@ServiceEvent' {} a -> s {message = a} :: ServiceEvent)

instance Core.FromJSON ServiceEvent where
  parseJSON =
    Core.withObject
      "ServiceEvent"
      ( \x ->
          ServiceEvent'
            Prelude.<$> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "message")
      )

instance Prelude.Hashable ServiceEvent

instance Prelude.NFData ServiceEvent
