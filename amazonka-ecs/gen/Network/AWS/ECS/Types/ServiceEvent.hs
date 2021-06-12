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

-- | Details on an event associated with a service.
--
-- /See:/ 'newServiceEvent' smart constructor.
data ServiceEvent = ServiceEvent'
  { -- | The event message.
    message :: Core.Maybe Core.Text,
    -- | The Unix timestamp for when the event was triggered.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The ID string of the event.
    id :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServiceEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'serviceEvent_message' - The event message.
--
-- 'createdAt', 'serviceEvent_createdAt' - The Unix timestamp for when the event was triggered.
--
-- 'id', 'serviceEvent_id' - The ID string of the event.
newServiceEvent ::
  ServiceEvent
newServiceEvent =
  ServiceEvent'
    { message = Core.Nothing,
      createdAt = Core.Nothing,
      id = Core.Nothing
    }

-- | The event message.
serviceEvent_message :: Lens.Lens' ServiceEvent (Core.Maybe Core.Text)
serviceEvent_message = Lens.lens (\ServiceEvent' {message} -> message) (\s@ServiceEvent' {} a -> s {message = a} :: ServiceEvent)

-- | The Unix timestamp for when the event was triggered.
serviceEvent_createdAt :: Lens.Lens' ServiceEvent (Core.Maybe Core.UTCTime)
serviceEvent_createdAt = Lens.lens (\ServiceEvent' {createdAt} -> createdAt) (\s@ServiceEvent' {} a -> s {createdAt = a} :: ServiceEvent) Core.. Lens.mapping Core._Time

-- | The ID string of the event.
serviceEvent_id :: Lens.Lens' ServiceEvent (Core.Maybe Core.Text)
serviceEvent_id = Lens.lens (\ServiceEvent' {id} -> id) (\s@ServiceEvent' {} a -> s {id = a} :: ServiceEvent)

instance Core.FromJSON ServiceEvent where
  parseJSON =
    Core.withObject
      "ServiceEvent"
      ( \x ->
          ServiceEvent'
            Core.<$> (x Core..:? "message")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "id")
      )

instance Core.Hashable ServiceEvent

instance Core.NFData ServiceEvent
