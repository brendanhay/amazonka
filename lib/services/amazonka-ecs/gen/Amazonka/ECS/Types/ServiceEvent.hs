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
-- Module      : Amazonka.ECS.Types.ServiceEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ServiceEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details for an event that\'s associated with a service.
--
-- /See:/ 'newServiceEvent' smart constructor.
data ServiceEvent = ServiceEvent'
  { -- | The Unix timestamp for the time when the event was triggered.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The ID string for the event.
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
-- 'createdAt', 'serviceEvent_createdAt' - The Unix timestamp for the time when the event was triggered.
--
-- 'id', 'serviceEvent_id' - The ID string for the event.
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

-- | The Unix timestamp for the time when the event was triggered.
serviceEvent_createdAt :: Lens.Lens' ServiceEvent (Prelude.Maybe Prelude.UTCTime)
serviceEvent_createdAt = Lens.lens (\ServiceEvent' {createdAt} -> createdAt) (\s@ServiceEvent' {} a -> s {createdAt = a} :: ServiceEvent) Prelude.. Lens.mapping Data._Time

-- | The ID string for the event.
serviceEvent_id :: Lens.Lens' ServiceEvent (Prelude.Maybe Prelude.Text)
serviceEvent_id = Lens.lens (\ServiceEvent' {id} -> id) (\s@ServiceEvent' {} a -> s {id = a} :: ServiceEvent)

-- | The event message.
serviceEvent_message :: Lens.Lens' ServiceEvent (Prelude.Maybe Prelude.Text)
serviceEvent_message = Lens.lens (\ServiceEvent' {message} -> message) (\s@ServiceEvent' {} a -> s {message = a} :: ServiceEvent)

instance Data.FromJSON ServiceEvent where
  parseJSON =
    Data.withObject
      "ServiceEvent"
      ( \x ->
          ServiceEvent'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "message")
      )

instance Prelude.Hashable ServiceEvent where
  hashWithSalt _salt ServiceEvent' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` message

instance Prelude.NFData ServiceEvent where
  rnf ServiceEvent' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf message
