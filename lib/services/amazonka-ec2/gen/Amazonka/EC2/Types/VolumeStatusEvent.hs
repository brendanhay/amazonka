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
-- Module      : Amazonka.EC2.Types.VolumeStatusEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VolumeStatusEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a volume status event.
--
-- /See:/ 'newVolumeStatusEvent' smart constructor.
data VolumeStatusEvent = VolumeStatusEvent'
  { -- | A description of the event.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of this event.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The type of this event.
    eventType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance associated with the event.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The latest end time of the event.
    notAfter :: Prelude.Maybe Data.ISO8601,
    -- | The earliest start time of the event.
    notBefore :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeStatusEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'volumeStatusEvent_description' - A description of the event.
--
-- 'eventId', 'volumeStatusEvent_eventId' - The ID of this event.
--
-- 'eventType', 'volumeStatusEvent_eventType' - The type of this event.
--
-- 'instanceId', 'volumeStatusEvent_instanceId' - The ID of the instance associated with the event.
--
-- 'notAfter', 'volumeStatusEvent_notAfter' - The latest end time of the event.
--
-- 'notBefore', 'volumeStatusEvent_notBefore' - The earliest start time of the event.
newVolumeStatusEvent ::
  VolumeStatusEvent
newVolumeStatusEvent =
  VolumeStatusEvent'
    { description = Prelude.Nothing,
      eventId = Prelude.Nothing,
      eventType = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      notAfter = Prelude.Nothing,
      notBefore = Prelude.Nothing
    }

-- | A description of the event.
volumeStatusEvent_description :: Lens.Lens' VolumeStatusEvent (Prelude.Maybe Prelude.Text)
volumeStatusEvent_description = Lens.lens (\VolumeStatusEvent' {description} -> description) (\s@VolumeStatusEvent' {} a -> s {description = a} :: VolumeStatusEvent)

-- | The ID of this event.
volumeStatusEvent_eventId :: Lens.Lens' VolumeStatusEvent (Prelude.Maybe Prelude.Text)
volumeStatusEvent_eventId = Lens.lens (\VolumeStatusEvent' {eventId} -> eventId) (\s@VolumeStatusEvent' {} a -> s {eventId = a} :: VolumeStatusEvent)

-- | The type of this event.
volumeStatusEvent_eventType :: Lens.Lens' VolumeStatusEvent (Prelude.Maybe Prelude.Text)
volumeStatusEvent_eventType = Lens.lens (\VolumeStatusEvent' {eventType} -> eventType) (\s@VolumeStatusEvent' {} a -> s {eventType = a} :: VolumeStatusEvent)

-- | The ID of the instance associated with the event.
volumeStatusEvent_instanceId :: Lens.Lens' VolumeStatusEvent (Prelude.Maybe Prelude.Text)
volumeStatusEvent_instanceId = Lens.lens (\VolumeStatusEvent' {instanceId} -> instanceId) (\s@VolumeStatusEvent' {} a -> s {instanceId = a} :: VolumeStatusEvent)

-- | The latest end time of the event.
volumeStatusEvent_notAfter :: Lens.Lens' VolumeStatusEvent (Prelude.Maybe Prelude.UTCTime)
volumeStatusEvent_notAfter = Lens.lens (\VolumeStatusEvent' {notAfter} -> notAfter) (\s@VolumeStatusEvent' {} a -> s {notAfter = a} :: VolumeStatusEvent) Prelude.. Lens.mapping Data._Time

-- | The earliest start time of the event.
volumeStatusEvent_notBefore :: Lens.Lens' VolumeStatusEvent (Prelude.Maybe Prelude.UTCTime)
volumeStatusEvent_notBefore = Lens.lens (\VolumeStatusEvent' {notBefore} -> notBefore) (\s@VolumeStatusEvent' {} a -> s {notBefore = a} :: VolumeStatusEvent) Prelude.. Lens.mapping Data._Time

instance Data.FromXML VolumeStatusEvent where
  parseXML x =
    VolumeStatusEvent'
      Prelude.<$> (x Data..@? "description")
      Prelude.<*> (x Data..@? "eventId")
      Prelude.<*> (x Data..@? "eventType")
      Prelude.<*> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "notAfter")
      Prelude.<*> (x Data..@? "notBefore")

instance Prelude.Hashable VolumeStatusEvent where
  hashWithSalt _salt VolumeStatusEvent' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` notAfter
      `Prelude.hashWithSalt` notBefore

instance Prelude.NFData VolumeStatusEvent where
  rnf VolumeStatusEvent' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf notAfter
      `Prelude.seq` Prelude.rnf notBefore
