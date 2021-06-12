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
-- Module      : Network.AWS.EC2.Types.VolumeStatusAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusAction where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a volume status operation code.
--
-- /See:/ 'newVolumeStatusAction' smart constructor.
data VolumeStatusAction = VolumeStatusAction'
  { -- | The event type associated with this operation.
    eventType :: Core.Maybe Core.Text,
    -- | The ID of the event associated with this operation.
    eventId :: Core.Maybe Core.Text,
    -- | The code identifying the operation, for example, @enable-volume-io@.
    code :: Core.Maybe Core.Text,
    -- | A description of the operation.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VolumeStatusAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventType', 'volumeStatusAction_eventType' - The event type associated with this operation.
--
-- 'eventId', 'volumeStatusAction_eventId' - The ID of the event associated with this operation.
--
-- 'code', 'volumeStatusAction_code' - The code identifying the operation, for example, @enable-volume-io@.
--
-- 'description', 'volumeStatusAction_description' - A description of the operation.
newVolumeStatusAction ::
  VolumeStatusAction
newVolumeStatusAction =
  VolumeStatusAction'
    { eventType = Core.Nothing,
      eventId = Core.Nothing,
      code = Core.Nothing,
      description = Core.Nothing
    }

-- | The event type associated with this operation.
volumeStatusAction_eventType :: Lens.Lens' VolumeStatusAction (Core.Maybe Core.Text)
volumeStatusAction_eventType = Lens.lens (\VolumeStatusAction' {eventType} -> eventType) (\s@VolumeStatusAction' {} a -> s {eventType = a} :: VolumeStatusAction)

-- | The ID of the event associated with this operation.
volumeStatusAction_eventId :: Lens.Lens' VolumeStatusAction (Core.Maybe Core.Text)
volumeStatusAction_eventId = Lens.lens (\VolumeStatusAction' {eventId} -> eventId) (\s@VolumeStatusAction' {} a -> s {eventId = a} :: VolumeStatusAction)

-- | The code identifying the operation, for example, @enable-volume-io@.
volumeStatusAction_code :: Lens.Lens' VolumeStatusAction (Core.Maybe Core.Text)
volumeStatusAction_code = Lens.lens (\VolumeStatusAction' {code} -> code) (\s@VolumeStatusAction' {} a -> s {code = a} :: VolumeStatusAction)

-- | A description of the operation.
volumeStatusAction_description :: Lens.Lens' VolumeStatusAction (Core.Maybe Core.Text)
volumeStatusAction_description = Lens.lens (\VolumeStatusAction' {description} -> description) (\s@VolumeStatusAction' {} a -> s {description = a} :: VolumeStatusAction)

instance Core.FromXML VolumeStatusAction where
  parseXML x =
    VolumeStatusAction'
      Core.<$> (x Core..@? "eventType")
      Core.<*> (x Core..@? "eventId")
      Core.<*> (x Core..@? "code")
      Core.<*> (x Core..@? "description")

instance Core.Hashable VolumeStatusAction

instance Core.NFData VolumeStatusAction
