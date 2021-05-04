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
-- Module      : Network.AWS.EC2.Types.VolumeStatusAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusAction where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a volume status operation code.
--
-- /See:/ 'newVolumeStatusAction' smart constructor.
data VolumeStatusAction = VolumeStatusAction'
  { -- | The event type associated with this operation.
    eventType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the event associated with this operation.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The code identifying the operation, for example, @enable-volume-io@.
    code :: Prelude.Maybe Prelude.Text,
    -- | A description of the operation.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { eventType = Prelude.Nothing,
      eventId = Prelude.Nothing,
      code = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The event type associated with this operation.
volumeStatusAction_eventType :: Lens.Lens' VolumeStatusAction (Prelude.Maybe Prelude.Text)
volumeStatusAction_eventType = Lens.lens (\VolumeStatusAction' {eventType} -> eventType) (\s@VolumeStatusAction' {} a -> s {eventType = a} :: VolumeStatusAction)

-- | The ID of the event associated with this operation.
volumeStatusAction_eventId :: Lens.Lens' VolumeStatusAction (Prelude.Maybe Prelude.Text)
volumeStatusAction_eventId = Lens.lens (\VolumeStatusAction' {eventId} -> eventId) (\s@VolumeStatusAction' {} a -> s {eventId = a} :: VolumeStatusAction)

-- | The code identifying the operation, for example, @enable-volume-io@.
volumeStatusAction_code :: Lens.Lens' VolumeStatusAction (Prelude.Maybe Prelude.Text)
volumeStatusAction_code = Lens.lens (\VolumeStatusAction' {code} -> code) (\s@VolumeStatusAction' {} a -> s {code = a} :: VolumeStatusAction)

-- | A description of the operation.
volumeStatusAction_description :: Lens.Lens' VolumeStatusAction (Prelude.Maybe Prelude.Text)
volumeStatusAction_description = Lens.lens (\VolumeStatusAction' {description} -> description) (\s@VolumeStatusAction' {} a -> s {description = a} :: VolumeStatusAction)

instance Prelude.FromXML VolumeStatusAction where
  parseXML x =
    VolumeStatusAction'
      Prelude.<$> (x Prelude..@? "eventType")
      Prelude.<*> (x Prelude..@? "eventId")
      Prelude.<*> (x Prelude..@? "code")
      Prelude.<*> (x Prelude..@? "description")

instance Prelude.Hashable VolumeStatusAction

instance Prelude.NFData VolumeStatusAction
