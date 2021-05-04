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
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceLogEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceLogEvent where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the log events of a container of an Amazon Lightsail container
-- service.
--
-- /See:/ 'newContainerServiceLogEvent' smart constructor.
data ContainerServiceLogEvent = ContainerServiceLogEvent'
  { -- | The message of the container service log event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the container service log event was created.
    createdAt :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContainerServiceLogEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'containerServiceLogEvent_message' - The message of the container service log event.
--
-- 'createdAt', 'containerServiceLogEvent_createdAt' - The timestamp when the container service log event was created.
newContainerServiceLogEvent ::
  ContainerServiceLogEvent
newContainerServiceLogEvent =
  ContainerServiceLogEvent'
    { message =
        Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The message of the container service log event.
containerServiceLogEvent_message :: Lens.Lens' ContainerServiceLogEvent (Prelude.Maybe Prelude.Text)
containerServiceLogEvent_message = Lens.lens (\ContainerServiceLogEvent' {message} -> message) (\s@ContainerServiceLogEvent' {} a -> s {message = a} :: ContainerServiceLogEvent)

-- | The timestamp when the container service log event was created.
containerServiceLogEvent_createdAt :: Lens.Lens' ContainerServiceLogEvent (Prelude.Maybe Prelude.UTCTime)
containerServiceLogEvent_createdAt = Lens.lens (\ContainerServiceLogEvent' {createdAt} -> createdAt) (\s@ContainerServiceLogEvent' {} a -> s {createdAt = a} :: ContainerServiceLogEvent) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON ContainerServiceLogEvent where
  parseJSON =
    Prelude.withObject
      "ContainerServiceLogEvent"
      ( \x ->
          ContainerServiceLogEvent'
            Prelude.<$> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "createdAt")
      )

instance Prelude.Hashable ContainerServiceLogEvent

instance Prelude.NFData ContainerServiceLogEvent
