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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceLogEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceLogEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the log events of a container of an Amazon Lightsail container
-- service.
--
-- /See:/ 'newContainerServiceLogEvent' smart constructor.
data ContainerServiceLogEvent = ContainerServiceLogEvent'
  { -- | The timestamp when the container service log event was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The message of the container service log event.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerServiceLogEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'containerServiceLogEvent_createdAt' - The timestamp when the container service log event was created.
--
-- 'message', 'containerServiceLogEvent_message' - The message of the container service log event.
newContainerServiceLogEvent ::
  ContainerServiceLogEvent
newContainerServiceLogEvent =
  ContainerServiceLogEvent'
    { createdAt =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The timestamp when the container service log event was created.
containerServiceLogEvent_createdAt :: Lens.Lens' ContainerServiceLogEvent (Prelude.Maybe Prelude.UTCTime)
containerServiceLogEvent_createdAt = Lens.lens (\ContainerServiceLogEvent' {createdAt} -> createdAt) (\s@ContainerServiceLogEvent' {} a -> s {createdAt = a} :: ContainerServiceLogEvent) Prelude.. Lens.mapping Data._Time

-- | The message of the container service log event.
containerServiceLogEvent_message :: Lens.Lens' ContainerServiceLogEvent (Prelude.Maybe Prelude.Text)
containerServiceLogEvent_message = Lens.lens (\ContainerServiceLogEvent' {message} -> message) (\s@ContainerServiceLogEvent' {} a -> s {message = a} :: ContainerServiceLogEvent)

instance Data.FromJSON ContainerServiceLogEvent where
  parseJSON =
    Data.withObject
      "ContainerServiceLogEvent"
      ( \x ->
          ContainerServiceLogEvent'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "message")
      )

instance Prelude.Hashable ContainerServiceLogEvent where
  hashWithSalt _salt ContainerServiceLogEvent' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` message

instance Prelude.NFData ContainerServiceLogEvent where
  rnf ContainerServiceLogEvent' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf message
