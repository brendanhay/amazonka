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
-- Module      : Amazonka.Lightsail.Types.RelationalDatabaseEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RelationalDatabaseEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an event for a database.
--
-- /See:/ 'newRelationalDatabaseEvent' smart constructor.
data RelationalDatabaseEvent = RelationalDatabaseEvent'
  { -- | The timestamp when the database event was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The category that the database event belongs to.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The message of the database event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The database that the database event relates to.
    resource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelationalDatabaseEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'relationalDatabaseEvent_createdAt' - The timestamp when the database event was created.
--
-- 'eventCategories', 'relationalDatabaseEvent_eventCategories' - The category that the database event belongs to.
--
-- 'message', 'relationalDatabaseEvent_message' - The message of the database event.
--
-- 'resource', 'relationalDatabaseEvent_resource' - The database that the database event relates to.
newRelationalDatabaseEvent ::
  RelationalDatabaseEvent
newRelationalDatabaseEvent =
  RelationalDatabaseEvent'
    { createdAt =
        Prelude.Nothing,
      eventCategories = Prelude.Nothing,
      message = Prelude.Nothing,
      resource = Prelude.Nothing
    }

-- | The timestamp when the database event was created.
relationalDatabaseEvent_createdAt :: Lens.Lens' RelationalDatabaseEvent (Prelude.Maybe Prelude.UTCTime)
relationalDatabaseEvent_createdAt = Lens.lens (\RelationalDatabaseEvent' {createdAt} -> createdAt) (\s@RelationalDatabaseEvent' {} a -> s {createdAt = a} :: RelationalDatabaseEvent) Prelude.. Lens.mapping Data._Time

-- | The category that the database event belongs to.
relationalDatabaseEvent_eventCategories :: Lens.Lens' RelationalDatabaseEvent (Prelude.Maybe [Prelude.Text])
relationalDatabaseEvent_eventCategories = Lens.lens (\RelationalDatabaseEvent' {eventCategories} -> eventCategories) (\s@RelationalDatabaseEvent' {} a -> s {eventCategories = a} :: RelationalDatabaseEvent) Prelude.. Lens.mapping Lens.coerced

-- | The message of the database event.
relationalDatabaseEvent_message :: Lens.Lens' RelationalDatabaseEvent (Prelude.Maybe Prelude.Text)
relationalDatabaseEvent_message = Lens.lens (\RelationalDatabaseEvent' {message} -> message) (\s@RelationalDatabaseEvent' {} a -> s {message = a} :: RelationalDatabaseEvent)

-- | The database that the database event relates to.
relationalDatabaseEvent_resource :: Lens.Lens' RelationalDatabaseEvent (Prelude.Maybe Prelude.Text)
relationalDatabaseEvent_resource = Lens.lens (\RelationalDatabaseEvent' {resource} -> resource) (\s@RelationalDatabaseEvent' {} a -> s {resource = a} :: RelationalDatabaseEvent)

instance Data.FromJSON RelationalDatabaseEvent where
  parseJSON =
    Data.withObject
      "RelationalDatabaseEvent"
      ( \x ->
          RelationalDatabaseEvent'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> ( x
                            Data..:? "eventCategories"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "resource")
      )

instance Prelude.Hashable RelationalDatabaseEvent where
  hashWithSalt _salt RelationalDatabaseEvent' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` eventCategories
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` resource

instance Prelude.NFData RelationalDatabaseEvent where
  rnf RelationalDatabaseEvent' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf eventCategories
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf resource
