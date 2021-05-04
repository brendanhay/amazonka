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
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseEvent where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an event for a database.
--
-- /See:/ 'newRelationalDatabaseEvent' smart constructor.
data RelationalDatabaseEvent = RelationalDatabaseEvent'
  { -- | The message of the database event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the database event was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The category that the database event belongs to.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The database that the database event relates to.
    resource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RelationalDatabaseEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'relationalDatabaseEvent_message' - The message of the database event.
--
-- 'createdAt', 'relationalDatabaseEvent_createdAt' - The timestamp when the database event was created.
--
-- 'eventCategories', 'relationalDatabaseEvent_eventCategories' - The category that the database event belongs to.
--
-- 'resource', 'relationalDatabaseEvent_resource' - The database that the database event relates to.
newRelationalDatabaseEvent ::
  RelationalDatabaseEvent
newRelationalDatabaseEvent =
  RelationalDatabaseEvent'
    { message = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      eventCategories = Prelude.Nothing,
      resource = Prelude.Nothing
    }

-- | The message of the database event.
relationalDatabaseEvent_message :: Lens.Lens' RelationalDatabaseEvent (Prelude.Maybe Prelude.Text)
relationalDatabaseEvent_message = Lens.lens (\RelationalDatabaseEvent' {message} -> message) (\s@RelationalDatabaseEvent' {} a -> s {message = a} :: RelationalDatabaseEvent)

-- | The timestamp when the database event was created.
relationalDatabaseEvent_createdAt :: Lens.Lens' RelationalDatabaseEvent (Prelude.Maybe Prelude.UTCTime)
relationalDatabaseEvent_createdAt = Lens.lens (\RelationalDatabaseEvent' {createdAt} -> createdAt) (\s@RelationalDatabaseEvent' {} a -> s {createdAt = a} :: RelationalDatabaseEvent) Prelude.. Lens.mapping Prelude._Time

-- | The category that the database event belongs to.
relationalDatabaseEvent_eventCategories :: Lens.Lens' RelationalDatabaseEvent (Prelude.Maybe [Prelude.Text])
relationalDatabaseEvent_eventCategories = Lens.lens (\RelationalDatabaseEvent' {eventCategories} -> eventCategories) (\s@RelationalDatabaseEvent' {} a -> s {eventCategories = a} :: RelationalDatabaseEvent) Prelude.. Lens.mapping Prelude._Coerce

-- | The database that the database event relates to.
relationalDatabaseEvent_resource :: Lens.Lens' RelationalDatabaseEvent (Prelude.Maybe Prelude.Text)
relationalDatabaseEvent_resource = Lens.lens (\RelationalDatabaseEvent' {resource} -> resource) (\s@RelationalDatabaseEvent' {} a -> s {resource = a} :: RelationalDatabaseEvent)

instance Prelude.FromJSON RelationalDatabaseEvent where
  parseJSON =
    Prelude.withObject
      "RelationalDatabaseEvent"
      ( \x ->
          RelationalDatabaseEvent'
            Prelude.<$> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> ( x Prelude..:? "eventCategories"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "resource")
      )

instance Prelude.Hashable RelationalDatabaseEvent

instance Prelude.NFData RelationalDatabaseEvent
