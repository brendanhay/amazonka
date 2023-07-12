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
-- Module      : Amazonka.DataExchange.Types.Event
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.Event where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.RevisionPublished
import qualified Amazonka.Prelude as Prelude

-- | What occurs to start an action.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | What occurs to start the revision publish action.
    revisionPublished :: Prelude.Maybe RevisionPublished
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionPublished', 'event_revisionPublished' - What occurs to start the revision publish action.
newEvent ::
  Event
newEvent =
  Event' {revisionPublished = Prelude.Nothing}

-- | What occurs to start the revision publish action.
event_revisionPublished :: Lens.Lens' Event (Prelude.Maybe RevisionPublished)
event_revisionPublished = Lens.lens (\Event' {revisionPublished} -> revisionPublished) (\s@Event' {} a -> s {revisionPublished = a} :: Event)

instance Data.FromJSON Event where
  parseJSON =
    Data.withObject
      "Event"
      ( \x ->
          Event' Prelude.<$> (x Data..:? "RevisionPublished")
      )

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt `Prelude.hashWithSalt` revisionPublished

instance Prelude.NFData Event where
  rnf Event' {..} = Prelude.rnf revisionPublished

instance Data.ToJSON Event where
  toJSON Event' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RevisionPublished" Data..=)
              Prelude.<$> revisionPublished
          ]
      )
