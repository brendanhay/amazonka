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
-- Module      : Amazonka.Pinpoint.Types.Session
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.Session where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a session.
--
-- /See:/ 'newSession' smart constructor.
data Session = Session'
  { -- | The duration of the session, in milliseconds.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The date and time when the session ended.
    stopTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the session began.
    startTimestamp :: Prelude.Text,
    -- | The unique identifier for the session.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Session' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'session_duration' - The duration of the session, in milliseconds.
--
-- 'stopTimestamp', 'session_stopTimestamp' - The date and time when the session ended.
--
-- 'startTimestamp', 'session_startTimestamp' - The date and time when the session began.
--
-- 'id', 'session_id' - The unique identifier for the session.
newSession ::
  -- | 'startTimestamp'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  Session
newSession pStartTimestamp_ pId_ =
  Session'
    { duration = Prelude.Nothing,
      stopTimestamp = Prelude.Nothing,
      startTimestamp = pStartTimestamp_,
      id = pId_
    }

-- | The duration of the session, in milliseconds.
session_duration :: Lens.Lens' Session (Prelude.Maybe Prelude.Int)
session_duration = Lens.lens (\Session' {duration} -> duration) (\s@Session' {} a -> s {duration = a} :: Session)

-- | The date and time when the session ended.
session_stopTimestamp :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_stopTimestamp = Lens.lens (\Session' {stopTimestamp} -> stopTimestamp) (\s@Session' {} a -> s {stopTimestamp = a} :: Session)

-- | The date and time when the session began.
session_startTimestamp :: Lens.Lens' Session Prelude.Text
session_startTimestamp = Lens.lens (\Session' {startTimestamp} -> startTimestamp) (\s@Session' {} a -> s {startTimestamp = a} :: Session)

-- | The unique identifier for the session.
session_id :: Lens.Lens' Session Prelude.Text
session_id = Lens.lens (\Session' {id} -> id) (\s@Session' {} a -> s {id = a} :: Session)

instance Prelude.Hashable Session where
  hashWithSalt _salt Session' {..} =
    _salt
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` stopTimestamp
      `Prelude.hashWithSalt` startTimestamp
      `Prelude.hashWithSalt` id

instance Prelude.NFData Session where
  rnf Session' {..} =
    Prelude.rnf duration
      `Prelude.seq` Prelude.rnf stopTimestamp
      `Prelude.seq` Prelude.rnf startTimestamp
      `Prelude.seq` Prelude.rnf id

instance Data.ToJSON Session where
  toJSON Session' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Duration" Data..=) Prelude.<$> duration,
            ("StopTimestamp" Data..=) Prelude.<$> stopTimestamp,
            Prelude.Just
              ("StartTimestamp" Data..= startTimestamp),
            Prelude.Just ("Id" Data..= id)
          ]
      )
