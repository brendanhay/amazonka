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
-- Module      : Network.AWS.Pinpoint.Types.Session
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Session where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a session.
--
-- /See:/ 'newSession' smart constructor.
data Session = Session'
  { -- | The date and time when the session ended.
    stopTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The duration of the session, in milliseconds.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The date and time when the session began.
    startTimestamp :: Prelude.Text,
    -- | The unique identifier for the session.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Session' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stopTimestamp', 'session_stopTimestamp' - The date and time when the session ended.
--
-- 'duration', 'session_duration' - The duration of the session, in milliseconds.
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
    { stopTimestamp = Prelude.Nothing,
      duration = Prelude.Nothing,
      startTimestamp = pStartTimestamp_,
      id = pId_
    }

-- | The date and time when the session ended.
session_stopTimestamp :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_stopTimestamp = Lens.lens (\Session' {stopTimestamp} -> stopTimestamp) (\s@Session' {} a -> s {stopTimestamp = a} :: Session)

-- | The duration of the session, in milliseconds.
session_duration :: Lens.Lens' Session (Prelude.Maybe Prelude.Int)
session_duration = Lens.lens (\Session' {duration} -> duration) (\s@Session' {} a -> s {duration = a} :: Session)

-- | The date and time when the session began.
session_startTimestamp :: Lens.Lens' Session Prelude.Text
session_startTimestamp = Lens.lens (\Session' {startTimestamp} -> startTimestamp) (\s@Session' {} a -> s {startTimestamp = a} :: Session)

-- | The unique identifier for the session.
session_id :: Lens.Lens' Session Prelude.Text
session_id = Lens.lens (\Session' {id} -> id) (\s@Session' {} a -> s {id = a} :: Session)

instance Prelude.Hashable Session

instance Prelude.NFData Session

instance Prelude.ToJSON Session where
  toJSON Session' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StopTimestamp" Prelude..=)
              Prelude.<$> stopTimestamp,
            ("Duration" Prelude..=) Prelude.<$> duration,
            Prelude.Just
              ("StartTimestamp" Prelude..= startTimestamp),
            Prelude.Just ("Id" Prelude..= id)
          ]
      )
