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
-- Module      : Amazonka.Rum.Types.RumEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.RumEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the information for one performance event that
-- RUM collects from a user session with your application.
--
-- /See:/ 'newRumEvent' smart constructor.
data RumEvent = RumEvent'
  { -- | Metadata about this event, which contains a JSON serialization of the
    -- identity of the user for this session. The user information comes from
    -- information such as the HTTP user-agent request header and document
    -- interface.
    metadata :: Prelude.Maybe Prelude.Text,
    -- | A string containing details about the event.
    details :: Prelude.Text,
    -- | A unique ID for this event.
    id :: Prelude.Text,
    -- | The exact time that this event occurred.
    timestamp :: Data.POSIX,
    -- | The JSON schema that denotes the type of event this is, such as a page
    -- load or a new session.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RumEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'rumEvent_metadata' - Metadata about this event, which contains a JSON serialization of the
-- identity of the user for this session. The user information comes from
-- information such as the HTTP user-agent request header and document
-- interface.
--
-- 'details', 'rumEvent_details' - A string containing details about the event.
--
-- 'id', 'rumEvent_id' - A unique ID for this event.
--
-- 'timestamp', 'rumEvent_timestamp' - The exact time that this event occurred.
--
-- 'type'', 'rumEvent_type' - The JSON schema that denotes the type of event this is, such as a page
-- load or a new session.
newRumEvent ::
  -- | 'details'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  -- | 'type''
  Prelude.Text ->
  RumEvent
newRumEvent pDetails_ pId_ pTimestamp_ pType_ =
  RumEvent'
    { metadata = Prelude.Nothing,
      details = pDetails_,
      id = pId_,
      timestamp = Data._Time Lens.# pTimestamp_,
      type' = pType_
    }

-- | Metadata about this event, which contains a JSON serialization of the
-- identity of the user for this session. The user information comes from
-- information such as the HTTP user-agent request header and document
-- interface.
rumEvent_metadata :: Lens.Lens' RumEvent (Prelude.Maybe Prelude.Text)
rumEvent_metadata = Lens.lens (\RumEvent' {metadata} -> metadata) (\s@RumEvent' {} a -> s {metadata = a} :: RumEvent)

-- | A string containing details about the event.
rumEvent_details :: Lens.Lens' RumEvent Prelude.Text
rumEvent_details = Lens.lens (\RumEvent' {details} -> details) (\s@RumEvent' {} a -> s {details = a} :: RumEvent)

-- | A unique ID for this event.
rumEvent_id :: Lens.Lens' RumEvent Prelude.Text
rumEvent_id = Lens.lens (\RumEvent' {id} -> id) (\s@RumEvent' {} a -> s {id = a} :: RumEvent)

-- | The exact time that this event occurred.
rumEvent_timestamp :: Lens.Lens' RumEvent Prelude.UTCTime
rumEvent_timestamp = Lens.lens (\RumEvent' {timestamp} -> timestamp) (\s@RumEvent' {} a -> s {timestamp = a} :: RumEvent) Prelude.. Data._Time

-- | The JSON schema that denotes the type of event this is, such as a page
-- load or a new session.
rumEvent_type :: Lens.Lens' RumEvent Prelude.Text
rumEvent_type = Lens.lens (\RumEvent' {type'} -> type') (\s@RumEvent' {} a -> s {type' = a} :: RumEvent)

instance Prelude.Hashable RumEvent where
  hashWithSalt _salt RumEvent' {..} =
    _salt `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RumEvent where
  rnf RumEvent' {..} =
    Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON RumEvent where
  toJSON RumEvent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("metadata" Data..=) Prelude.<$> metadata,
            Prelude.Just ("details" Data..= details),
            Prelude.Just ("id" Data..= id),
            Prelude.Just ("timestamp" Data..= timestamp),
            Prelude.Just ("type" Data..= type')
          ]
      )
