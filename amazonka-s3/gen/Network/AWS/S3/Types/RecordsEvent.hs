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
-- Module      : Network.AWS.S3.Types.RecordsEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RecordsEvent where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | The container for the records event.
--
-- /See:/ 'newRecordsEvent' smart constructor.
data RecordsEvent = RecordsEvent'
  { -- | The byte array of partial, one or more result records.
    payload :: Prelude.Maybe Prelude.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RecordsEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'recordsEvent_payload' - The byte array of partial, one or more result records.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newRecordsEvent ::
  RecordsEvent
newRecordsEvent =
  RecordsEvent' {payload = Prelude.Nothing}

-- | The byte array of partial, one or more result records.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
recordsEvent_payload :: Lens.Lens' RecordsEvent (Prelude.Maybe Prelude.ByteString)
recordsEvent_payload = Lens.lens (\RecordsEvent' {payload} -> payload) (\s@RecordsEvent' {} a -> s {payload = a} :: RecordsEvent) Prelude.. Lens.mapping Prelude._Base64

instance Prelude.FromXML RecordsEvent where
  parseXML x =
    RecordsEvent' Prelude.<$> (x Prelude..@? "Payload")

instance Prelude.Hashable RecordsEvent

instance Prelude.NFData RecordsEvent
