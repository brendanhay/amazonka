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
-- Module      : Amazonka.CloudTrailData.Types.AuditEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrailData.Types.AuditEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An event from a source outside of Amazon Web Services that you want
-- CloudTrail to log.
--
-- /See:/ 'newAuditEvent' smart constructor.
data AuditEvent = AuditEvent'
  { -- | A checksum is a base64-SHA256 algorithm that helps you verify that
    -- CloudTrail receives the event that matches with the checksum. Calculate
    -- the checksum by running a command like the following:
    --
    -- @printf %s @/@$eventdata@/@ | openssl dgst -binary -sha256 | base64@
    eventDataChecksum :: Prelude.Maybe Prelude.Text,
    -- | The content of an audit event that comes from the event, such as
    -- @userIdentity@, @userAgent@, and @eventSource@.
    eventData :: Prelude.Text,
    -- | The original event ID from the source event.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventDataChecksum', 'auditEvent_eventDataChecksum' - A checksum is a base64-SHA256 algorithm that helps you verify that
-- CloudTrail receives the event that matches with the checksum. Calculate
-- the checksum by running a command like the following:
--
-- @printf %s @/@$eventdata@/@ | openssl dgst -binary -sha256 | base64@
--
-- 'eventData', 'auditEvent_eventData' - The content of an audit event that comes from the event, such as
-- @userIdentity@, @userAgent@, and @eventSource@.
--
-- 'id', 'auditEvent_id' - The original event ID from the source event.
newAuditEvent ::
  -- | 'eventData'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  AuditEvent
newAuditEvent pEventData_ pId_ =
  AuditEvent'
    { eventDataChecksum = Prelude.Nothing,
      eventData = pEventData_,
      id = pId_
    }

-- | A checksum is a base64-SHA256 algorithm that helps you verify that
-- CloudTrail receives the event that matches with the checksum. Calculate
-- the checksum by running a command like the following:
--
-- @printf %s @/@$eventdata@/@ | openssl dgst -binary -sha256 | base64@
auditEvent_eventDataChecksum :: Lens.Lens' AuditEvent (Prelude.Maybe Prelude.Text)
auditEvent_eventDataChecksum = Lens.lens (\AuditEvent' {eventDataChecksum} -> eventDataChecksum) (\s@AuditEvent' {} a -> s {eventDataChecksum = a} :: AuditEvent)

-- | The content of an audit event that comes from the event, such as
-- @userIdentity@, @userAgent@, and @eventSource@.
auditEvent_eventData :: Lens.Lens' AuditEvent Prelude.Text
auditEvent_eventData = Lens.lens (\AuditEvent' {eventData} -> eventData) (\s@AuditEvent' {} a -> s {eventData = a} :: AuditEvent)

-- | The original event ID from the source event.
auditEvent_id :: Lens.Lens' AuditEvent Prelude.Text
auditEvent_id = Lens.lens (\AuditEvent' {id} -> id) (\s@AuditEvent' {} a -> s {id = a} :: AuditEvent)

instance Prelude.Hashable AuditEvent where
  hashWithSalt _salt AuditEvent' {..} =
    _salt
      `Prelude.hashWithSalt` eventDataChecksum
      `Prelude.hashWithSalt` eventData
      `Prelude.hashWithSalt` id

instance Prelude.NFData AuditEvent where
  rnf AuditEvent' {..} =
    Prelude.rnf eventDataChecksum
      `Prelude.seq` Prelude.rnf eventData
      `Prelude.seq` Prelude.rnf id

instance Data.ToJSON AuditEvent where
  toJSON AuditEvent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("eventDataChecksum" Data..=)
              Prelude.<$> eventDataChecksum,
            Prelude.Just ("eventData" Data..= eventData),
            Prelude.Just ("id" Data..= id)
          ]
      )
