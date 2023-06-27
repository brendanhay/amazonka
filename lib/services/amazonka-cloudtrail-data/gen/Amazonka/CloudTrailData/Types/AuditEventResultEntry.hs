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
-- Module      : Amazonka.CloudTrailData.Types.AuditEventResultEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrailData.Types.AuditEventResultEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A response that includes successful and failed event results.
--
-- /See:/ 'newAuditEventResultEntry' smart constructor.
data AuditEventResultEntry = AuditEventResultEntry'
  { -- | The event ID assigned by CloudTrail.
    eventID :: Prelude.Text,
    -- | The original event ID from the source event.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditEventResultEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventID', 'auditEventResultEntry_eventID' - The event ID assigned by CloudTrail.
--
-- 'id', 'auditEventResultEntry_id' - The original event ID from the source event.
newAuditEventResultEntry ::
  -- | 'eventID'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  AuditEventResultEntry
newAuditEventResultEntry pEventID_ pId_ =
  AuditEventResultEntry'
    { eventID = pEventID_,
      id = pId_
    }

-- | The event ID assigned by CloudTrail.
auditEventResultEntry_eventID :: Lens.Lens' AuditEventResultEntry Prelude.Text
auditEventResultEntry_eventID = Lens.lens (\AuditEventResultEntry' {eventID} -> eventID) (\s@AuditEventResultEntry' {} a -> s {eventID = a} :: AuditEventResultEntry)

-- | The original event ID from the source event.
auditEventResultEntry_id :: Lens.Lens' AuditEventResultEntry Prelude.Text
auditEventResultEntry_id = Lens.lens (\AuditEventResultEntry' {id} -> id) (\s@AuditEventResultEntry' {} a -> s {id = a} :: AuditEventResultEntry)

instance Data.FromJSON AuditEventResultEntry where
  parseJSON =
    Data.withObject
      "AuditEventResultEntry"
      ( \x ->
          AuditEventResultEntry'
            Prelude.<$> (x Data..: "eventID")
            Prelude.<*> (x Data..: "id")
      )

instance Prelude.Hashable AuditEventResultEntry where
  hashWithSalt _salt AuditEventResultEntry' {..} =
    _salt
      `Prelude.hashWithSalt` eventID
      `Prelude.hashWithSalt` id

instance Prelude.NFData AuditEventResultEntry where
  rnf AuditEventResultEntry' {..} =
    Prelude.rnf eventID `Prelude.seq` Prelude.rnf id
