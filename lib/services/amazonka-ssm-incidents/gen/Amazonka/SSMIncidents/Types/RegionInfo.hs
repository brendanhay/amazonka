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
-- Module      : Amazonka.SSMIncidents.Types.RegionInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.RegionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.RegionStatus

-- | Information about a Amazon Web Services Region in your replication set.
--
-- /See:/ 'newRegionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { -- | The ID of the KMS key used to encrypt the data in this Amazon Web
    -- Services Region.
    sseKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Information displayed about the status of the Amazon Web Services
    -- Region.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The status of the Amazon Web Services Region in the replication set.
    status :: RegionStatus,
    -- | The most recent date and time that Incident Manager updated the Amazon
    -- Web Services Region\'s status.
    statusUpdateDateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sseKmsKeyId', 'regionInfo_sseKmsKeyId' - The ID of the KMS key used to encrypt the data in this Amazon Web
-- Services Region.
--
-- 'statusMessage', 'regionInfo_statusMessage' - Information displayed about the status of the Amazon Web Services
-- Region.
--
-- 'status', 'regionInfo_status' - The status of the Amazon Web Services Region in the replication set.
--
-- 'statusUpdateDateTime', 'regionInfo_statusUpdateDateTime' - The most recent date and time that Incident Manager updated the Amazon
-- Web Services Region\'s status.
newRegionInfo ::
  -- | 'status'
  RegionStatus ->
  -- | 'statusUpdateDateTime'
  Prelude.UTCTime ->
  RegionInfo
newRegionInfo pStatus_ pStatusUpdateDateTime_ =
  RegionInfo'
    { sseKmsKeyId = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      status = pStatus_,
      statusUpdateDateTime =
        Data._Time Lens.# pStatusUpdateDateTime_
    }

-- | The ID of the KMS key used to encrypt the data in this Amazon Web
-- Services Region.
regionInfo_sseKmsKeyId :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_sseKmsKeyId = Lens.lens (\RegionInfo' {sseKmsKeyId} -> sseKmsKeyId) (\s@RegionInfo' {} a -> s {sseKmsKeyId = a} :: RegionInfo)

-- | Information displayed about the status of the Amazon Web Services
-- Region.
regionInfo_statusMessage :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_statusMessage = Lens.lens (\RegionInfo' {statusMessage} -> statusMessage) (\s@RegionInfo' {} a -> s {statusMessage = a} :: RegionInfo)

-- | The status of the Amazon Web Services Region in the replication set.
regionInfo_status :: Lens.Lens' RegionInfo RegionStatus
regionInfo_status = Lens.lens (\RegionInfo' {status} -> status) (\s@RegionInfo' {} a -> s {status = a} :: RegionInfo)

-- | The most recent date and time that Incident Manager updated the Amazon
-- Web Services Region\'s status.
regionInfo_statusUpdateDateTime :: Lens.Lens' RegionInfo Prelude.UTCTime
regionInfo_statusUpdateDateTime = Lens.lens (\RegionInfo' {statusUpdateDateTime} -> statusUpdateDateTime) (\s@RegionInfo' {} a -> s {statusUpdateDateTime = a} :: RegionInfo) Prelude.. Data._Time

instance Data.FromJSON RegionInfo where
  parseJSON =
    Data.withObject
      "RegionInfo"
      ( \x ->
          RegionInfo'
            Prelude.<$> (x Data..:? "sseKmsKeyId")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "statusUpdateDateTime")
      )

instance Prelude.Hashable RegionInfo where
  hashWithSalt _salt RegionInfo' {..} =
    _salt
      `Prelude.hashWithSalt` sseKmsKeyId
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusUpdateDateTime

instance Prelude.NFData RegionInfo where
  rnf RegionInfo' {..} =
    Prelude.rnf sseKmsKeyId
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusUpdateDateTime
