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
-- Module      : Amazonka.GroundStation.Types.ContactData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.ContactData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.ContactStatus
import Amazonka.GroundStation.Types.Elevation
import qualified Amazonka.Prelude as Prelude

-- | Data describing a contact.
--
-- /See:/ 'newContactData' smart constructor.
data ContactData = ContactData'
  { -- | Tags assigned to a contact.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | ARN of a mission profile.
    missionProfileArn :: Prelude.Maybe Prelude.Text,
    -- | UUID of a contact.
    contactId :: Prelude.Maybe Prelude.Text,
    -- | Error message of a contact.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Amount of time after a contact ends that you’d like to receive a
    -- CloudWatch event indicating the pass has finished.
    postPassEndTime :: Prelude.Maybe Data.POSIX,
    -- | ARN of a satellite.
    satelliteArn :: Prelude.Maybe Prelude.Text,
    -- | Status of a contact.
    contactStatus :: Prelude.Maybe ContactStatus,
    -- | End time of a contact in UTC.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Region of a contact.
    region :: Prelude.Maybe Prelude.Text,
    -- | Amount of time prior to contact start you’d like to receive a CloudWatch
    -- event indicating an upcoming pass.
    prePassStartTime :: Prelude.Maybe Data.POSIX,
    -- | Name of a ground station.
    groundStation :: Prelude.Maybe Prelude.Text,
    -- | Start time of a contact in UTC.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Maximum elevation angle of a contact.
    maximumElevation :: Prelude.Maybe Elevation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'contactData_tags' - Tags assigned to a contact.
--
-- 'missionProfileArn', 'contactData_missionProfileArn' - ARN of a mission profile.
--
-- 'contactId', 'contactData_contactId' - UUID of a contact.
--
-- 'errorMessage', 'contactData_errorMessage' - Error message of a contact.
--
-- 'postPassEndTime', 'contactData_postPassEndTime' - Amount of time after a contact ends that you’d like to receive a
-- CloudWatch event indicating the pass has finished.
--
-- 'satelliteArn', 'contactData_satelliteArn' - ARN of a satellite.
--
-- 'contactStatus', 'contactData_contactStatus' - Status of a contact.
--
-- 'endTime', 'contactData_endTime' - End time of a contact in UTC.
--
-- 'region', 'contactData_region' - Region of a contact.
--
-- 'prePassStartTime', 'contactData_prePassStartTime' - Amount of time prior to contact start you’d like to receive a CloudWatch
-- event indicating an upcoming pass.
--
-- 'groundStation', 'contactData_groundStation' - Name of a ground station.
--
-- 'startTime', 'contactData_startTime' - Start time of a contact in UTC.
--
-- 'maximumElevation', 'contactData_maximumElevation' - Maximum elevation angle of a contact.
newContactData ::
  ContactData
newContactData =
  ContactData'
    { tags = Prelude.Nothing,
      missionProfileArn = Prelude.Nothing,
      contactId = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      postPassEndTime = Prelude.Nothing,
      satelliteArn = Prelude.Nothing,
      contactStatus = Prelude.Nothing,
      endTime = Prelude.Nothing,
      region = Prelude.Nothing,
      prePassStartTime = Prelude.Nothing,
      groundStation = Prelude.Nothing,
      startTime = Prelude.Nothing,
      maximumElevation = Prelude.Nothing
    }

-- | Tags assigned to a contact.
contactData_tags :: Lens.Lens' ContactData (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
contactData_tags = Lens.lens (\ContactData' {tags} -> tags) (\s@ContactData' {} a -> s {tags = a} :: ContactData) Prelude.. Lens.mapping Lens.coerced

-- | ARN of a mission profile.
contactData_missionProfileArn :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_missionProfileArn = Lens.lens (\ContactData' {missionProfileArn} -> missionProfileArn) (\s@ContactData' {} a -> s {missionProfileArn = a} :: ContactData)

-- | UUID of a contact.
contactData_contactId :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_contactId = Lens.lens (\ContactData' {contactId} -> contactId) (\s@ContactData' {} a -> s {contactId = a} :: ContactData)

-- | Error message of a contact.
contactData_errorMessage :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_errorMessage = Lens.lens (\ContactData' {errorMessage} -> errorMessage) (\s@ContactData' {} a -> s {errorMessage = a} :: ContactData)

-- | Amount of time after a contact ends that you’d like to receive a
-- CloudWatch event indicating the pass has finished.
contactData_postPassEndTime :: Lens.Lens' ContactData (Prelude.Maybe Prelude.UTCTime)
contactData_postPassEndTime = Lens.lens (\ContactData' {postPassEndTime} -> postPassEndTime) (\s@ContactData' {} a -> s {postPassEndTime = a} :: ContactData) Prelude.. Lens.mapping Data._Time

-- | ARN of a satellite.
contactData_satelliteArn :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_satelliteArn = Lens.lens (\ContactData' {satelliteArn} -> satelliteArn) (\s@ContactData' {} a -> s {satelliteArn = a} :: ContactData)

-- | Status of a contact.
contactData_contactStatus :: Lens.Lens' ContactData (Prelude.Maybe ContactStatus)
contactData_contactStatus = Lens.lens (\ContactData' {contactStatus} -> contactStatus) (\s@ContactData' {} a -> s {contactStatus = a} :: ContactData)

-- | End time of a contact in UTC.
contactData_endTime :: Lens.Lens' ContactData (Prelude.Maybe Prelude.UTCTime)
contactData_endTime = Lens.lens (\ContactData' {endTime} -> endTime) (\s@ContactData' {} a -> s {endTime = a} :: ContactData) Prelude.. Lens.mapping Data._Time

-- | Region of a contact.
contactData_region :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_region = Lens.lens (\ContactData' {region} -> region) (\s@ContactData' {} a -> s {region = a} :: ContactData)

-- | Amount of time prior to contact start you’d like to receive a CloudWatch
-- event indicating an upcoming pass.
contactData_prePassStartTime :: Lens.Lens' ContactData (Prelude.Maybe Prelude.UTCTime)
contactData_prePassStartTime = Lens.lens (\ContactData' {prePassStartTime} -> prePassStartTime) (\s@ContactData' {} a -> s {prePassStartTime = a} :: ContactData) Prelude.. Lens.mapping Data._Time

-- | Name of a ground station.
contactData_groundStation :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_groundStation = Lens.lens (\ContactData' {groundStation} -> groundStation) (\s@ContactData' {} a -> s {groundStation = a} :: ContactData)

-- | Start time of a contact in UTC.
contactData_startTime :: Lens.Lens' ContactData (Prelude.Maybe Prelude.UTCTime)
contactData_startTime = Lens.lens (\ContactData' {startTime} -> startTime) (\s@ContactData' {} a -> s {startTime = a} :: ContactData) Prelude.. Lens.mapping Data._Time

-- | Maximum elevation angle of a contact.
contactData_maximumElevation :: Lens.Lens' ContactData (Prelude.Maybe Elevation)
contactData_maximumElevation = Lens.lens (\ContactData' {maximumElevation} -> maximumElevation) (\s@ContactData' {} a -> s {maximumElevation = a} :: ContactData)

instance Data.FromJSON ContactData where
  parseJSON =
    Data.withObject
      "ContactData"
      ( \x ->
          ContactData'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "missionProfileArn")
            Prelude.<*> (x Data..:? "contactId")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "postPassEndTime")
            Prelude.<*> (x Data..:? "satelliteArn")
            Prelude.<*> (x Data..:? "contactStatus")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "prePassStartTime")
            Prelude.<*> (x Data..:? "groundStation")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "maximumElevation")
      )

instance Prelude.Hashable ContactData where
  hashWithSalt _salt ContactData' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` missionProfileArn
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` postPassEndTime
      `Prelude.hashWithSalt` satelliteArn
      `Prelude.hashWithSalt` contactStatus
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` prePassStartTime
      `Prelude.hashWithSalt` groundStation
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` maximumElevation

instance Prelude.NFData ContactData where
  rnf ContactData' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf missionProfileArn
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf postPassEndTime
      `Prelude.seq` Prelude.rnf satelliteArn
      `Prelude.seq` Prelude.rnf contactStatus
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf prePassStartTime
      `Prelude.seq` Prelude.rnf groundStation
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf maximumElevation
