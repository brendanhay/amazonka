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
-- Module      : Network.AWS.GroundStation.Types.ContactData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GroundStation.Types.ContactData where

import qualified Network.AWS.Core as Core
import Network.AWS.GroundStation.Types.ContactStatus
import Network.AWS.GroundStation.Types.Elevation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Data describing a contact.
--
-- /See:/ 'newContactData' smart constructor.
data ContactData = ContactData'
  { -- | Status of a contact.
    contactStatus :: Prelude.Maybe ContactStatus,
    -- | ARN of a mission profile.
    missionProfileArn :: Prelude.Maybe Prelude.Text,
    -- | Start time of a contact.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | ARN of a satellite.
    satelliteArn :: Prelude.Maybe Prelude.Text,
    -- | Maximum elevation angle of a contact.
    maximumElevation :: Prelude.Maybe Elevation,
    -- | Name of a ground station.
    groundStation :: Prelude.Maybe Prelude.Text,
    -- | End time of a contact.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | UUID of a contact.
    contactId :: Prelude.Maybe Prelude.Text,
    -- | Region of a contact.
    region :: Prelude.Maybe Prelude.Text,
    -- | Amount of time after a contact ends that you’d like to receive a
    -- CloudWatch event indicating the pass has finished.
    postPassEndTime :: Prelude.Maybe Core.POSIX,
    -- | Amount of time prior to contact start you’d like to receive a CloudWatch
    -- event indicating an upcoming pass.
    prePassStartTime :: Prelude.Maybe Core.POSIX,
    -- | Error message of a contact.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Tags assigned to a contact.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'contactStatus', 'contactData_contactStatus' - Status of a contact.
--
-- 'missionProfileArn', 'contactData_missionProfileArn' - ARN of a mission profile.
--
-- 'startTime', 'contactData_startTime' - Start time of a contact.
--
-- 'satelliteArn', 'contactData_satelliteArn' - ARN of a satellite.
--
-- 'maximumElevation', 'contactData_maximumElevation' - Maximum elevation angle of a contact.
--
-- 'groundStation', 'contactData_groundStation' - Name of a ground station.
--
-- 'endTime', 'contactData_endTime' - End time of a contact.
--
-- 'contactId', 'contactData_contactId' - UUID of a contact.
--
-- 'region', 'contactData_region' - Region of a contact.
--
-- 'postPassEndTime', 'contactData_postPassEndTime' - Amount of time after a contact ends that you’d like to receive a
-- CloudWatch event indicating the pass has finished.
--
-- 'prePassStartTime', 'contactData_prePassStartTime' - Amount of time prior to contact start you’d like to receive a CloudWatch
-- event indicating an upcoming pass.
--
-- 'errorMessage', 'contactData_errorMessage' - Error message of a contact.
--
-- 'tags', 'contactData_tags' - Tags assigned to a contact.
newContactData ::
  ContactData
newContactData =
  ContactData'
    { contactStatus = Prelude.Nothing,
      missionProfileArn = Prelude.Nothing,
      startTime = Prelude.Nothing,
      satelliteArn = Prelude.Nothing,
      maximumElevation = Prelude.Nothing,
      groundStation = Prelude.Nothing,
      endTime = Prelude.Nothing,
      contactId = Prelude.Nothing,
      region = Prelude.Nothing,
      postPassEndTime = Prelude.Nothing,
      prePassStartTime = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Status of a contact.
contactData_contactStatus :: Lens.Lens' ContactData (Prelude.Maybe ContactStatus)
contactData_contactStatus = Lens.lens (\ContactData' {contactStatus} -> contactStatus) (\s@ContactData' {} a -> s {contactStatus = a} :: ContactData)

-- | ARN of a mission profile.
contactData_missionProfileArn :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_missionProfileArn = Lens.lens (\ContactData' {missionProfileArn} -> missionProfileArn) (\s@ContactData' {} a -> s {missionProfileArn = a} :: ContactData)

-- | Start time of a contact.
contactData_startTime :: Lens.Lens' ContactData (Prelude.Maybe Prelude.UTCTime)
contactData_startTime = Lens.lens (\ContactData' {startTime} -> startTime) (\s@ContactData' {} a -> s {startTime = a} :: ContactData) Prelude.. Lens.mapping Core._Time

-- | ARN of a satellite.
contactData_satelliteArn :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_satelliteArn = Lens.lens (\ContactData' {satelliteArn} -> satelliteArn) (\s@ContactData' {} a -> s {satelliteArn = a} :: ContactData)

-- | Maximum elevation angle of a contact.
contactData_maximumElevation :: Lens.Lens' ContactData (Prelude.Maybe Elevation)
contactData_maximumElevation = Lens.lens (\ContactData' {maximumElevation} -> maximumElevation) (\s@ContactData' {} a -> s {maximumElevation = a} :: ContactData)

-- | Name of a ground station.
contactData_groundStation :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_groundStation = Lens.lens (\ContactData' {groundStation} -> groundStation) (\s@ContactData' {} a -> s {groundStation = a} :: ContactData)

-- | End time of a contact.
contactData_endTime :: Lens.Lens' ContactData (Prelude.Maybe Prelude.UTCTime)
contactData_endTime = Lens.lens (\ContactData' {endTime} -> endTime) (\s@ContactData' {} a -> s {endTime = a} :: ContactData) Prelude.. Lens.mapping Core._Time

-- | UUID of a contact.
contactData_contactId :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_contactId = Lens.lens (\ContactData' {contactId} -> contactId) (\s@ContactData' {} a -> s {contactId = a} :: ContactData)

-- | Region of a contact.
contactData_region :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_region = Lens.lens (\ContactData' {region} -> region) (\s@ContactData' {} a -> s {region = a} :: ContactData)

-- | Amount of time after a contact ends that you’d like to receive a
-- CloudWatch event indicating the pass has finished.
contactData_postPassEndTime :: Lens.Lens' ContactData (Prelude.Maybe Prelude.UTCTime)
contactData_postPassEndTime = Lens.lens (\ContactData' {postPassEndTime} -> postPassEndTime) (\s@ContactData' {} a -> s {postPassEndTime = a} :: ContactData) Prelude.. Lens.mapping Core._Time

-- | Amount of time prior to contact start you’d like to receive a CloudWatch
-- event indicating an upcoming pass.
contactData_prePassStartTime :: Lens.Lens' ContactData (Prelude.Maybe Prelude.UTCTime)
contactData_prePassStartTime = Lens.lens (\ContactData' {prePassStartTime} -> prePassStartTime) (\s@ContactData' {} a -> s {prePassStartTime = a} :: ContactData) Prelude.. Lens.mapping Core._Time

-- | Error message of a contact.
contactData_errorMessage :: Lens.Lens' ContactData (Prelude.Maybe Prelude.Text)
contactData_errorMessage = Lens.lens (\ContactData' {errorMessage} -> errorMessage) (\s@ContactData' {} a -> s {errorMessage = a} :: ContactData)

-- | Tags assigned to a contact.
contactData_tags :: Lens.Lens' ContactData (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
contactData_tags = Lens.lens (\ContactData' {tags} -> tags) (\s@ContactData' {} a -> s {tags = a} :: ContactData) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ContactData where
  parseJSON =
    Core.withObject
      "ContactData"
      ( \x ->
          ContactData'
            Prelude.<$> (x Core..:? "contactStatus")
            Prelude.<*> (x Core..:? "missionProfileArn")
            Prelude.<*> (x Core..:? "startTime")
            Prelude.<*> (x Core..:? "satelliteArn")
            Prelude.<*> (x Core..:? "maximumElevation")
            Prelude.<*> (x Core..:? "groundStation")
            Prelude.<*> (x Core..:? "endTime")
            Prelude.<*> (x Core..:? "contactId")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "postPassEndTime")
            Prelude.<*> (x Core..:? "prePassStartTime")
            Prelude.<*> (x Core..:? "errorMessage")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ContactData

instance Prelude.NFData ContactData
