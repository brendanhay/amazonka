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
-- Module      : Network.AWS.MechanicalTurk.Types.Qualification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.Qualification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.Locale
import Network.AWS.MechanicalTurk.Types.QualificationStatus

-- | The Qualification data structure represents a Qualification assigned to
-- a user, including the Qualification type and the value (score).
--
-- /See:/ 'newQualification' smart constructor.
data Qualification = Qualification'
  { -- | The ID of the Qualification type for the Qualification.
    qualificationTypeId :: Core.Maybe Core.Text,
    -- | The status of the Qualification. Valid values are Granted | Revoked.
    status :: Core.Maybe QualificationStatus,
    -- | The date and time the Qualification was granted to the Worker. If the
    -- Worker\'s Qualification was revoked, and then re-granted based on a new
    -- Qualification request, GrantTime is the date and time of the last call
    -- to the AcceptQualificationRequest operation.
    grantTime :: Core.Maybe Core.POSIX,
    -- | The ID of the Worker who possesses the Qualification.
    workerId :: Core.Maybe Core.Text,
    localeValue :: Core.Maybe Locale,
    -- | The value (score) of the Qualification, if the Qualification has an
    -- integer value.
    integerValue :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Qualification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualificationTypeId', 'qualification_qualificationTypeId' - The ID of the Qualification type for the Qualification.
--
-- 'status', 'qualification_status' - The status of the Qualification. Valid values are Granted | Revoked.
--
-- 'grantTime', 'qualification_grantTime' - The date and time the Qualification was granted to the Worker. If the
-- Worker\'s Qualification was revoked, and then re-granted based on a new
-- Qualification request, GrantTime is the date and time of the last call
-- to the AcceptQualificationRequest operation.
--
-- 'workerId', 'qualification_workerId' - The ID of the Worker who possesses the Qualification.
--
-- 'localeValue', 'qualification_localeValue' - Undocumented member.
--
-- 'integerValue', 'qualification_integerValue' - The value (score) of the Qualification, if the Qualification has an
-- integer value.
newQualification ::
  Qualification
newQualification =
  Qualification'
    { qualificationTypeId = Core.Nothing,
      status = Core.Nothing,
      grantTime = Core.Nothing,
      workerId = Core.Nothing,
      localeValue = Core.Nothing,
      integerValue = Core.Nothing
    }

-- | The ID of the Qualification type for the Qualification.
qualification_qualificationTypeId :: Lens.Lens' Qualification (Core.Maybe Core.Text)
qualification_qualificationTypeId = Lens.lens (\Qualification' {qualificationTypeId} -> qualificationTypeId) (\s@Qualification' {} a -> s {qualificationTypeId = a} :: Qualification)

-- | The status of the Qualification. Valid values are Granted | Revoked.
qualification_status :: Lens.Lens' Qualification (Core.Maybe QualificationStatus)
qualification_status = Lens.lens (\Qualification' {status} -> status) (\s@Qualification' {} a -> s {status = a} :: Qualification)

-- | The date and time the Qualification was granted to the Worker. If the
-- Worker\'s Qualification was revoked, and then re-granted based on a new
-- Qualification request, GrantTime is the date and time of the last call
-- to the AcceptQualificationRequest operation.
qualification_grantTime :: Lens.Lens' Qualification (Core.Maybe Core.UTCTime)
qualification_grantTime = Lens.lens (\Qualification' {grantTime} -> grantTime) (\s@Qualification' {} a -> s {grantTime = a} :: Qualification) Core.. Lens.mapping Core._Time

-- | The ID of the Worker who possesses the Qualification.
qualification_workerId :: Lens.Lens' Qualification (Core.Maybe Core.Text)
qualification_workerId = Lens.lens (\Qualification' {workerId} -> workerId) (\s@Qualification' {} a -> s {workerId = a} :: Qualification)

-- | Undocumented member.
qualification_localeValue :: Lens.Lens' Qualification (Core.Maybe Locale)
qualification_localeValue = Lens.lens (\Qualification' {localeValue} -> localeValue) (\s@Qualification' {} a -> s {localeValue = a} :: Qualification)

-- | The value (score) of the Qualification, if the Qualification has an
-- integer value.
qualification_integerValue :: Lens.Lens' Qualification (Core.Maybe Core.Int)
qualification_integerValue = Lens.lens (\Qualification' {integerValue} -> integerValue) (\s@Qualification' {} a -> s {integerValue = a} :: Qualification)

instance Core.FromJSON Qualification where
  parseJSON =
    Core.withObject
      "Qualification"
      ( \x ->
          Qualification'
            Core.<$> (x Core..:? "QualificationTypeId")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "GrantTime")
            Core.<*> (x Core..:? "WorkerId")
            Core.<*> (x Core..:? "LocaleValue")
            Core.<*> (x Core..:? "IntegerValue")
      )

instance Core.Hashable Qualification

instance Core.NFData Qualification
