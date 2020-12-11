-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.Qualification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.Qualification
  ( Qualification (..),

    -- * Smart constructor
    mkQualification,

    -- * Lenses
    qStatus,
    qIntegerValue,
    qLocaleValue,
    qQualificationTypeId,
    qGrantTime,
    qWorkerId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.Locale
import Network.AWS.MechanicalTurk.Types.QualificationStatus
import qualified Network.AWS.Prelude as Lude

-- | The Qualification data structure represents a Qualification assigned to a user, including the Qualification type and the value (score).
--
-- /See:/ 'mkQualification' smart constructor.
data Qualification = Qualification'
  { status ::
      Lude.Maybe QualificationStatus,
    integerValue :: Lude.Maybe Lude.Int,
    localeValue :: Lude.Maybe Locale,
    qualificationTypeId :: Lude.Maybe Lude.Text,
    grantTime :: Lude.Maybe Lude.Timestamp,
    workerId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Qualification' with the minimum fields required to make a request.
--
-- * 'grantTime' - The date and time the Qualification was granted to the Worker. If the Worker's Qualification was revoked, and then re-granted based on a new Qualification request, GrantTime is the date and time of the last call to the AcceptQualificationRequest operation.
-- * 'integerValue' - The value (score) of the Qualification, if the Qualification has an integer value.
-- * 'localeValue' - Undocumented field.
-- * 'qualificationTypeId' - The ID of the Qualification type for the Qualification.
-- * 'status' - The status of the Qualification. Valid values are Granted | Revoked.
-- * 'workerId' - The ID of the Worker who possesses the Qualification.
mkQualification ::
  Qualification
mkQualification =
  Qualification'
    { status = Lude.Nothing,
      integerValue = Lude.Nothing,
      localeValue = Lude.Nothing,
      qualificationTypeId = Lude.Nothing,
      grantTime = Lude.Nothing,
      workerId = Lude.Nothing
    }

-- | The status of the Qualification. Valid values are Granted | Revoked.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qStatus :: Lens.Lens' Qualification (Lude.Maybe QualificationStatus)
qStatus = Lens.lens (status :: Qualification -> Lude.Maybe QualificationStatus) (\s a -> s {status = a} :: Qualification)
{-# DEPRECATED qStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The value (score) of the Qualification, if the Qualification has an integer value.
--
-- /Note:/ Consider using 'integerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qIntegerValue :: Lens.Lens' Qualification (Lude.Maybe Lude.Int)
qIntegerValue = Lens.lens (integerValue :: Qualification -> Lude.Maybe Lude.Int) (\s a -> s {integerValue = a} :: Qualification)
{-# DEPRECATED qIntegerValue "Use generic-lens or generic-optics with 'integerValue' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'localeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qLocaleValue :: Lens.Lens' Qualification (Lude.Maybe Locale)
qLocaleValue = Lens.lens (localeValue :: Qualification -> Lude.Maybe Locale) (\s a -> s {localeValue = a} :: Qualification)
{-# DEPRECATED qLocaleValue "Use generic-lens or generic-optics with 'localeValue' instead." #-}

-- | The ID of the Qualification type for the Qualification.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qQualificationTypeId :: Lens.Lens' Qualification (Lude.Maybe Lude.Text)
qQualificationTypeId = Lens.lens (qualificationTypeId :: Qualification -> Lude.Maybe Lude.Text) (\s a -> s {qualificationTypeId = a} :: Qualification)
{-# DEPRECATED qQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | The date and time the Qualification was granted to the Worker. If the Worker's Qualification was revoked, and then re-granted based on a new Qualification request, GrantTime is the date and time of the last call to the AcceptQualificationRequest operation.
--
-- /Note:/ Consider using 'grantTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qGrantTime :: Lens.Lens' Qualification (Lude.Maybe Lude.Timestamp)
qGrantTime = Lens.lens (grantTime :: Qualification -> Lude.Maybe Lude.Timestamp) (\s a -> s {grantTime = a} :: Qualification)
{-# DEPRECATED qGrantTime "Use generic-lens or generic-optics with 'grantTime' instead." #-}

-- | The ID of the Worker who possesses the Qualification.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qWorkerId :: Lens.Lens' Qualification (Lude.Maybe Lude.Text)
qWorkerId = Lens.lens (workerId :: Qualification -> Lude.Maybe Lude.Text) (\s a -> s {workerId = a} :: Qualification)
{-# DEPRECATED qWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

instance Lude.FromJSON Qualification where
  parseJSON =
    Lude.withObject
      "Qualification"
      ( \x ->
          Qualification'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "IntegerValue")
            Lude.<*> (x Lude..:? "LocaleValue")
            Lude.<*> (x Lude..:? "QualificationTypeId")
            Lude.<*> (x Lude..:? "GrantTime")
            Lude.<*> (x Lude..:? "WorkerId")
      )
