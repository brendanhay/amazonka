{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.Qualification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.Qualification
  ( Qualification (..)
  -- * Smart constructor
  , mkQualification
  -- * Lenses
  , qGrantTime
  , qIntegerValue
  , qLocaleValue
  , qQualificationTypeId
  , qStatus
  , qWorkerId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.CustomerId as Types
import qualified Network.AWS.MechanicalTurk.Types.EntityId as Types
import qualified Network.AWS.MechanicalTurk.Types.Locale as Types
import qualified Network.AWS.MechanicalTurk.Types.QualificationStatus as Types
import qualified Network.AWS.Prelude as Core

-- | The Qualification data structure represents a Qualification assigned to a user, including the Qualification type and the value (score).
--
-- /See:/ 'mkQualification' smart constructor.
data Qualification = Qualification'
  { grantTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the Qualification was granted to the Worker. If the Worker's Qualification was revoked, and then re-granted based on a new Qualification request, GrantTime is the date and time of the last call to the AcceptQualificationRequest operation.
  , integerValue :: Core.Maybe Core.Int
    -- ^ The value (score) of the Qualification, if the Qualification has an integer value.
  , localeValue :: Core.Maybe Types.Locale
  , qualificationTypeId :: Core.Maybe Types.EntityId
    -- ^ The ID of the Qualification type for the Qualification.
  , status :: Core.Maybe Types.QualificationStatus
    -- ^ The status of the Qualification. Valid values are Granted | Revoked.
  , workerId :: Core.Maybe Types.CustomerId
    -- ^ The ID of the Worker who possesses the Qualification. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Qualification' value with any optional fields omitted.
mkQualification
    :: Qualification
mkQualification
  = Qualification'{grantTime = Core.Nothing,
                   integerValue = Core.Nothing, localeValue = Core.Nothing,
                   qualificationTypeId = Core.Nothing, status = Core.Nothing,
                   workerId = Core.Nothing}

-- | The date and time the Qualification was granted to the Worker. If the Worker's Qualification was revoked, and then re-granted based on a new Qualification request, GrantTime is the date and time of the last call to the AcceptQualificationRequest operation.
--
-- /Note:/ Consider using 'grantTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qGrantTime :: Lens.Lens' Qualification (Core.Maybe Core.NominalDiffTime)
qGrantTime = Lens.field @"grantTime"
{-# INLINEABLE qGrantTime #-}
{-# DEPRECATED grantTime "Use generic-lens or generic-optics with 'grantTime' instead"  #-}

-- | The value (score) of the Qualification, if the Qualification has an integer value.
--
-- /Note:/ Consider using 'integerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qIntegerValue :: Lens.Lens' Qualification (Core.Maybe Core.Int)
qIntegerValue = Lens.field @"integerValue"
{-# INLINEABLE qIntegerValue #-}
{-# DEPRECATED integerValue "Use generic-lens or generic-optics with 'integerValue' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'localeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qLocaleValue :: Lens.Lens' Qualification (Core.Maybe Types.Locale)
qLocaleValue = Lens.field @"localeValue"
{-# INLINEABLE qLocaleValue #-}
{-# DEPRECATED localeValue "Use generic-lens or generic-optics with 'localeValue' instead"  #-}

-- | The ID of the Qualification type for the Qualification.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qQualificationTypeId :: Lens.Lens' Qualification (Core.Maybe Types.EntityId)
qQualificationTypeId = Lens.field @"qualificationTypeId"
{-# INLINEABLE qQualificationTypeId #-}
{-# DEPRECATED qualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead"  #-}

-- | The status of the Qualification. Valid values are Granted | Revoked.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qStatus :: Lens.Lens' Qualification (Core.Maybe Types.QualificationStatus)
qStatus = Lens.field @"status"
{-# INLINEABLE qStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The ID of the Worker who possesses the Qualification. 
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qWorkerId :: Lens.Lens' Qualification (Core.Maybe Types.CustomerId)
qWorkerId = Lens.field @"workerId"
{-# INLINEABLE qWorkerId #-}
{-# DEPRECATED workerId "Use generic-lens or generic-optics with 'workerId' instead"  #-}

instance Core.FromJSON Qualification where
        parseJSON
          = Core.withObject "Qualification" Core.$
              \ x ->
                Qualification' Core.<$>
                  (x Core..:? "GrantTime") Core.<*> x Core..:? "IntegerValue"
                    Core.<*> x Core..:? "LocaleValue"
                    Core.<*> x Core..:? "QualificationTypeId"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "WorkerId"
