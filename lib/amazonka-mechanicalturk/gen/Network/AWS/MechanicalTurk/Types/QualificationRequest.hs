{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.QualificationRequest
  ( QualificationRequest (..)
  -- * Smart constructor
  , mkQualificationRequest
  -- * Lenses
  , qrfAnswer
  , qrfQualificationRequestId
  , qrfQualificationTypeId
  , qrfSubmitTime
  , qrfTest
  , qrfWorkerId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.CustomerId as Types
import qualified Network.AWS.MechanicalTurk.Types.QualificationTypeId as Types
import qualified Network.AWS.Prelude as Core

-- | The QualificationRequest data structure represents a request a Worker has made for a Qualification. 
--
-- /See:/ 'mkQualificationRequest' smart constructor.
data QualificationRequest = QualificationRequest'
  { answer :: Core.Maybe Core.Text
    -- ^ The Worker's answers for the Qualification type's test contained in a QuestionFormAnswers document, if the type has a test and the Worker has submitted answers. If the Worker does not provide any answers, Answer may be empty. 
  , qualificationRequestId :: Core.Maybe Core.Text
    -- ^ The ID of the Qualification request, a unique identifier generated when the request was submitted. 
  , qualificationTypeId :: Core.Maybe Types.QualificationTypeId
    -- ^ The ID of the Qualification type the Worker is requesting, as returned by the CreateQualificationType operation. 
  , submitTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the Qualification request had a status of Submitted. This is either the time the Worker submitted answers for a Qualification test, or the time the Worker requested the Qualification if the Qualification type does not have a test. 
  , test :: Core.Maybe Core.Text
    -- ^ The contents of the Qualification test that was presented to the Worker, if the type has a test and the Worker has submitted answers. This value is identical to the QuestionForm associated with the Qualification type at the time the Worker requests the Qualification.
  , workerId :: Core.Maybe Types.CustomerId
    -- ^ The ID of the Worker requesting the Qualification.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'QualificationRequest' value with any optional fields omitted.
mkQualificationRequest
    :: QualificationRequest
mkQualificationRequest
  = QualificationRequest'{answer = Core.Nothing,
                          qualificationRequestId = Core.Nothing,
                          qualificationTypeId = Core.Nothing, submitTime = Core.Nothing,
                          test = Core.Nothing, workerId = Core.Nothing}

-- | The Worker's answers for the Qualification type's test contained in a QuestionFormAnswers document, if the type has a test and the Worker has submitted answers. If the Worker does not provide any answers, Answer may be empty. 
--
-- /Note:/ Consider using 'answer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrfAnswer :: Lens.Lens' QualificationRequest (Core.Maybe Core.Text)
qrfAnswer = Lens.field @"answer"
{-# INLINEABLE qrfAnswer #-}
{-# DEPRECATED answer "Use generic-lens or generic-optics with 'answer' instead"  #-}

-- | The ID of the Qualification request, a unique identifier generated when the request was submitted. 
--
-- /Note:/ Consider using 'qualificationRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrfQualificationRequestId :: Lens.Lens' QualificationRequest (Core.Maybe Core.Text)
qrfQualificationRequestId = Lens.field @"qualificationRequestId"
{-# INLINEABLE qrfQualificationRequestId #-}
{-# DEPRECATED qualificationRequestId "Use generic-lens or generic-optics with 'qualificationRequestId' instead"  #-}

-- | The ID of the Qualification type the Worker is requesting, as returned by the CreateQualificationType operation. 
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrfQualificationTypeId :: Lens.Lens' QualificationRequest (Core.Maybe Types.QualificationTypeId)
qrfQualificationTypeId = Lens.field @"qualificationTypeId"
{-# INLINEABLE qrfQualificationTypeId #-}
{-# DEPRECATED qualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead"  #-}

-- | The date and time the Qualification request had a status of Submitted. This is either the time the Worker submitted answers for a Qualification test, or the time the Worker requested the Qualification if the Qualification type does not have a test. 
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrfSubmitTime :: Lens.Lens' QualificationRequest (Core.Maybe Core.NominalDiffTime)
qrfSubmitTime = Lens.field @"submitTime"
{-# INLINEABLE qrfSubmitTime #-}
{-# DEPRECATED submitTime "Use generic-lens or generic-optics with 'submitTime' instead"  #-}

-- | The contents of the Qualification test that was presented to the Worker, if the type has a test and the Worker has submitted answers. This value is identical to the QuestionForm associated with the Qualification type at the time the Worker requests the Qualification.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrfTest :: Lens.Lens' QualificationRequest (Core.Maybe Core.Text)
qrfTest = Lens.field @"test"
{-# INLINEABLE qrfTest #-}
{-# DEPRECATED test "Use generic-lens or generic-optics with 'test' instead"  #-}

-- | The ID of the Worker requesting the Qualification.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrfWorkerId :: Lens.Lens' QualificationRequest (Core.Maybe Types.CustomerId)
qrfWorkerId = Lens.field @"workerId"
{-# INLINEABLE qrfWorkerId #-}
{-# DEPRECATED workerId "Use generic-lens or generic-optics with 'workerId' instead"  #-}

instance Core.FromJSON QualificationRequest where
        parseJSON
          = Core.withObject "QualificationRequest" Core.$
              \ x ->
                QualificationRequest' Core.<$>
                  (x Core..:? "Answer") Core.<*> x Core..:? "QualificationRequestId"
                    Core.<*> x Core..:? "QualificationTypeId"
                    Core.<*> x Core..:? "SubmitTime"
                    Core.<*> x Core..:? "Test"
                    Core.<*> x Core..:? "WorkerId"
