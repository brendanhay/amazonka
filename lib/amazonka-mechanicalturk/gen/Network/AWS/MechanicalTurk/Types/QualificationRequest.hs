{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.QualificationRequest
  ( QualificationRequest (..),

    -- * Smart constructor
    mkQualificationRequest,

    -- * Lenses
    quaQualificationRequestId,
    quaTest,
    quaQualificationTypeId,
    quaAnswer,
    quaWorkerId,
    quaSubmitTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The QualificationRequest data structure represents a request a Worker has made for a Qualification.
--
-- /See:/ 'mkQualificationRequest' smart constructor.
data QualificationRequest = QualificationRequest'
  { qualificationRequestId ::
      Lude.Maybe Lude.Text,
    test :: Lude.Maybe Lude.Text,
    qualificationTypeId :: Lude.Maybe Lude.Text,
    answer :: Lude.Maybe Lude.Text,
    workerId :: Lude.Maybe Lude.Text,
    submitTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QualificationRequest' with the minimum fields required to make a request.
--
-- * 'answer' - The Worker's answers for the Qualification type's test contained in a QuestionFormAnswers document, if the type has a test and the Worker has submitted answers. If the Worker does not provide any answers, Answer may be empty.
-- * 'qualificationRequestId' - The ID of the Qualification request, a unique identifier generated when the request was submitted.
-- * 'qualificationTypeId' - The ID of the Qualification type the Worker is requesting, as returned by the CreateQualificationType operation.
-- * 'submitTime' - The date and time the Qualification request had a status of Submitted. This is either the time the Worker submitted answers for a Qualification test, or the time the Worker requested the Qualification if the Qualification type does not have a test.
-- * 'test' - The contents of the Qualification test that was presented to the Worker, if the type has a test and the Worker has submitted answers. This value is identical to the QuestionForm associated with the Qualification type at the time the Worker requests the Qualification.
-- * 'workerId' - The ID of the Worker requesting the Qualification.
mkQualificationRequest ::
  QualificationRequest
mkQualificationRequest =
  QualificationRequest'
    { qualificationRequestId = Lude.Nothing,
      test = Lude.Nothing,
      qualificationTypeId = Lude.Nothing,
      answer = Lude.Nothing,
      workerId = Lude.Nothing,
      submitTime = Lude.Nothing
    }

-- | The ID of the Qualification request, a unique identifier generated when the request was submitted.
--
-- /Note:/ Consider using 'qualificationRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
quaQualificationRequestId :: Lens.Lens' QualificationRequest (Lude.Maybe Lude.Text)
quaQualificationRequestId = Lens.lens (qualificationRequestId :: QualificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {qualificationRequestId = a} :: QualificationRequest)
{-# DEPRECATED quaQualificationRequestId "Use generic-lens or generic-optics with 'qualificationRequestId' instead." #-}

-- | The contents of the Qualification test that was presented to the Worker, if the type has a test and the Worker has submitted answers. This value is identical to the QuestionForm associated with the Qualification type at the time the Worker requests the Qualification.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
quaTest :: Lens.Lens' QualificationRequest (Lude.Maybe Lude.Text)
quaTest = Lens.lens (test :: QualificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {test = a} :: QualificationRequest)
{-# DEPRECATED quaTest "Use generic-lens or generic-optics with 'test' instead." #-}

-- | The ID of the Qualification type the Worker is requesting, as returned by the CreateQualificationType operation.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
quaQualificationTypeId :: Lens.Lens' QualificationRequest (Lude.Maybe Lude.Text)
quaQualificationTypeId = Lens.lens (qualificationTypeId :: QualificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {qualificationTypeId = a} :: QualificationRequest)
{-# DEPRECATED quaQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | The Worker's answers for the Qualification type's test contained in a QuestionFormAnswers document, if the type has a test and the Worker has submitted answers. If the Worker does not provide any answers, Answer may be empty.
--
-- /Note:/ Consider using 'answer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
quaAnswer :: Lens.Lens' QualificationRequest (Lude.Maybe Lude.Text)
quaAnswer = Lens.lens (answer :: QualificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {answer = a} :: QualificationRequest)
{-# DEPRECATED quaAnswer "Use generic-lens or generic-optics with 'answer' instead." #-}

-- | The ID of the Worker requesting the Qualification.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
quaWorkerId :: Lens.Lens' QualificationRequest (Lude.Maybe Lude.Text)
quaWorkerId = Lens.lens (workerId :: QualificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {workerId = a} :: QualificationRequest)
{-# DEPRECATED quaWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

-- | The date and time the Qualification request had a status of Submitted. This is either the time the Worker submitted answers for a Qualification test, or the time the Worker requested the Qualification if the Qualification type does not have a test.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
quaSubmitTime :: Lens.Lens' QualificationRequest (Lude.Maybe Lude.Timestamp)
quaSubmitTime = Lens.lens (submitTime :: QualificationRequest -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: QualificationRequest)
{-# DEPRECATED quaSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON QualificationRequest where
  parseJSON =
    Lude.withObject
      "QualificationRequest"
      ( \x ->
          QualificationRequest'
            Lude.<$> (x Lude..:? "QualificationRequestId")
            Lude.<*> (x Lude..:? "Test")
            Lude.<*> (x Lude..:? "QualificationTypeId")
            Lude.<*> (x Lude..:? "Answer")
            Lude.<*> (x Lude..:? "WorkerId")
            Lude.<*> (x Lude..:? "SubmitTime")
      )
