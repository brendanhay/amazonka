{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.UpdateQualificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateQualificationType@ operation modifies the attributes of an existing Qualification type, which is represented by a QualificationType data structure. Only the owner of a Qualification type can modify its attributes.
--
-- Most attributes of a Qualification type can be changed after the type has been created. However, the Name and Keywords fields cannot be modified. The RetryDelayInSeconds parameter can be modified or added to change the delay or to enable retries, but RetryDelayInSeconds cannot be used to disable retries.
-- You can use this operation to update the test for a Qualification type. The test is updated based on the values specified for the Test, TestDurationInSeconds and AnswerKey parameters. All three parameters specify the updated test. If you are updating the test for a type, you must specify the Test and TestDurationInSeconds parameters. The AnswerKey parameter is optional; omitting it specifies that the updated test does not have an answer key.
-- If you omit the Test parameter, the test for the Qualification type is unchanged. There is no way to remove a test from a Qualification type that has one. If the type already has a test, you cannot update it to be AutoGranted. If the Qualification type does not have a test and one is provided by an update, the type will henceforth have a test.
-- If you want to update the test duration or answer key for an existing test without changing the questions, you must specify a Test parameter with the original questions, along with the updated values.
-- If you provide an updated Test but no AnswerKey, the new test will not have an answer key. Requests for such Qualifications must be granted manually.
-- You can also update the AutoGranted and AutoGrantedValue attributes of the Qualification type.
module Network.AWS.MechanicalTurk.UpdateQualificationType
  ( -- * Creating a request
    UpdateQualificationType (..),
    mkUpdateQualificationType,

    -- ** Request lenses
    uqtTestDurationInSeconds,
    uqtQualificationTypeStatus,
    uqtAnswerKey,
    uqtTest,
    uqtAutoGranted,
    uqtAutoGrantedValue,
    uqtDescription,
    uqtRetryDelayInSeconds,
    uqtQualificationTypeId,

    -- * Destructuring the response
    UpdateQualificationTypeResponse (..),
    mkUpdateQualificationTypeResponse,

    -- ** Response lenses
    uqtrsQualificationType,
    uqtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateQualificationType' smart constructor.
data UpdateQualificationType = UpdateQualificationType'
  { testDurationInSeconds ::
      Lude.Maybe Lude.Integer,
    qualificationTypeStatus ::
      Lude.Maybe QualificationTypeStatus,
    answerKey :: Lude.Maybe Lude.Text,
    test :: Lude.Maybe Lude.Text,
    autoGranted :: Lude.Maybe Lude.Bool,
    autoGrantedValue :: Lude.Maybe Lude.Int,
    description :: Lude.Maybe Lude.Text,
    retryDelayInSeconds ::
      Lude.Maybe Lude.Integer,
    qualificationTypeId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateQualificationType' with the minimum fields required to make a request.
--
-- * 'answerKey' - The answers to the Qualification test specified in the Test parameter, in the form of an AnswerKey data structure.
-- * 'autoGranted' - Specifies whether requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test.
--
-- Constraints: If the Test parameter is specified, this parameter cannot be true.
-- * 'autoGrantedValue' - The Qualification value to use for automatically granted Qualifications. This parameter is used only if the AutoGranted parameter is true.
-- * 'description' - The new description of the Qualification type.
-- * 'qualificationTypeId' - The ID of the Qualification type to update.
-- * 'qualificationTypeStatus' - The new status of the Qualification type - Active | Inactive
-- * 'retryDelayInSeconds' - The amount of time, in seconds, that Workers must wait after requesting a Qualification of the specified Qualification type before they can retry the Qualification request. It is not possible to disable retries for a Qualification type after it has been created with retries enabled. If you want to disable retries, you must dispose of the existing retry-enabled Qualification type using DisposeQualificationType and then create a new Qualification type with retries disabled using CreateQualificationType.
-- * 'test' - The questions for the Qualification test a Worker must answer correctly to obtain a Qualification of this type. If this parameter is specified, @TestDurationInSeconds@ must also be specified.
--
-- Constraints: Must not be longer than 65535 bytes. Must be a QuestionForm data structure. This parameter cannot be specified if AutoGranted is true.
-- Constraints: None. If not specified, the Worker may request the Qualification without answering any questions.
-- * 'testDurationInSeconds' - The number of seconds the Worker has to complete the Qualification test, starting from the time the Worker requests the Qualification.
mkUpdateQualificationType ::
  -- | 'qualificationTypeId'
  Lude.Text ->
  UpdateQualificationType
mkUpdateQualificationType pQualificationTypeId_ =
  UpdateQualificationType'
    { testDurationInSeconds = Lude.Nothing,
      qualificationTypeStatus = Lude.Nothing,
      answerKey = Lude.Nothing,
      test = Lude.Nothing,
      autoGranted = Lude.Nothing,
      autoGrantedValue = Lude.Nothing,
      description = Lude.Nothing,
      retryDelayInSeconds = Lude.Nothing,
      qualificationTypeId = pQualificationTypeId_
    }

-- | The number of seconds the Worker has to complete the Qualification test, starting from the time the Worker requests the Qualification.
--
-- /Note:/ Consider using 'testDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtTestDurationInSeconds :: Lens.Lens' UpdateQualificationType (Lude.Maybe Lude.Integer)
uqtTestDurationInSeconds = Lens.lens (testDurationInSeconds :: UpdateQualificationType -> Lude.Maybe Lude.Integer) (\s a -> s {testDurationInSeconds = a} :: UpdateQualificationType)
{-# DEPRECATED uqtTestDurationInSeconds "Use generic-lens or generic-optics with 'testDurationInSeconds' instead." #-}

-- | The new status of the Qualification type - Active | Inactive
--
-- /Note:/ Consider using 'qualificationTypeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtQualificationTypeStatus :: Lens.Lens' UpdateQualificationType (Lude.Maybe QualificationTypeStatus)
uqtQualificationTypeStatus = Lens.lens (qualificationTypeStatus :: UpdateQualificationType -> Lude.Maybe QualificationTypeStatus) (\s a -> s {qualificationTypeStatus = a} :: UpdateQualificationType)
{-# DEPRECATED uqtQualificationTypeStatus "Use generic-lens or generic-optics with 'qualificationTypeStatus' instead." #-}

-- | The answers to the Qualification test specified in the Test parameter, in the form of an AnswerKey data structure.
--
-- /Note:/ Consider using 'answerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtAnswerKey :: Lens.Lens' UpdateQualificationType (Lude.Maybe Lude.Text)
uqtAnswerKey = Lens.lens (answerKey :: UpdateQualificationType -> Lude.Maybe Lude.Text) (\s a -> s {answerKey = a} :: UpdateQualificationType)
{-# DEPRECATED uqtAnswerKey "Use generic-lens or generic-optics with 'answerKey' instead." #-}

-- | The questions for the Qualification test a Worker must answer correctly to obtain a Qualification of this type. If this parameter is specified, @TestDurationInSeconds@ must also be specified.
--
-- Constraints: Must not be longer than 65535 bytes. Must be a QuestionForm data structure. This parameter cannot be specified if AutoGranted is true.
-- Constraints: None. If not specified, the Worker may request the Qualification without answering any questions.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtTest :: Lens.Lens' UpdateQualificationType (Lude.Maybe Lude.Text)
uqtTest = Lens.lens (test :: UpdateQualificationType -> Lude.Maybe Lude.Text) (\s a -> s {test = a} :: UpdateQualificationType)
{-# DEPRECATED uqtTest "Use generic-lens or generic-optics with 'test' instead." #-}

-- | Specifies whether requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test.
--
-- Constraints: If the Test parameter is specified, this parameter cannot be true.
--
-- /Note:/ Consider using 'autoGranted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtAutoGranted :: Lens.Lens' UpdateQualificationType (Lude.Maybe Lude.Bool)
uqtAutoGranted = Lens.lens (autoGranted :: UpdateQualificationType -> Lude.Maybe Lude.Bool) (\s a -> s {autoGranted = a} :: UpdateQualificationType)
{-# DEPRECATED uqtAutoGranted "Use generic-lens or generic-optics with 'autoGranted' instead." #-}

-- | The Qualification value to use for automatically granted Qualifications. This parameter is used only if the AutoGranted parameter is true.
--
-- /Note:/ Consider using 'autoGrantedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtAutoGrantedValue :: Lens.Lens' UpdateQualificationType (Lude.Maybe Lude.Int)
uqtAutoGrantedValue = Lens.lens (autoGrantedValue :: UpdateQualificationType -> Lude.Maybe Lude.Int) (\s a -> s {autoGrantedValue = a} :: UpdateQualificationType)
{-# DEPRECATED uqtAutoGrantedValue "Use generic-lens or generic-optics with 'autoGrantedValue' instead." #-}

-- | The new description of the Qualification type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtDescription :: Lens.Lens' UpdateQualificationType (Lude.Maybe Lude.Text)
uqtDescription = Lens.lens (description :: UpdateQualificationType -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateQualificationType)
{-# DEPRECATED uqtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The amount of time, in seconds, that Workers must wait after requesting a Qualification of the specified Qualification type before they can retry the Qualification request. It is not possible to disable retries for a Qualification type after it has been created with retries enabled. If you want to disable retries, you must dispose of the existing retry-enabled Qualification type using DisposeQualificationType and then create a new Qualification type with retries disabled using CreateQualificationType.
--
-- /Note:/ Consider using 'retryDelayInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtRetryDelayInSeconds :: Lens.Lens' UpdateQualificationType (Lude.Maybe Lude.Integer)
uqtRetryDelayInSeconds = Lens.lens (retryDelayInSeconds :: UpdateQualificationType -> Lude.Maybe Lude.Integer) (\s a -> s {retryDelayInSeconds = a} :: UpdateQualificationType)
{-# DEPRECATED uqtRetryDelayInSeconds "Use generic-lens or generic-optics with 'retryDelayInSeconds' instead." #-}

-- | The ID of the Qualification type to update.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtQualificationTypeId :: Lens.Lens' UpdateQualificationType Lude.Text
uqtQualificationTypeId = Lens.lens (qualificationTypeId :: UpdateQualificationType -> Lude.Text) (\s a -> s {qualificationTypeId = a} :: UpdateQualificationType)
{-# DEPRECATED uqtQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

instance Lude.AWSRequest UpdateQualificationType where
  type Rs UpdateQualificationType = UpdateQualificationTypeResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateQualificationTypeResponse'
            Lude.<$> (x Lude..?> "QualificationType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateQualificationType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.UpdateQualificationType" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateQualificationType where
  toJSON UpdateQualificationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TestDurationInSeconds" Lude..=) Lude.<$> testDurationInSeconds,
            ("QualificationTypeStatus" Lude..=)
              Lude.<$> qualificationTypeStatus,
            ("AnswerKey" Lude..=) Lude.<$> answerKey,
            ("Test" Lude..=) Lude.<$> test,
            ("AutoGranted" Lude..=) Lude.<$> autoGranted,
            ("AutoGrantedValue" Lude..=) Lude.<$> autoGrantedValue,
            ("Description" Lude..=) Lude.<$> description,
            ("RetryDelayInSeconds" Lude..=) Lude.<$> retryDelayInSeconds,
            Lude.Just ("QualificationTypeId" Lude..= qualificationTypeId)
          ]
      )

instance Lude.ToPath UpdateQualificationType where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateQualificationType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateQualificationTypeResponse' smart constructor.
data UpdateQualificationTypeResponse = UpdateQualificationTypeResponse'
  { qualificationType ::
      Lude.Maybe
        QualificationType,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateQualificationTypeResponse' with the minimum fields required to make a request.
--
-- * 'qualificationType' - Contains a QualificationType data structure.
-- * 'responseStatus' - The response status code.
mkUpdateQualificationTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateQualificationTypeResponse
mkUpdateQualificationTypeResponse pResponseStatus_ =
  UpdateQualificationTypeResponse'
    { qualificationType =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains a QualificationType data structure.
--
-- /Note:/ Consider using 'qualificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtrsQualificationType :: Lens.Lens' UpdateQualificationTypeResponse (Lude.Maybe QualificationType)
uqtrsQualificationType = Lens.lens (qualificationType :: UpdateQualificationTypeResponse -> Lude.Maybe QualificationType) (\s a -> s {qualificationType = a} :: UpdateQualificationTypeResponse)
{-# DEPRECATED uqtrsQualificationType "Use generic-lens or generic-optics with 'qualificationType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtrsResponseStatus :: Lens.Lens' UpdateQualificationTypeResponse Lude.Int
uqtrsResponseStatus = Lens.lens (responseStatus :: UpdateQualificationTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateQualificationTypeResponse)
{-# DEPRECATED uqtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
