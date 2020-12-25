{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    uqtQualificationTypeId,
    uqtAnswerKey,
    uqtAutoGranted,
    uqtAutoGrantedValue,
    uqtDescription,
    uqtQualificationTypeStatus,
    uqtRetryDelayInSeconds,
    uqtTest,
    uqtTestDurationInSeconds,

    -- * Destructuring the response
    UpdateQualificationTypeResponse (..),
    mkUpdateQualificationTypeResponse,

    -- ** Response lenses
    uqtrrsQualificationType,
    uqtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateQualificationType' smart constructor.
data UpdateQualificationType = UpdateQualificationType'
  { -- | The ID of the Qualification type to update.
    qualificationTypeId :: Types.EntityId,
    -- | The answers to the Qualification test specified in the Test parameter, in the form of an AnswerKey data structure.
    answerKey :: Core.Maybe Types.String,
    -- | Specifies whether requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test.
    --
    -- Constraints: If the Test parameter is specified, this parameter cannot be true.
    autoGranted :: Core.Maybe Core.Bool,
    -- | The Qualification value to use for automatically granted Qualifications. This parameter is used only if the AutoGranted parameter is true.
    autoGrantedValue :: Core.Maybe Core.Int,
    -- | The new description of the Qualification type.
    description :: Core.Maybe Types.String,
    -- | The new status of the Qualification type - Active | Inactive
    qualificationTypeStatus :: Core.Maybe Types.QualificationTypeStatus,
    -- | The amount of time, in seconds, that Workers must wait after requesting a Qualification of the specified Qualification type before they can retry the Qualification request. It is not possible to disable retries for a Qualification type after it has been created with retries enabled. If you want to disable retries, you must dispose of the existing retry-enabled Qualification type using DisposeQualificationType and then create a new Qualification type with retries disabled using CreateQualificationType.
    retryDelayInSeconds :: Core.Maybe Core.Integer,
    -- | The questions for the Qualification test a Worker must answer correctly to obtain a Qualification of this type. If this parameter is specified, @TestDurationInSeconds@ must also be specified.
    --
    -- Constraints: Must not be longer than 65535 bytes. Must be a QuestionForm data structure. This parameter cannot be specified if AutoGranted is true.
    -- Constraints: None. If not specified, the Worker may request the Qualification without answering any questions.
    test :: Core.Maybe Types.String,
    -- | The number of seconds the Worker has to complete the Qualification test, starting from the time the Worker requests the Qualification.
    testDurationInSeconds :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateQualificationType' value with any optional fields omitted.
mkUpdateQualificationType ::
  -- | 'qualificationTypeId'
  Types.EntityId ->
  UpdateQualificationType
mkUpdateQualificationType qualificationTypeId =
  UpdateQualificationType'
    { qualificationTypeId,
      answerKey = Core.Nothing,
      autoGranted = Core.Nothing,
      autoGrantedValue = Core.Nothing,
      description = Core.Nothing,
      qualificationTypeStatus = Core.Nothing,
      retryDelayInSeconds = Core.Nothing,
      test = Core.Nothing,
      testDurationInSeconds = Core.Nothing
    }

-- | The ID of the Qualification type to update.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtQualificationTypeId :: Lens.Lens' UpdateQualificationType Types.EntityId
uqtQualificationTypeId = Lens.field @"qualificationTypeId"
{-# DEPRECATED uqtQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | The answers to the Qualification test specified in the Test parameter, in the form of an AnswerKey data structure.
--
-- /Note:/ Consider using 'answerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtAnswerKey :: Lens.Lens' UpdateQualificationType (Core.Maybe Types.String)
uqtAnswerKey = Lens.field @"answerKey"
{-# DEPRECATED uqtAnswerKey "Use generic-lens or generic-optics with 'answerKey' instead." #-}

-- | Specifies whether requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test.
--
-- Constraints: If the Test parameter is specified, this parameter cannot be true.
--
-- /Note:/ Consider using 'autoGranted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtAutoGranted :: Lens.Lens' UpdateQualificationType (Core.Maybe Core.Bool)
uqtAutoGranted = Lens.field @"autoGranted"
{-# DEPRECATED uqtAutoGranted "Use generic-lens or generic-optics with 'autoGranted' instead." #-}

-- | The Qualification value to use for automatically granted Qualifications. This parameter is used only if the AutoGranted parameter is true.
--
-- /Note:/ Consider using 'autoGrantedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtAutoGrantedValue :: Lens.Lens' UpdateQualificationType (Core.Maybe Core.Int)
uqtAutoGrantedValue = Lens.field @"autoGrantedValue"
{-# DEPRECATED uqtAutoGrantedValue "Use generic-lens or generic-optics with 'autoGrantedValue' instead." #-}

-- | The new description of the Qualification type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtDescription :: Lens.Lens' UpdateQualificationType (Core.Maybe Types.String)
uqtDescription = Lens.field @"description"
{-# DEPRECATED uqtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new status of the Qualification type - Active | Inactive
--
-- /Note:/ Consider using 'qualificationTypeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtQualificationTypeStatus :: Lens.Lens' UpdateQualificationType (Core.Maybe Types.QualificationTypeStatus)
uqtQualificationTypeStatus = Lens.field @"qualificationTypeStatus"
{-# DEPRECATED uqtQualificationTypeStatus "Use generic-lens or generic-optics with 'qualificationTypeStatus' instead." #-}

-- | The amount of time, in seconds, that Workers must wait after requesting a Qualification of the specified Qualification type before they can retry the Qualification request. It is not possible to disable retries for a Qualification type after it has been created with retries enabled. If you want to disable retries, you must dispose of the existing retry-enabled Qualification type using DisposeQualificationType and then create a new Qualification type with retries disabled using CreateQualificationType.
--
-- /Note:/ Consider using 'retryDelayInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtRetryDelayInSeconds :: Lens.Lens' UpdateQualificationType (Core.Maybe Core.Integer)
uqtRetryDelayInSeconds = Lens.field @"retryDelayInSeconds"
{-# DEPRECATED uqtRetryDelayInSeconds "Use generic-lens or generic-optics with 'retryDelayInSeconds' instead." #-}

-- | The questions for the Qualification test a Worker must answer correctly to obtain a Qualification of this type. If this parameter is specified, @TestDurationInSeconds@ must also be specified.
--
-- Constraints: Must not be longer than 65535 bytes. Must be a QuestionForm data structure. This parameter cannot be specified if AutoGranted is true.
-- Constraints: None. If not specified, the Worker may request the Qualification without answering any questions.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtTest :: Lens.Lens' UpdateQualificationType (Core.Maybe Types.String)
uqtTest = Lens.field @"test"
{-# DEPRECATED uqtTest "Use generic-lens or generic-optics with 'test' instead." #-}

-- | The number of seconds the Worker has to complete the Qualification test, starting from the time the Worker requests the Qualification.
--
-- /Note:/ Consider using 'testDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtTestDurationInSeconds :: Lens.Lens' UpdateQualificationType (Core.Maybe Core.Integer)
uqtTestDurationInSeconds = Lens.field @"testDurationInSeconds"
{-# DEPRECATED uqtTestDurationInSeconds "Use generic-lens or generic-optics with 'testDurationInSeconds' instead." #-}

instance Core.FromJSON UpdateQualificationType where
  toJSON UpdateQualificationType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("QualificationTypeId" Core..= qualificationTypeId),
            ("AnswerKey" Core..=) Core.<$> answerKey,
            ("AutoGranted" Core..=) Core.<$> autoGranted,
            ("AutoGrantedValue" Core..=) Core.<$> autoGrantedValue,
            ("Description" Core..=) Core.<$> description,
            ("QualificationTypeStatus" Core..=)
              Core.<$> qualificationTypeStatus,
            ("RetryDelayInSeconds" Core..=) Core.<$> retryDelayInSeconds,
            ("Test" Core..=) Core.<$> test,
            ("TestDurationInSeconds" Core..=) Core.<$> testDurationInSeconds
          ]
      )

instance Core.AWSRequest UpdateQualificationType where
  type Rs UpdateQualificationType = UpdateQualificationTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.UpdateQualificationType"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateQualificationTypeResponse'
            Core.<$> (x Core..:? "QualificationType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateQualificationTypeResponse' smart constructor.
data UpdateQualificationTypeResponse = UpdateQualificationTypeResponse'
  { -- | Contains a QualificationType data structure.
    qualificationType :: Core.Maybe Types.QualificationType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateQualificationTypeResponse' value with any optional fields omitted.
mkUpdateQualificationTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateQualificationTypeResponse
mkUpdateQualificationTypeResponse responseStatus =
  UpdateQualificationTypeResponse'
    { qualificationType =
        Core.Nothing,
      responseStatus
    }

-- | Contains a QualificationType data structure.
--
-- /Note:/ Consider using 'qualificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtrrsQualificationType :: Lens.Lens' UpdateQualificationTypeResponse (Core.Maybe Types.QualificationType)
uqtrrsQualificationType = Lens.field @"qualificationType"
{-# DEPRECATED uqtrrsQualificationType "Use generic-lens or generic-optics with 'qualificationType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqtrrsResponseStatus :: Lens.Lens' UpdateQualificationTypeResponse Core.Int
uqtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uqtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
