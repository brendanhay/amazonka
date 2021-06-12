{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.UpdateQualificationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateQualificationType@ operation modifies the attributes of an
-- existing Qualification type, which is represented by a QualificationType
-- data structure. Only the owner of a Qualification type can modify its
-- attributes.
--
-- Most attributes of a Qualification type can be changed after the type
-- has been created. However, the Name and Keywords fields cannot be
-- modified. The RetryDelayInSeconds parameter can be modified or added to
-- change the delay or to enable retries, but RetryDelayInSeconds cannot be
-- used to disable retries.
--
-- You can use this operation to update the test for a Qualification type.
-- The test is updated based on the values specified for the Test,
-- TestDurationInSeconds and AnswerKey parameters. All three parameters
-- specify the updated test. If you are updating the test for a type, you
-- must specify the Test and TestDurationInSeconds parameters. The
-- AnswerKey parameter is optional; omitting it specifies that the updated
-- test does not have an answer key.
--
-- If you omit the Test parameter, the test for the Qualification type is
-- unchanged. There is no way to remove a test from a Qualification type
-- that has one. If the type already has a test, you cannot update it to be
-- AutoGranted. If the Qualification type does not have a test and one is
-- provided by an update, the type will henceforth have a test.
--
-- If you want to update the test duration or answer key for an existing
-- test without changing the questions, you must specify a Test parameter
-- with the original questions, along with the updated values.
--
-- If you provide an updated Test but no AnswerKey, the new test will not
-- have an answer key. Requests for such Qualifications must be granted
-- manually.
--
-- You can also update the AutoGranted and AutoGrantedValue attributes of
-- the Qualification type.
module Network.AWS.MechanicalTurk.UpdateQualificationType
  ( -- * Creating a Request
    UpdateQualificationType (..),
    newUpdateQualificationType,

    -- * Request Lenses
    updateQualificationType_retryDelayInSeconds,
    updateQualificationType_autoGranted,
    updateQualificationType_qualificationTypeStatus,
    updateQualificationType_testDurationInSeconds,
    updateQualificationType_description,
    updateQualificationType_test,
    updateQualificationType_answerKey,
    updateQualificationType_autoGrantedValue,
    updateQualificationType_qualificationTypeId,

    -- * Destructuring the Response
    UpdateQualificationTypeResponse (..),
    newUpdateQualificationTypeResponse,

    -- * Response Lenses
    updateQualificationTypeResponse_qualificationType,
    updateQualificationTypeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQualificationType' smart constructor.
data UpdateQualificationType = UpdateQualificationType'
  { -- | The amount of time, in seconds, that Workers must wait after requesting
    -- a Qualification of the specified Qualification type before they can
    -- retry the Qualification request. It is not possible to disable retries
    -- for a Qualification type after it has been created with retries enabled.
    -- If you want to disable retries, you must dispose of the existing
    -- retry-enabled Qualification type using DisposeQualificationType and then
    -- create a new Qualification type with retries disabled using
    -- CreateQualificationType.
    retryDelayInSeconds :: Core.Maybe Core.Integer,
    -- | Specifies whether requests for the Qualification type are granted
    -- immediately, without prompting the Worker with a Qualification test.
    --
    -- Constraints: If the Test parameter is specified, this parameter cannot
    -- be true.
    autoGranted :: Core.Maybe Core.Bool,
    -- | The new status of the Qualification type - Active | Inactive
    qualificationTypeStatus :: Core.Maybe QualificationTypeStatus,
    -- | The number of seconds the Worker has to complete the Qualification test,
    -- starting from the time the Worker requests the Qualification.
    testDurationInSeconds :: Core.Maybe Core.Integer,
    -- | The new description of the Qualification type.
    description :: Core.Maybe Core.Text,
    -- | The questions for the Qualification test a Worker must answer correctly
    -- to obtain a Qualification of this type. If this parameter is specified,
    -- @TestDurationInSeconds@ must also be specified.
    --
    -- Constraints: Must not be longer than 65535 bytes. Must be a QuestionForm
    -- data structure. This parameter cannot be specified if AutoGranted is
    -- true.
    --
    -- Constraints: None. If not specified, the Worker may request the
    -- Qualification without answering any questions.
    test :: Core.Maybe Core.Text,
    -- | The answers to the Qualification test specified in the Test parameter,
    -- in the form of an AnswerKey data structure.
    answerKey :: Core.Maybe Core.Text,
    -- | The Qualification value to use for automatically granted Qualifications.
    -- This parameter is used only if the AutoGranted parameter is true.
    autoGrantedValue :: Core.Maybe Core.Int,
    -- | The ID of the Qualification type to update.
    qualificationTypeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQualificationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retryDelayInSeconds', 'updateQualificationType_retryDelayInSeconds' - The amount of time, in seconds, that Workers must wait after requesting
-- a Qualification of the specified Qualification type before they can
-- retry the Qualification request. It is not possible to disable retries
-- for a Qualification type after it has been created with retries enabled.
-- If you want to disable retries, you must dispose of the existing
-- retry-enabled Qualification type using DisposeQualificationType and then
-- create a new Qualification type with retries disabled using
-- CreateQualificationType.
--
-- 'autoGranted', 'updateQualificationType_autoGranted' - Specifies whether requests for the Qualification type are granted
-- immediately, without prompting the Worker with a Qualification test.
--
-- Constraints: If the Test parameter is specified, this parameter cannot
-- be true.
--
-- 'qualificationTypeStatus', 'updateQualificationType_qualificationTypeStatus' - The new status of the Qualification type - Active | Inactive
--
-- 'testDurationInSeconds', 'updateQualificationType_testDurationInSeconds' - The number of seconds the Worker has to complete the Qualification test,
-- starting from the time the Worker requests the Qualification.
--
-- 'description', 'updateQualificationType_description' - The new description of the Qualification type.
--
-- 'test', 'updateQualificationType_test' - The questions for the Qualification test a Worker must answer correctly
-- to obtain a Qualification of this type. If this parameter is specified,
-- @TestDurationInSeconds@ must also be specified.
--
-- Constraints: Must not be longer than 65535 bytes. Must be a QuestionForm
-- data structure. This parameter cannot be specified if AutoGranted is
-- true.
--
-- Constraints: None. If not specified, the Worker may request the
-- Qualification without answering any questions.
--
-- 'answerKey', 'updateQualificationType_answerKey' - The answers to the Qualification test specified in the Test parameter,
-- in the form of an AnswerKey data structure.
--
-- 'autoGrantedValue', 'updateQualificationType_autoGrantedValue' - The Qualification value to use for automatically granted Qualifications.
-- This parameter is used only if the AutoGranted parameter is true.
--
-- 'qualificationTypeId', 'updateQualificationType_qualificationTypeId' - The ID of the Qualification type to update.
newUpdateQualificationType ::
  -- | 'qualificationTypeId'
  Core.Text ->
  UpdateQualificationType
newUpdateQualificationType pQualificationTypeId_ =
  UpdateQualificationType'
    { retryDelayInSeconds =
        Core.Nothing,
      autoGranted = Core.Nothing,
      qualificationTypeStatus = Core.Nothing,
      testDurationInSeconds = Core.Nothing,
      description = Core.Nothing,
      test = Core.Nothing,
      answerKey = Core.Nothing,
      autoGrantedValue = Core.Nothing,
      qualificationTypeId = pQualificationTypeId_
    }

-- | The amount of time, in seconds, that Workers must wait after requesting
-- a Qualification of the specified Qualification type before they can
-- retry the Qualification request. It is not possible to disable retries
-- for a Qualification type after it has been created with retries enabled.
-- If you want to disable retries, you must dispose of the existing
-- retry-enabled Qualification type using DisposeQualificationType and then
-- create a new Qualification type with retries disabled using
-- CreateQualificationType.
updateQualificationType_retryDelayInSeconds :: Lens.Lens' UpdateQualificationType (Core.Maybe Core.Integer)
updateQualificationType_retryDelayInSeconds = Lens.lens (\UpdateQualificationType' {retryDelayInSeconds} -> retryDelayInSeconds) (\s@UpdateQualificationType' {} a -> s {retryDelayInSeconds = a} :: UpdateQualificationType)

-- | Specifies whether requests for the Qualification type are granted
-- immediately, without prompting the Worker with a Qualification test.
--
-- Constraints: If the Test parameter is specified, this parameter cannot
-- be true.
updateQualificationType_autoGranted :: Lens.Lens' UpdateQualificationType (Core.Maybe Core.Bool)
updateQualificationType_autoGranted = Lens.lens (\UpdateQualificationType' {autoGranted} -> autoGranted) (\s@UpdateQualificationType' {} a -> s {autoGranted = a} :: UpdateQualificationType)

-- | The new status of the Qualification type - Active | Inactive
updateQualificationType_qualificationTypeStatus :: Lens.Lens' UpdateQualificationType (Core.Maybe QualificationTypeStatus)
updateQualificationType_qualificationTypeStatus = Lens.lens (\UpdateQualificationType' {qualificationTypeStatus} -> qualificationTypeStatus) (\s@UpdateQualificationType' {} a -> s {qualificationTypeStatus = a} :: UpdateQualificationType)

-- | The number of seconds the Worker has to complete the Qualification test,
-- starting from the time the Worker requests the Qualification.
updateQualificationType_testDurationInSeconds :: Lens.Lens' UpdateQualificationType (Core.Maybe Core.Integer)
updateQualificationType_testDurationInSeconds = Lens.lens (\UpdateQualificationType' {testDurationInSeconds} -> testDurationInSeconds) (\s@UpdateQualificationType' {} a -> s {testDurationInSeconds = a} :: UpdateQualificationType)

-- | The new description of the Qualification type.
updateQualificationType_description :: Lens.Lens' UpdateQualificationType (Core.Maybe Core.Text)
updateQualificationType_description = Lens.lens (\UpdateQualificationType' {description} -> description) (\s@UpdateQualificationType' {} a -> s {description = a} :: UpdateQualificationType)

-- | The questions for the Qualification test a Worker must answer correctly
-- to obtain a Qualification of this type. If this parameter is specified,
-- @TestDurationInSeconds@ must also be specified.
--
-- Constraints: Must not be longer than 65535 bytes. Must be a QuestionForm
-- data structure. This parameter cannot be specified if AutoGranted is
-- true.
--
-- Constraints: None. If not specified, the Worker may request the
-- Qualification without answering any questions.
updateQualificationType_test :: Lens.Lens' UpdateQualificationType (Core.Maybe Core.Text)
updateQualificationType_test = Lens.lens (\UpdateQualificationType' {test} -> test) (\s@UpdateQualificationType' {} a -> s {test = a} :: UpdateQualificationType)

-- | The answers to the Qualification test specified in the Test parameter,
-- in the form of an AnswerKey data structure.
updateQualificationType_answerKey :: Lens.Lens' UpdateQualificationType (Core.Maybe Core.Text)
updateQualificationType_answerKey = Lens.lens (\UpdateQualificationType' {answerKey} -> answerKey) (\s@UpdateQualificationType' {} a -> s {answerKey = a} :: UpdateQualificationType)

-- | The Qualification value to use for automatically granted Qualifications.
-- This parameter is used only if the AutoGranted parameter is true.
updateQualificationType_autoGrantedValue :: Lens.Lens' UpdateQualificationType (Core.Maybe Core.Int)
updateQualificationType_autoGrantedValue = Lens.lens (\UpdateQualificationType' {autoGrantedValue} -> autoGrantedValue) (\s@UpdateQualificationType' {} a -> s {autoGrantedValue = a} :: UpdateQualificationType)

-- | The ID of the Qualification type to update.
updateQualificationType_qualificationTypeId :: Lens.Lens' UpdateQualificationType Core.Text
updateQualificationType_qualificationTypeId = Lens.lens (\UpdateQualificationType' {qualificationTypeId} -> qualificationTypeId) (\s@UpdateQualificationType' {} a -> s {qualificationTypeId = a} :: UpdateQualificationType)

instance Core.AWSRequest UpdateQualificationType where
  type
    AWSResponse UpdateQualificationType =
      UpdateQualificationTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateQualificationTypeResponse'
            Core.<$> (x Core..?> "QualificationType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateQualificationType

instance Core.NFData UpdateQualificationType

instance Core.ToHeaders UpdateQualificationType where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.UpdateQualificationType" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateQualificationType where
  toJSON UpdateQualificationType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RetryDelayInSeconds" Core..=)
              Core.<$> retryDelayInSeconds,
            ("AutoGranted" Core..=) Core.<$> autoGranted,
            ("QualificationTypeStatus" Core..=)
              Core.<$> qualificationTypeStatus,
            ("TestDurationInSeconds" Core..=)
              Core.<$> testDurationInSeconds,
            ("Description" Core..=) Core.<$> description,
            ("Test" Core..=) Core.<$> test,
            ("AnswerKey" Core..=) Core.<$> answerKey,
            ("AutoGrantedValue" Core..=)
              Core.<$> autoGrantedValue,
            Core.Just
              ("QualificationTypeId" Core..= qualificationTypeId)
          ]
      )

instance Core.ToPath UpdateQualificationType where
  toPath = Core.const "/"

instance Core.ToQuery UpdateQualificationType where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateQualificationTypeResponse' smart constructor.
data UpdateQualificationTypeResponse = UpdateQualificationTypeResponse'
  { -- | Contains a QualificationType data structure.
    qualificationType :: Core.Maybe QualificationType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQualificationTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualificationType', 'updateQualificationTypeResponse_qualificationType' - Contains a QualificationType data structure.
--
-- 'httpStatus', 'updateQualificationTypeResponse_httpStatus' - The response's http status code.
newUpdateQualificationTypeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateQualificationTypeResponse
newUpdateQualificationTypeResponse pHttpStatus_ =
  UpdateQualificationTypeResponse'
    { qualificationType =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains a QualificationType data structure.
updateQualificationTypeResponse_qualificationType :: Lens.Lens' UpdateQualificationTypeResponse (Core.Maybe QualificationType)
updateQualificationTypeResponse_qualificationType = Lens.lens (\UpdateQualificationTypeResponse' {qualificationType} -> qualificationType) (\s@UpdateQualificationTypeResponse' {} a -> s {qualificationType = a} :: UpdateQualificationTypeResponse)

-- | The response's http status code.
updateQualificationTypeResponse_httpStatus :: Lens.Lens' UpdateQualificationTypeResponse Core.Int
updateQualificationTypeResponse_httpStatus = Lens.lens (\UpdateQualificationTypeResponse' {httpStatus} -> httpStatus) (\s@UpdateQualificationTypeResponse' {} a -> s {httpStatus = a} :: UpdateQualificationTypeResponse)

instance Core.NFData UpdateQualificationTypeResponse
