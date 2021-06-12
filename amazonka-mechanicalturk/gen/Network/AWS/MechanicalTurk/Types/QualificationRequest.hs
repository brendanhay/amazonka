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
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.QualificationRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The QualificationRequest data structure represents a request a Worker
-- has made for a Qualification.
--
-- /See:/ 'newQualificationRequest' smart constructor.
data QualificationRequest = QualificationRequest'
  { -- | The ID of the Qualification type the Worker is requesting, as returned
    -- by the CreateQualificationType operation.
    qualificationTypeId :: Core.Maybe Core.Text,
    -- | The Worker\'s answers for the Qualification type\'s test contained in a
    -- QuestionFormAnswers document, if the type has a test and the Worker has
    -- submitted answers. If the Worker does not provide any answers, Answer
    -- may be empty.
    answer :: Core.Maybe Core.Text,
    -- | The date and time the Qualification request had a status of Submitted.
    -- This is either the time the Worker submitted answers for a Qualification
    -- test, or the time the Worker requested the Qualification if the
    -- Qualification type does not have a test.
    submitTime :: Core.Maybe Core.POSIX,
    -- | The contents of the Qualification test that was presented to the Worker,
    -- if the type has a test and the Worker has submitted answers. This value
    -- is identical to the QuestionForm associated with the Qualification type
    -- at the time the Worker requests the Qualification.
    test :: Core.Maybe Core.Text,
    -- | The ID of the Worker requesting the Qualification.
    workerId :: Core.Maybe Core.Text,
    -- | The ID of the Qualification request, a unique identifier generated when
    -- the request was submitted.
    qualificationRequestId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QualificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualificationTypeId', 'qualificationRequest_qualificationTypeId' - The ID of the Qualification type the Worker is requesting, as returned
-- by the CreateQualificationType operation.
--
-- 'answer', 'qualificationRequest_answer' - The Worker\'s answers for the Qualification type\'s test contained in a
-- QuestionFormAnswers document, if the type has a test and the Worker has
-- submitted answers. If the Worker does not provide any answers, Answer
-- may be empty.
--
-- 'submitTime', 'qualificationRequest_submitTime' - The date and time the Qualification request had a status of Submitted.
-- This is either the time the Worker submitted answers for a Qualification
-- test, or the time the Worker requested the Qualification if the
-- Qualification type does not have a test.
--
-- 'test', 'qualificationRequest_test' - The contents of the Qualification test that was presented to the Worker,
-- if the type has a test and the Worker has submitted answers. This value
-- is identical to the QuestionForm associated with the Qualification type
-- at the time the Worker requests the Qualification.
--
-- 'workerId', 'qualificationRequest_workerId' - The ID of the Worker requesting the Qualification.
--
-- 'qualificationRequestId', 'qualificationRequest_qualificationRequestId' - The ID of the Qualification request, a unique identifier generated when
-- the request was submitted.
newQualificationRequest ::
  QualificationRequest
newQualificationRequest =
  QualificationRequest'
    { qualificationTypeId =
        Core.Nothing,
      answer = Core.Nothing,
      submitTime = Core.Nothing,
      test = Core.Nothing,
      workerId = Core.Nothing,
      qualificationRequestId = Core.Nothing
    }

-- | The ID of the Qualification type the Worker is requesting, as returned
-- by the CreateQualificationType operation.
qualificationRequest_qualificationTypeId :: Lens.Lens' QualificationRequest (Core.Maybe Core.Text)
qualificationRequest_qualificationTypeId = Lens.lens (\QualificationRequest' {qualificationTypeId} -> qualificationTypeId) (\s@QualificationRequest' {} a -> s {qualificationTypeId = a} :: QualificationRequest)

-- | The Worker\'s answers for the Qualification type\'s test contained in a
-- QuestionFormAnswers document, if the type has a test and the Worker has
-- submitted answers. If the Worker does not provide any answers, Answer
-- may be empty.
qualificationRequest_answer :: Lens.Lens' QualificationRequest (Core.Maybe Core.Text)
qualificationRequest_answer = Lens.lens (\QualificationRequest' {answer} -> answer) (\s@QualificationRequest' {} a -> s {answer = a} :: QualificationRequest)

-- | The date and time the Qualification request had a status of Submitted.
-- This is either the time the Worker submitted answers for a Qualification
-- test, or the time the Worker requested the Qualification if the
-- Qualification type does not have a test.
qualificationRequest_submitTime :: Lens.Lens' QualificationRequest (Core.Maybe Core.UTCTime)
qualificationRequest_submitTime = Lens.lens (\QualificationRequest' {submitTime} -> submitTime) (\s@QualificationRequest' {} a -> s {submitTime = a} :: QualificationRequest) Core.. Lens.mapping Core._Time

-- | The contents of the Qualification test that was presented to the Worker,
-- if the type has a test and the Worker has submitted answers. This value
-- is identical to the QuestionForm associated with the Qualification type
-- at the time the Worker requests the Qualification.
qualificationRequest_test :: Lens.Lens' QualificationRequest (Core.Maybe Core.Text)
qualificationRequest_test = Lens.lens (\QualificationRequest' {test} -> test) (\s@QualificationRequest' {} a -> s {test = a} :: QualificationRequest)

-- | The ID of the Worker requesting the Qualification.
qualificationRequest_workerId :: Lens.Lens' QualificationRequest (Core.Maybe Core.Text)
qualificationRequest_workerId = Lens.lens (\QualificationRequest' {workerId} -> workerId) (\s@QualificationRequest' {} a -> s {workerId = a} :: QualificationRequest)

-- | The ID of the Qualification request, a unique identifier generated when
-- the request was submitted.
qualificationRequest_qualificationRequestId :: Lens.Lens' QualificationRequest (Core.Maybe Core.Text)
qualificationRequest_qualificationRequestId = Lens.lens (\QualificationRequest' {qualificationRequestId} -> qualificationRequestId) (\s@QualificationRequest' {} a -> s {qualificationRequestId = a} :: QualificationRequest)

instance Core.FromJSON QualificationRequest where
  parseJSON =
    Core.withObject
      "QualificationRequest"
      ( \x ->
          QualificationRequest'
            Core.<$> (x Core..:? "QualificationTypeId")
            Core.<*> (x Core..:? "Answer")
            Core.<*> (x Core..:? "SubmitTime")
            Core.<*> (x Core..:? "Test")
            Core.<*> (x Core..:? "WorkerId")
            Core.<*> (x Core..:? "QualificationRequestId")
      )

instance Core.Hashable QualificationRequest

instance Core.NFData QualificationRequest
