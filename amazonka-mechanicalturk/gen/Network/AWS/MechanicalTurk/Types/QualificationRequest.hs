{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The QualificationRequest data structure represents a request a Worker
-- has made for a Qualification.
--
-- /See:/ 'newQualificationRequest' smart constructor.
data QualificationRequest = QualificationRequest'
  { -- | The ID of the Qualification type the Worker is requesting, as returned
    -- by the CreateQualificationType operation.
    qualificationTypeId :: Prelude.Maybe Prelude.Text,
    -- | The Worker\'s answers for the Qualification type\'s test contained in a
    -- QuestionFormAnswers document, if the type has a test and the Worker has
    -- submitted answers. If the Worker does not provide any answers, Answer
    -- may be empty.
    answer :: Prelude.Maybe Prelude.Text,
    -- | The date and time the Qualification request had a status of Submitted.
    -- This is either the time the Worker submitted answers for a Qualification
    -- test, or the time the Worker requested the Qualification if the
    -- Qualification type does not have a test.
    submitTime :: Prelude.Maybe Prelude.POSIX,
    -- | The contents of the Qualification test that was presented to the Worker,
    -- if the type has a test and the Worker has submitted answers. This value
    -- is identical to the QuestionForm associated with the Qualification type
    -- at the time the Worker requests the Qualification.
    test :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Worker requesting the Qualification.
    workerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Qualification request, a unique identifier generated when
    -- the request was submitted.
    qualificationRequestId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      answer = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      test = Prelude.Nothing,
      workerId = Prelude.Nothing,
      qualificationRequestId = Prelude.Nothing
    }

-- | The ID of the Qualification type the Worker is requesting, as returned
-- by the CreateQualificationType operation.
qualificationRequest_qualificationTypeId :: Lens.Lens' QualificationRequest (Prelude.Maybe Prelude.Text)
qualificationRequest_qualificationTypeId = Lens.lens (\QualificationRequest' {qualificationTypeId} -> qualificationTypeId) (\s@QualificationRequest' {} a -> s {qualificationTypeId = a} :: QualificationRequest)

-- | The Worker\'s answers for the Qualification type\'s test contained in a
-- QuestionFormAnswers document, if the type has a test and the Worker has
-- submitted answers. If the Worker does not provide any answers, Answer
-- may be empty.
qualificationRequest_answer :: Lens.Lens' QualificationRequest (Prelude.Maybe Prelude.Text)
qualificationRequest_answer = Lens.lens (\QualificationRequest' {answer} -> answer) (\s@QualificationRequest' {} a -> s {answer = a} :: QualificationRequest)

-- | The date and time the Qualification request had a status of Submitted.
-- This is either the time the Worker submitted answers for a Qualification
-- test, or the time the Worker requested the Qualification if the
-- Qualification type does not have a test.
qualificationRequest_submitTime :: Lens.Lens' QualificationRequest (Prelude.Maybe Prelude.UTCTime)
qualificationRequest_submitTime = Lens.lens (\QualificationRequest' {submitTime} -> submitTime) (\s@QualificationRequest' {} a -> s {submitTime = a} :: QualificationRequest) Prelude.. Lens.mapping Prelude._Time

-- | The contents of the Qualification test that was presented to the Worker,
-- if the type has a test and the Worker has submitted answers. This value
-- is identical to the QuestionForm associated with the Qualification type
-- at the time the Worker requests the Qualification.
qualificationRequest_test :: Lens.Lens' QualificationRequest (Prelude.Maybe Prelude.Text)
qualificationRequest_test = Lens.lens (\QualificationRequest' {test} -> test) (\s@QualificationRequest' {} a -> s {test = a} :: QualificationRequest)

-- | The ID of the Worker requesting the Qualification.
qualificationRequest_workerId :: Lens.Lens' QualificationRequest (Prelude.Maybe Prelude.Text)
qualificationRequest_workerId = Lens.lens (\QualificationRequest' {workerId} -> workerId) (\s@QualificationRequest' {} a -> s {workerId = a} :: QualificationRequest)

-- | The ID of the Qualification request, a unique identifier generated when
-- the request was submitted.
qualificationRequest_qualificationRequestId :: Lens.Lens' QualificationRequest (Prelude.Maybe Prelude.Text)
qualificationRequest_qualificationRequestId = Lens.lens (\QualificationRequest' {qualificationRequestId} -> qualificationRequestId) (\s@QualificationRequest' {} a -> s {qualificationRequestId = a} :: QualificationRequest)

instance Prelude.FromJSON QualificationRequest where
  parseJSON =
    Prelude.withObject
      "QualificationRequest"
      ( \x ->
          QualificationRequest'
            Prelude.<$> (x Prelude..:? "QualificationTypeId")
            Prelude.<*> (x Prelude..:? "Answer")
            Prelude.<*> (x Prelude..:? "SubmitTime")
            Prelude.<*> (x Prelude..:? "Test")
            Prelude.<*> (x Prelude..:? "WorkerId")
            Prelude.<*> (x Prelude..:? "QualificationRequestId")
      )

instance Prelude.Hashable QualificationRequest

instance Prelude.NFData QualificationRequest
