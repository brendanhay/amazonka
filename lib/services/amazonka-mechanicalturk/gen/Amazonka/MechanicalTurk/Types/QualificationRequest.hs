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
-- Module      : Amazonka.MechanicalTurk.Types.QualificationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.QualificationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The QualificationRequest data structure represents a request a Worker
-- has made for a Qualification.
--
-- /See:/ 'newQualificationRequest' smart constructor.
data QualificationRequest = QualificationRequest'
  { -- | The Worker\'s answers for the Qualification type\'s test contained in a
    -- QuestionFormAnswers document, if the type has a test and the Worker has
    -- submitted answers. If the Worker does not provide any answers, Answer
    -- may be empty.
    answer :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Qualification request, a unique identifier generated when
    -- the request was submitted.
    qualificationRequestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Qualification type the Worker is requesting, as returned
    -- by the CreateQualificationType operation.
    qualificationTypeId :: Prelude.Maybe Prelude.Text,
    -- | The date and time the Qualification request had a status of Submitted.
    -- This is either the time the Worker submitted answers for a Qualification
    -- test, or the time the Worker requested the Qualification if the
    -- Qualification type does not have a test.
    submitTime :: Prelude.Maybe Data.POSIX,
    -- | The contents of the Qualification test that was presented to the Worker,
    -- if the type has a test and the Worker has submitted answers. This value
    -- is identical to the QuestionForm associated with the Qualification type
    -- at the time the Worker requests the Qualification.
    test :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Worker requesting the Qualification.
    workerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QualificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'answer', 'qualificationRequest_answer' - The Worker\'s answers for the Qualification type\'s test contained in a
-- QuestionFormAnswers document, if the type has a test and the Worker has
-- submitted answers. If the Worker does not provide any answers, Answer
-- may be empty.
--
-- 'qualificationRequestId', 'qualificationRequest_qualificationRequestId' - The ID of the Qualification request, a unique identifier generated when
-- the request was submitted.
--
-- 'qualificationTypeId', 'qualificationRequest_qualificationTypeId' - The ID of the Qualification type the Worker is requesting, as returned
-- by the CreateQualificationType operation.
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
newQualificationRequest ::
  QualificationRequest
newQualificationRequest =
  QualificationRequest'
    { answer = Prelude.Nothing,
      qualificationRequestId = Prelude.Nothing,
      qualificationTypeId = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      test = Prelude.Nothing,
      workerId = Prelude.Nothing
    }

-- | The Worker\'s answers for the Qualification type\'s test contained in a
-- QuestionFormAnswers document, if the type has a test and the Worker has
-- submitted answers. If the Worker does not provide any answers, Answer
-- may be empty.
qualificationRequest_answer :: Lens.Lens' QualificationRequest (Prelude.Maybe Prelude.Text)
qualificationRequest_answer = Lens.lens (\QualificationRequest' {answer} -> answer) (\s@QualificationRequest' {} a -> s {answer = a} :: QualificationRequest)

-- | The ID of the Qualification request, a unique identifier generated when
-- the request was submitted.
qualificationRequest_qualificationRequestId :: Lens.Lens' QualificationRequest (Prelude.Maybe Prelude.Text)
qualificationRequest_qualificationRequestId = Lens.lens (\QualificationRequest' {qualificationRequestId} -> qualificationRequestId) (\s@QualificationRequest' {} a -> s {qualificationRequestId = a} :: QualificationRequest)

-- | The ID of the Qualification type the Worker is requesting, as returned
-- by the CreateQualificationType operation.
qualificationRequest_qualificationTypeId :: Lens.Lens' QualificationRequest (Prelude.Maybe Prelude.Text)
qualificationRequest_qualificationTypeId = Lens.lens (\QualificationRequest' {qualificationTypeId} -> qualificationTypeId) (\s@QualificationRequest' {} a -> s {qualificationTypeId = a} :: QualificationRequest)

-- | The date and time the Qualification request had a status of Submitted.
-- This is either the time the Worker submitted answers for a Qualification
-- test, or the time the Worker requested the Qualification if the
-- Qualification type does not have a test.
qualificationRequest_submitTime :: Lens.Lens' QualificationRequest (Prelude.Maybe Prelude.UTCTime)
qualificationRequest_submitTime = Lens.lens (\QualificationRequest' {submitTime} -> submitTime) (\s@QualificationRequest' {} a -> s {submitTime = a} :: QualificationRequest) Prelude.. Lens.mapping Data._Time

-- | The contents of the Qualification test that was presented to the Worker,
-- if the type has a test and the Worker has submitted answers. This value
-- is identical to the QuestionForm associated with the Qualification type
-- at the time the Worker requests the Qualification.
qualificationRequest_test :: Lens.Lens' QualificationRequest (Prelude.Maybe Prelude.Text)
qualificationRequest_test = Lens.lens (\QualificationRequest' {test} -> test) (\s@QualificationRequest' {} a -> s {test = a} :: QualificationRequest)

-- | The ID of the Worker requesting the Qualification.
qualificationRequest_workerId :: Lens.Lens' QualificationRequest (Prelude.Maybe Prelude.Text)
qualificationRequest_workerId = Lens.lens (\QualificationRequest' {workerId} -> workerId) (\s@QualificationRequest' {} a -> s {workerId = a} :: QualificationRequest)

instance Data.FromJSON QualificationRequest where
  parseJSON =
    Data.withObject
      "QualificationRequest"
      ( \x ->
          QualificationRequest'
            Prelude.<$> (x Data..:? "Answer")
            Prelude.<*> (x Data..:? "QualificationRequestId")
            Prelude.<*> (x Data..:? "QualificationTypeId")
            Prelude.<*> (x Data..:? "SubmitTime")
            Prelude.<*> (x Data..:? "Test")
            Prelude.<*> (x Data..:? "WorkerId")
      )

instance Prelude.Hashable QualificationRequest where
  hashWithSalt _salt QualificationRequest' {..} =
    _salt
      `Prelude.hashWithSalt` answer
      `Prelude.hashWithSalt` qualificationRequestId
      `Prelude.hashWithSalt` qualificationTypeId
      `Prelude.hashWithSalt` submitTime
      `Prelude.hashWithSalt` test
      `Prelude.hashWithSalt` workerId

instance Prelude.NFData QualificationRequest where
  rnf QualificationRequest' {..} =
    Prelude.rnf answer
      `Prelude.seq` Prelude.rnf qualificationRequestId
      `Prelude.seq` Prelude.rnf qualificationTypeId
      `Prelude.seq` Prelude.rnf submitTime
      `Prelude.seq` Prelude.rnf test
      `Prelude.seq` Prelude.rnf workerId
