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
-- Module      : Amazonka.MechanicalTurk.Types.QualificationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.QualificationType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MechanicalTurk.Types.QualificationTypeStatus
import qualified Amazonka.Prelude as Prelude

-- | The QualificationType data structure represents a Qualification type, a
-- description of a property of a Worker that must match the requirements
-- of a HIT for the Worker to be able to accept the HIT. The type also
-- describes how a Worker can obtain a Qualification of that type, such as
-- through a Qualification test.
--
-- /See:/ 'newQualificationType' smart constructor.
data QualificationType = QualificationType'
  { -- | A unique identifier for the Qualification type. A Qualification type is
    -- given a Qualification type ID when you call the CreateQualificationType
    -- operation.
    qualificationTypeId :: Prelude.Maybe Prelude.Text,
    -- | The Qualification integer value to use for automatically granted
    -- Qualifications, if AutoGranted is true. This is 1 by default.
    autoGrantedValue :: Prelude.Maybe Prelude.Int,
    -- | The name of the Qualification type. The type name is used to identify
    -- the type, and to find the type using a Qualification type search.
    name :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, given to a Worker to complete the
    -- Qualification test, beginning from the time the Worker requests the
    -- Qualification.
    testDurationInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | One or more words or phrases that describe theQualification type,
    -- separated by commas. The Keywords make the type easier to find using a
    -- search.
    keywords :: Prelude.Maybe Prelude.Text,
    -- | A long description for the Qualification type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The questions for a Qualification test associated with this
    -- Qualification type that a user can take to obtain a Qualification of
    -- this type. This parameter must be specified if AnswerKey is present. A
    -- Qualification type cannot have both a specified Test parameter and an
    -- AutoGranted value of true.
    test :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, Workers must wait after taking the
    -- Qualification test before they can take it again. Workers can take a
    -- Qualification test multiple times if they were not granted the
    -- Qualification from a previous attempt, or if the test offers a gradient
    -- score and they want a better score. If not specified, retries are
    -- disabled and Workers can request a Qualification only once.
    retryDelayInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The date and time the Qualification type was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The answers to the Qualification test specified in the Test parameter.
    answerKey :: Prelude.Maybe Prelude.Text,
    -- | Specifies that requests for the Qualification type are granted
    -- immediately, without prompting the Worker with a Qualification test.
    -- Valid values are True | False.
    autoGranted :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the Qualification type is one that a user can request
    -- through the Amazon Mechanical Turk web site, such as by taking a
    -- Qualification test. This value is False for Qualifications assigned
    -- automatically by the system. Valid values are True | False.
    isRequestable :: Prelude.Maybe Prelude.Bool,
    -- | The status of the Qualification type. A Qualification type\'s status
    -- determines if users can apply to receive a Qualification of this type,
    -- and if HITs can be created with requirements based on this type. Valid
    -- values are Active | Inactive.
    qualificationTypeStatus :: Prelude.Maybe QualificationTypeStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QualificationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualificationTypeId', 'qualificationType_qualificationTypeId' - A unique identifier for the Qualification type. A Qualification type is
-- given a Qualification type ID when you call the CreateQualificationType
-- operation.
--
-- 'autoGrantedValue', 'qualificationType_autoGrantedValue' - The Qualification integer value to use for automatically granted
-- Qualifications, if AutoGranted is true. This is 1 by default.
--
-- 'name', 'qualificationType_name' - The name of the Qualification type. The type name is used to identify
-- the type, and to find the type using a Qualification type search.
--
-- 'testDurationInSeconds', 'qualificationType_testDurationInSeconds' - The amount of time, in seconds, given to a Worker to complete the
-- Qualification test, beginning from the time the Worker requests the
-- Qualification.
--
-- 'keywords', 'qualificationType_keywords' - One or more words or phrases that describe theQualification type,
-- separated by commas. The Keywords make the type easier to find using a
-- search.
--
-- 'description', 'qualificationType_description' - A long description for the Qualification type.
--
-- 'test', 'qualificationType_test' - The questions for a Qualification test associated with this
-- Qualification type that a user can take to obtain a Qualification of
-- this type. This parameter must be specified if AnswerKey is present. A
-- Qualification type cannot have both a specified Test parameter and an
-- AutoGranted value of true.
--
-- 'retryDelayInSeconds', 'qualificationType_retryDelayInSeconds' - The amount of time, in seconds, Workers must wait after taking the
-- Qualification test before they can take it again. Workers can take a
-- Qualification test multiple times if they were not granted the
-- Qualification from a previous attempt, or if the test offers a gradient
-- score and they want a better score. If not specified, retries are
-- disabled and Workers can request a Qualification only once.
--
-- 'creationTime', 'qualificationType_creationTime' - The date and time the Qualification type was created.
--
-- 'answerKey', 'qualificationType_answerKey' - The answers to the Qualification test specified in the Test parameter.
--
-- 'autoGranted', 'qualificationType_autoGranted' - Specifies that requests for the Qualification type are granted
-- immediately, without prompting the Worker with a Qualification test.
-- Valid values are True | False.
--
-- 'isRequestable', 'qualificationType_isRequestable' - Specifies whether the Qualification type is one that a user can request
-- through the Amazon Mechanical Turk web site, such as by taking a
-- Qualification test. This value is False for Qualifications assigned
-- automatically by the system. Valid values are True | False.
--
-- 'qualificationTypeStatus', 'qualificationType_qualificationTypeStatus' - The status of the Qualification type. A Qualification type\'s status
-- determines if users can apply to receive a Qualification of this type,
-- and if HITs can be created with requirements based on this type. Valid
-- values are Active | Inactive.
newQualificationType ::
  QualificationType
newQualificationType =
  QualificationType'
    { qualificationTypeId =
        Prelude.Nothing,
      autoGrantedValue = Prelude.Nothing,
      name = Prelude.Nothing,
      testDurationInSeconds = Prelude.Nothing,
      keywords = Prelude.Nothing,
      description = Prelude.Nothing,
      test = Prelude.Nothing,
      retryDelayInSeconds = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      answerKey = Prelude.Nothing,
      autoGranted = Prelude.Nothing,
      isRequestable = Prelude.Nothing,
      qualificationTypeStatus = Prelude.Nothing
    }

-- | A unique identifier for the Qualification type. A Qualification type is
-- given a Qualification type ID when you call the CreateQualificationType
-- operation.
qualificationType_qualificationTypeId :: Lens.Lens' QualificationType (Prelude.Maybe Prelude.Text)
qualificationType_qualificationTypeId = Lens.lens (\QualificationType' {qualificationTypeId} -> qualificationTypeId) (\s@QualificationType' {} a -> s {qualificationTypeId = a} :: QualificationType)

-- | The Qualification integer value to use for automatically granted
-- Qualifications, if AutoGranted is true. This is 1 by default.
qualificationType_autoGrantedValue :: Lens.Lens' QualificationType (Prelude.Maybe Prelude.Int)
qualificationType_autoGrantedValue = Lens.lens (\QualificationType' {autoGrantedValue} -> autoGrantedValue) (\s@QualificationType' {} a -> s {autoGrantedValue = a} :: QualificationType)

-- | The name of the Qualification type. The type name is used to identify
-- the type, and to find the type using a Qualification type search.
qualificationType_name :: Lens.Lens' QualificationType (Prelude.Maybe Prelude.Text)
qualificationType_name = Lens.lens (\QualificationType' {name} -> name) (\s@QualificationType' {} a -> s {name = a} :: QualificationType)

-- | The amount of time, in seconds, given to a Worker to complete the
-- Qualification test, beginning from the time the Worker requests the
-- Qualification.
qualificationType_testDurationInSeconds :: Lens.Lens' QualificationType (Prelude.Maybe Prelude.Integer)
qualificationType_testDurationInSeconds = Lens.lens (\QualificationType' {testDurationInSeconds} -> testDurationInSeconds) (\s@QualificationType' {} a -> s {testDurationInSeconds = a} :: QualificationType)

-- | One or more words or phrases that describe theQualification type,
-- separated by commas. The Keywords make the type easier to find using a
-- search.
qualificationType_keywords :: Lens.Lens' QualificationType (Prelude.Maybe Prelude.Text)
qualificationType_keywords = Lens.lens (\QualificationType' {keywords} -> keywords) (\s@QualificationType' {} a -> s {keywords = a} :: QualificationType)

-- | A long description for the Qualification type.
qualificationType_description :: Lens.Lens' QualificationType (Prelude.Maybe Prelude.Text)
qualificationType_description = Lens.lens (\QualificationType' {description} -> description) (\s@QualificationType' {} a -> s {description = a} :: QualificationType)

-- | The questions for a Qualification test associated with this
-- Qualification type that a user can take to obtain a Qualification of
-- this type. This parameter must be specified if AnswerKey is present. A
-- Qualification type cannot have both a specified Test parameter and an
-- AutoGranted value of true.
qualificationType_test :: Lens.Lens' QualificationType (Prelude.Maybe Prelude.Text)
qualificationType_test = Lens.lens (\QualificationType' {test} -> test) (\s@QualificationType' {} a -> s {test = a} :: QualificationType)

-- | The amount of time, in seconds, Workers must wait after taking the
-- Qualification test before they can take it again. Workers can take a
-- Qualification test multiple times if they were not granted the
-- Qualification from a previous attempt, or if the test offers a gradient
-- score and they want a better score. If not specified, retries are
-- disabled and Workers can request a Qualification only once.
qualificationType_retryDelayInSeconds :: Lens.Lens' QualificationType (Prelude.Maybe Prelude.Integer)
qualificationType_retryDelayInSeconds = Lens.lens (\QualificationType' {retryDelayInSeconds} -> retryDelayInSeconds) (\s@QualificationType' {} a -> s {retryDelayInSeconds = a} :: QualificationType)

-- | The date and time the Qualification type was created.
qualificationType_creationTime :: Lens.Lens' QualificationType (Prelude.Maybe Prelude.UTCTime)
qualificationType_creationTime = Lens.lens (\QualificationType' {creationTime} -> creationTime) (\s@QualificationType' {} a -> s {creationTime = a} :: QualificationType) Prelude.. Lens.mapping Core._Time

-- | The answers to the Qualification test specified in the Test parameter.
qualificationType_answerKey :: Lens.Lens' QualificationType (Prelude.Maybe Prelude.Text)
qualificationType_answerKey = Lens.lens (\QualificationType' {answerKey} -> answerKey) (\s@QualificationType' {} a -> s {answerKey = a} :: QualificationType)

-- | Specifies that requests for the Qualification type are granted
-- immediately, without prompting the Worker with a Qualification test.
-- Valid values are True | False.
qualificationType_autoGranted :: Lens.Lens' QualificationType (Prelude.Maybe Prelude.Bool)
qualificationType_autoGranted = Lens.lens (\QualificationType' {autoGranted} -> autoGranted) (\s@QualificationType' {} a -> s {autoGranted = a} :: QualificationType)

-- | Specifies whether the Qualification type is one that a user can request
-- through the Amazon Mechanical Turk web site, such as by taking a
-- Qualification test. This value is False for Qualifications assigned
-- automatically by the system. Valid values are True | False.
qualificationType_isRequestable :: Lens.Lens' QualificationType (Prelude.Maybe Prelude.Bool)
qualificationType_isRequestable = Lens.lens (\QualificationType' {isRequestable} -> isRequestable) (\s@QualificationType' {} a -> s {isRequestable = a} :: QualificationType)

-- | The status of the Qualification type. A Qualification type\'s status
-- determines if users can apply to receive a Qualification of this type,
-- and if HITs can be created with requirements based on this type. Valid
-- values are Active | Inactive.
qualificationType_qualificationTypeStatus :: Lens.Lens' QualificationType (Prelude.Maybe QualificationTypeStatus)
qualificationType_qualificationTypeStatus = Lens.lens (\QualificationType' {qualificationTypeStatus} -> qualificationTypeStatus) (\s@QualificationType' {} a -> s {qualificationTypeStatus = a} :: QualificationType)

instance Core.FromJSON QualificationType where
  parseJSON =
    Core.withObject
      "QualificationType"
      ( \x ->
          QualificationType'
            Prelude.<$> (x Core..:? "QualificationTypeId")
            Prelude.<*> (x Core..:? "AutoGrantedValue")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "TestDurationInSeconds")
            Prelude.<*> (x Core..:? "Keywords")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Test")
            Prelude.<*> (x Core..:? "RetryDelayInSeconds")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "AnswerKey")
            Prelude.<*> (x Core..:? "AutoGranted")
            Prelude.<*> (x Core..:? "IsRequestable")
            Prelude.<*> (x Core..:? "QualificationTypeStatus")
      )

instance Prelude.Hashable QualificationType where
  hashWithSalt _salt QualificationType' {..} =
    _salt `Prelude.hashWithSalt` qualificationTypeId
      `Prelude.hashWithSalt` autoGrantedValue
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` testDurationInSeconds
      `Prelude.hashWithSalt` keywords
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` test
      `Prelude.hashWithSalt` retryDelayInSeconds
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` answerKey
      `Prelude.hashWithSalt` autoGranted
      `Prelude.hashWithSalt` isRequestable
      `Prelude.hashWithSalt` qualificationTypeStatus

instance Prelude.NFData QualificationType where
  rnf QualificationType' {..} =
    Prelude.rnf qualificationTypeId
      `Prelude.seq` Prelude.rnf autoGrantedValue
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf testDurationInSeconds
      `Prelude.seq` Prelude.rnf keywords
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf test
      `Prelude.seq` Prelude.rnf retryDelayInSeconds
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf answerKey
      `Prelude.seq` Prelude.rnf autoGranted
      `Prelude.seq` Prelude.rnf isRequestable
      `Prelude.seq` Prelude.rnf qualificationTypeStatus
