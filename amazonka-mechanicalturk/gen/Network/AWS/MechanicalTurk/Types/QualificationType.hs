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
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.QualificationType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.QualificationTypeStatus

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
    qualificationTypeId :: Core.Maybe Core.Text,
    -- | The date and time the Qualification type was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | Specifies whether the Qualification type is one that a user can request
    -- through the Amazon Mechanical Turk web site, such as by taking a
    -- Qualification test. This value is False for Qualifications assigned
    -- automatically by the system. Valid values are True | False.
    isRequestable :: Core.Maybe Core.Bool,
    -- | The amount of time, in seconds, Workers must wait after taking the
    -- Qualification test before they can take it again. Workers can take a
    -- Qualification test multiple times if they were not granted the
    -- Qualification from a previous attempt, or if the test offers a gradient
    -- score and they want a better score. If not specified, retries are
    -- disabled and Workers can request a Qualification only once.
    retryDelayInSeconds :: Core.Maybe Core.Integer,
    -- | Specifies that requests for the Qualification type are granted
    -- immediately, without prompting the Worker with a Qualification test.
    -- Valid values are True | False.
    autoGranted :: Core.Maybe Core.Bool,
    -- | The status of the Qualification type. A Qualification type\'s status
    -- determines if users can apply to receive a Qualification of this type,
    -- and if HITs can be created with requirements based on this type. Valid
    -- values are Active | Inactive.
    qualificationTypeStatus :: Core.Maybe QualificationTypeStatus,
    -- | The name of the Qualification type. The type name is used to identify
    -- the type, and to find the type using a Qualification type search.
    name :: Core.Maybe Core.Text,
    -- | The amount of time, in seconds, given to a Worker to complete the
    -- Qualification test, beginning from the time the Worker requests the
    -- Qualification.
    testDurationInSeconds :: Core.Maybe Core.Integer,
    -- | A long description for the Qualification type.
    description :: Core.Maybe Core.Text,
    -- | The questions for a Qualification test associated with this
    -- Qualification type that a user can take to obtain a Qualification of
    -- this type. This parameter must be specified if AnswerKey is present. A
    -- Qualification type cannot have both a specified Test parameter and an
    -- AutoGranted value of true.
    test :: Core.Maybe Core.Text,
    -- | The answers to the Qualification test specified in the Test parameter.
    answerKey :: Core.Maybe Core.Text,
    -- | The Qualification integer value to use for automatically granted
    -- Qualifications, if AutoGranted is true. This is 1 by default.
    autoGrantedValue :: Core.Maybe Core.Int,
    -- | One or more words or phrases that describe theQualification type,
    -- separated by commas. The Keywords make the type easier to find using a
    -- search.
    keywords :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'creationTime', 'qualificationType_creationTime' - The date and time the Qualification type was created.
--
-- 'isRequestable', 'qualificationType_isRequestable' - Specifies whether the Qualification type is one that a user can request
-- through the Amazon Mechanical Turk web site, such as by taking a
-- Qualification test. This value is False for Qualifications assigned
-- automatically by the system. Valid values are True | False.
--
-- 'retryDelayInSeconds', 'qualificationType_retryDelayInSeconds' - The amount of time, in seconds, Workers must wait after taking the
-- Qualification test before they can take it again. Workers can take a
-- Qualification test multiple times if they were not granted the
-- Qualification from a previous attempt, or if the test offers a gradient
-- score and they want a better score. If not specified, retries are
-- disabled and Workers can request a Qualification only once.
--
-- 'autoGranted', 'qualificationType_autoGranted' - Specifies that requests for the Qualification type are granted
-- immediately, without prompting the Worker with a Qualification test.
-- Valid values are True | False.
--
-- 'qualificationTypeStatus', 'qualificationType_qualificationTypeStatus' - The status of the Qualification type. A Qualification type\'s status
-- determines if users can apply to receive a Qualification of this type,
-- and if HITs can be created with requirements based on this type. Valid
-- values are Active | Inactive.
--
-- 'name', 'qualificationType_name' - The name of the Qualification type. The type name is used to identify
-- the type, and to find the type using a Qualification type search.
--
-- 'testDurationInSeconds', 'qualificationType_testDurationInSeconds' - The amount of time, in seconds, given to a Worker to complete the
-- Qualification test, beginning from the time the Worker requests the
-- Qualification.
--
-- 'description', 'qualificationType_description' - A long description for the Qualification type.
--
-- 'test', 'qualificationType_test' - The questions for a Qualification test associated with this
-- Qualification type that a user can take to obtain a Qualification of
-- this type. This parameter must be specified if AnswerKey is present. A
-- Qualification type cannot have both a specified Test parameter and an
-- AutoGranted value of true.
--
-- 'answerKey', 'qualificationType_answerKey' - The answers to the Qualification test specified in the Test parameter.
--
-- 'autoGrantedValue', 'qualificationType_autoGrantedValue' - The Qualification integer value to use for automatically granted
-- Qualifications, if AutoGranted is true. This is 1 by default.
--
-- 'keywords', 'qualificationType_keywords' - One or more words or phrases that describe theQualification type,
-- separated by commas. The Keywords make the type easier to find using a
-- search.
newQualificationType ::
  QualificationType
newQualificationType =
  QualificationType'
    { qualificationTypeId =
        Core.Nothing,
      creationTime = Core.Nothing,
      isRequestable = Core.Nothing,
      retryDelayInSeconds = Core.Nothing,
      autoGranted = Core.Nothing,
      qualificationTypeStatus = Core.Nothing,
      name = Core.Nothing,
      testDurationInSeconds = Core.Nothing,
      description = Core.Nothing,
      test = Core.Nothing,
      answerKey = Core.Nothing,
      autoGrantedValue = Core.Nothing,
      keywords = Core.Nothing
    }

-- | A unique identifier for the Qualification type. A Qualification type is
-- given a Qualification type ID when you call the CreateQualificationType
-- operation.
qualificationType_qualificationTypeId :: Lens.Lens' QualificationType (Core.Maybe Core.Text)
qualificationType_qualificationTypeId = Lens.lens (\QualificationType' {qualificationTypeId} -> qualificationTypeId) (\s@QualificationType' {} a -> s {qualificationTypeId = a} :: QualificationType)

-- | The date and time the Qualification type was created.
qualificationType_creationTime :: Lens.Lens' QualificationType (Core.Maybe Core.UTCTime)
qualificationType_creationTime = Lens.lens (\QualificationType' {creationTime} -> creationTime) (\s@QualificationType' {} a -> s {creationTime = a} :: QualificationType) Core.. Lens.mapping Core._Time

-- | Specifies whether the Qualification type is one that a user can request
-- through the Amazon Mechanical Turk web site, such as by taking a
-- Qualification test. This value is False for Qualifications assigned
-- automatically by the system. Valid values are True | False.
qualificationType_isRequestable :: Lens.Lens' QualificationType (Core.Maybe Core.Bool)
qualificationType_isRequestable = Lens.lens (\QualificationType' {isRequestable} -> isRequestable) (\s@QualificationType' {} a -> s {isRequestable = a} :: QualificationType)

-- | The amount of time, in seconds, Workers must wait after taking the
-- Qualification test before they can take it again. Workers can take a
-- Qualification test multiple times if they were not granted the
-- Qualification from a previous attempt, or if the test offers a gradient
-- score and they want a better score. If not specified, retries are
-- disabled and Workers can request a Qualification only once.
qualificationType_retryDelayInSeconds :: Lens.Lens' QualificationType (Core.Maybe Core.Integer)
qualificationType_retryDelayInSeconds = Lens.lens (\QualificationType' {retryDelayInSeconds} -> retryDelayInSeconds) (\s@QualificationType' {} a -> s {retryDelayInSeconds = a} :: QualificationType)

-- | Specifies that requests for the Qualification type are granted
-- immediately, without prompting the Worker with a Qualification test.
-- Valid values are True | False.
qualificationType_autoGranted :: Lens.Lens' QualificationType (Core.Maybe Core.Bool)
qualificationType_autoGranted = Lens.lens (\QualificationType' {autoGranted} -> autoGranted) (\s@QualificationType' {} a -> s {autoGranted = a} :: QualificationType)

-- | The status of the Qualification type. A Qualification type\'s status
-- determines if users can apply to receive a Qualification of this type,
-- and if HITs can be created with requirements based on this type. Valid
-- values are Active | Inactive.
qualificationType_qualificationTypeStatus :: Lens.Lens' QualificationType (Core.Maybe QualificationTypeStatus)
qualificationType_qualificationTypeStatus = Lens.lens (\QualificationType' {qualificationTypeStatus} -> qualificationTypeStatus) (\s@QualificationType' {} a -> s {qualificationTypeStatus = a} :: QualificationType)

-- | The name of the Qualification type. The type name is used to identify
-- the type, and to find the type using a Qualification type search.
qualificationType_name :: Lens.Lens' QualificationType (Core.Maybe Core.Text)
qualificationType_name = Lens.lens (\QualificationType' {name} -> name) (\s@QualificationType' {} a -> s {name = a} :: QualificationType)

-- | The amount of time, in seconds, given to a Worker to complete the
-- Qualification test, beginning from the time the Worker requests the
-- Qualification.
qualificationType_testDurationInSeconds :: Lens.Lens' QualificationType (Core.Maybe Core.Integer)
qualificationType_testDurationInSeconds = Lens.lens (\QualificationType' {testDurationInSeconds} -> testDurationInSeconds) (\s@QualificationType' {} a -> s {testDurationInSeconds = a} :: QualificationType)

-- | A long description for the Qualification type.
qualificationType_description :: Lens.Lens' QualificationType (Core.Maybe Core.Text)
qualificationType_description = Lens.lens (\QualificationType' {description} -> description) (\s@QualificationType' {} a -> s {description = a} :: QualificationType)

-- | The questions for a Qualification test associated with this
-- Qualification type that a user can take to obtain a Qualification of
-- this type. This parameter must be specified if AnswerKey is present. A
-- Qualification type cannot have both a specified Test parameter and an
-- AutoGranted value of true.
qualificationType_test :: Lens.Lens' QualificationType (Core.Maybe Core.Text)
qualificationType_test = Lens.lens (\QualificationType' {test} -> test) (\s@QualificationType' {} a -> s {test = a} :: QualificationType)

-- | The answers to the Qualification test specified in the Test parameter.
qualificationType_answerKey :: Lens.Lens' QualificationType (Core.Maybe Core.Text)
qualificationType_answerKey = Lens.lens (\QualificationType' {answerKey} -> answerKey) (\s@QualificationType' {} a -> s {answerKey = a} :: QualificationType)

-- | The Qualification integer value to use for automatically granted
-- Qualifications, if AutoGranted is true. This is 1 by default.
qualificationType_autoGrantedValue :: Lens.Lens' QualificationType (Core.Maybe Core.Int)
qualificationType_autoGrantedValue = Lens.lens (\QualificationType' {autoGrantedValue} -> autoGrantedValue) (\s@QualificationType' {} a -> s {autoGrantedValue = a} :: QualificationType)

-- | One or more words or phrases that describe theQualification type,
-- separated by commas. The Keywords make the type easier to find using a
-- search.
qualificationType_keywords :: Lens.Lens' QualificationType (Core.Maybe Core.Text)
qualificationType_keywords = Lens.lens (\QualificationType' {keywords} -> keywords) (\s@QualificationType' {} a -> s {keywords = a} :: QualificationType)

instance Core.FromJSON QualificationType where
  parseJSON =
    Core.withObject
      "QualificationType"
      ( \x ->
          QualificationType'
            Core.<$> (x Core..:? "QualificationTypeId")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "IsRequestable")
            Core.<*> (x Core..:? "RetryDelayInSeconds")
            Core.<*> (x Core..:? "AutoGranted")
            Core.<*> (x Core..:? "QualificationTypeStatus")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "TestDurationInSeconds")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Test")
            Core.<*> (x Core..:? "AnswerKey")
            Core.<*> (x Core..:? "AutoGrantedValue")
            Core.<*> (x Core..:? "Keywords")
      )

instance Core.Hashable QualificationType

instance Core.NFData QualificationType
