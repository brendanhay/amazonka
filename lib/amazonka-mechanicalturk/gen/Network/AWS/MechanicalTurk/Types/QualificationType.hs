{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.QualificationType
  ( QualificationType (..),

    -- * Smart constructor
    mkQualificationType,

    -- * Lenses
    qtCreationTime,
    qtTestDurationInSeconds,
    qtQualificationTypeStatus,
    qtAnswerKey,
    qtTest,
    qtQualificationTypeId,
    qtName,
    qtKeywords,
    qtAutoGranted,
    qtAutoGrantedValue,
    qtDescription,
    qtIsRequestable,
    qtRetryDelayInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.QualificationTypeStatus
import qualified Network.AWS.Prelude as Lude

-- | The QualificationType data structure represents a Qualification type, a description of a property of a Worker that must match the requirements of a HIT for the Worker to be able to accept the HIT. The type also describes how a Worker can obtain a Qualification of that type, such as through a Qualification test.
--
-- /See:/ 'mkQualificationType' smart constructor.
data QualificationType = QualificationType'
  { -- | The date and time the Qualification type was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The amount of time, in seconds, given to a Worker to complete the Qualification test, beginning from the time the Worker requests the Qualification.
    testDurationInSeconds :: Lude.Maybe Lude.Integer,
    -- | The status of the Qualification type. A Qualification type's status determines if users can apply to receive a Qualification of this type, and if HITs can be created with requirements based on this type. Valid values are Active | Inactive.
    qualificationTypeStatus :: Lude.Maybe QualificationTypeStatus,
    -- | The answers to the Qualification test specified in the Test parameter.
    answerKey :: Lude.Maybe Lude.Text,
    -- | The questions for a Qualification test associated with this Qualification type that a user can take to obtain a Qualification of this type. This parameter must be specified if AnswerKey is present. A Qualification type cannot have both a specified Test parameter and an AutoGranted value of true.
    test :: Lude.Maybe Lude.Text,
    -- | A unique identifier for the Qualification type. A Qualification type is given a Qualification type ID when you call the CreateQualificationType operation.
    qualificationTypeId :: Lude.Maybe Lude.Text,
    -- | The name of the Qualification type. The type name is used to identify the type, and to find the type using a Qualification type search.
    name :: Lude.Maybe Lude.Text,
    -- | One or more words or phrases that describe theQualification type, separated by commas. The Keywords make the type easier to find using a search.
    keywords :: Lude.Maybe Lude.Text,
    -- | Specifies that requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test. Valid values are True | False.
    autoGranted :: Lude.Maybe Lude.Bool,
    -- | The Qualification integer value to use for automatically granted Qualifications, if AutoGranted is true. This is 1 by default.
    autoGrantedValue :: Lude.Maybe Lude.Int,
    -- | A long description for the Qualification type.
    description :: Lude.Maybe Lude.Text,
    -- | Specifies whether the Qualification type is one that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test. This value is False for Qualifications assigned automatically by the system. Valid values are True | False.
    isRequestable :: Lude.Maybe Lude.Bool,
    -- | The amount of time, in seconds, Workers must wait after taking the Qualification test before they can take it again. Workers can take a Qualification test multiple times if they were not granted the Qualification from a previous attempt, or if the test offers a gradient score and they want a better score. If not specified, retries are disabled and Workers can request a Qualification only once.
    retryDelayInSeconds :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QualificationType' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time the Qualification type was created.
-- * 'testDurationInSeconds' - The amount of time, in seconds, given to a Worker to complete the Qualification test, beginning from the time the Worker requests the Qualification.
-- * 'qualificationTypeStatus' - The status of the Qualification type. A Qualification type's status determines if users can apply to receive a Qualification of this type, and if HITs can be created with requirements based on this type. Valid values are Active | Inactive.
-- * 'answerKey' - The answers to the Qualification test specified in the Test parameter.
-- * 'test' - The questions for a Qualification test associated with this Qualification type that a user can take to obtain a Qualification of this type. This parameter must be specified if AnswerKey is present. A Qualification type cannot have both a specified Test parameter and an AutoGranted value of true.
-- * 'qualificationTypeId' - A unique identifier for the Qualification type. A Qualification type is given a Qualification type ID when you call the CreateQualificationType operation.
-- * 'name' - The name of the Qualification type. The type name is used to identify the type, and to find the type using a Qualification type search.
-- * 'keywords' - One or more words or phrases that describe theQualification type, separated by commas. The Keywords make the type easier to find using a search.
-- * 'autoGranted' - Specifies that requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test. Valid values are True | False.
-- * 'autoGrantedValue' - The Qualification integer value to use for automatically granted Qualifications, if AutoGranted is true. This is 1 by default.
-- * 'description' - A long description for the Qualification type.
-- * 'isRequestable' - Specifies whether the Qualification type is one that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test. This value is False for Qualifications assigned automatically by the system. Valid values are True | False.
-- * 'retryDelayInSeconds' - The amount of time, in seconds, Workers must wait after taking the Qualification test before they can take it again. Workers can take a Qualification test multiple times if they were not granted the Qualification from a previous attempt, or if the test offers a gradient score and they want a better score. If not specified, retries are disabled and Workers can request a Qualification only once.
mkQualificationType ::
  QualificationType
mkQualificationType =
  QualificationType'
    { creationTime = Lude.Nothing,
      testDurationInSeconds = Lude.Nothing,
      qualificationTypeStatus = Lude.Nothing,
      answerKey = Lude.Nothing,
      test = Lude.Nothing,
      qualificationTypeId = Lude.Nothing,
      name = Lude.Nothing,
      keywords = Lude.Nothing,
      autoGranted = Lude.Nothing,
      autoGrantedValue = Lude.Nothing,
      description = Lude.Nothing,
      isRequestable = Lude.Nothing,
      retryDelayInSeconds = Lude.Nothing
    }

-- | The date and time the Qualification type was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtCreationTime :: Lens.Lens' QualificationType (Lude.Maybe Lude.Timestamp)
qtCreationTime = Lens.lens (creationTime :: QualificationType -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: QualificationType)
{-# DEPRECATED qtCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The amount of time, in seconds, given to a Worker to complete the Qualification test, beginning from the time the Worker requests the Qualification.
--
-- /Note:/ Consider using 'testDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtTestDurationInSeconds :: Lens.Lens' QualificationType (Lude.Maybe Lude.Integer)
qtTestDurationInSeconds = Lens.lens (testDurationInSeconds :: QualificationType -> Lude.Maybe Lude.Integer) (\s a -> s {testDurationInSeconds = a} :: QualificationType)
{-# DEPRECATED qtTestDurationInSeconds "Use generic-lens or generic-optics with 'testDurationInSeconds' instead." #-}

-- | The status of the Qualification type. A Qualification type's status determines if users can apply to receive a Qualification of this type, and if HITs can be created with requirements based on this type. Valid values are Active | Inactive.
--
-- /Note:/ Consider using 'qualificationTypeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtQualificationTypeStatus :: Lens.Lens' QualificationType (Lude.Maybe QualificationTypeStatus)
qtQualificationTypeStatus = Lens.lens (qualificationTypeStatus :: QualificationType -> Lude.Maybe QualificationTypeStatus) (\s a -> s {qualificationTypeStatus = a} :: QualificationType)
{-# DEPRECATED qtQualificationTypeStatus "Use generic-lens or generic-optics with 'qualificationTypeStatus' instead." #-}

-- | The answers to the Qualification test specified in the Test parameter.
--
-- /Note:/ Consider using 'answerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtAnswerKey :: Lens.Lens' QualificationType (Lude.Maybe Lude.Text)
qtAnswerKey = Lens.lens (answerKey :: QualificationType -> Lude.Maybe Lude.Text) (\s a -> s {answerKey = a} :: QualificationType)
{-# DEPRECATED qtAnswerKey "Use generic-lens or generic-optics with 'answerKey' instead." #-}

-- | The questions for a Qualification test associated with this Qualification type that a user can take to obtain a Qualification of this type. This parameter must be specified if AnswerKey is present. A Qualification type cannot have both a specified Test parameter and an AutoGranted value of true.
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtTest :: Lens.Lens' QualificationType (Lude.Maybe Lude.Text)
qtTest = Lens.lens (test :: QualificationType -> Lude.Maybe Lude.Text) (\s a -> s {test = a} :: QualificationType)
{-# DEPRECATED qtTest "Use generic-lens or generic-optics with 'test' instead." #-}

-- | A unique identifier for the Qualification type. A Qualification type is given a Qualification type ID when you call the CreateQualificationType operation.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtQualificationTypeId :: Lens.Lens' QualificationType (Lude.Maybe Lude.Text)
qtQualificationTypeId = Lens.lens (qualificationTypeId :: QualificationType -> Lude.Maybe Lude.Text) (\s a -> s {qualificationTypeId = a} :: QualificationType)
{-# DEPRECATED qtQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | The name of the Qualification type. The type name is used to identify the type, and to find the type using a Qualification type search.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtName :: Lens.Lens' QualificationType (Lude.Maybe Lude.Text)
qtName = Lens.lens (name :: QualificationType -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: QualificationType)
{-# DEPRECATED qtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | One or more words or phrases that describe theQualification type, separated by commas. The Keywords make the type easier to find using a search.
--
-- /Note:/ Consider using 'keywords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtKeywords :: Lens.Lens' QualificationType (Lude.Maybe Lude.Text)
qtKeywords = Lens.lens (keywords :: QualificationType -> Lude.Maybe Lude.Text) (\s a -> s {keywords = a} :: QualificationType)
{-# DEPRECATED qtKeywords "Use generic-lens or generic-optics with 'keywords' instead." #-}

-- | Specifies that requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test. Valid values are True | False.
--
-- /Note:/ Consider using 'autoGranted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtAutoGranted :: Lens.Lens' QualificationType (Lude.Maybe Lude.Bool)
qtAutoGranted = Lens.lens (autoGranted :: QualificationType -> Lude.Maybe Lude.Bool) (\s a -> s {autoGranted = a} :: QualificationType)
{-# DEPRECATED qtAutoGranted "Use generic-lens or generic-optics with 'autoGranted' instead." #-}

-- | The Qualification integer value to use for automatically granted Qualifications, if AutoGranted is true. This is 1 by default.
--
-- /Note:/ Consider using 'autoGrantedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtAutoGrantedValue :: Lens.Lens' QualificationType (Lude.Maybe Lude.Int)
qtAutoGrantedValue = Lens.lens (autoGrantedValue :: QualificationType -> Lude.Maybe Lude.Int) (\s a -> s {autoGrantedValue = a} :: QualificationType)
{-# DEPRECATED qtAutoGrantedValue "Use generic-lens or generic-optics with 'autoGrantedValue' instead." #-}

-- | A long description for the Qualification type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtDescription :: Lens.Lens' QualificationType (Lude.Maybe Lude.Text)
qtDescription = Lens.lens (description :: QualificationType -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: QualificationType)
{-# DEPRECATED qtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies whether the Qualification type is one that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test. This value is False for Qualifications assigned automatically by the system. Valid values are True | False.
--
-- /Note:/ Consider using 'isRequestable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtIsRequestable :: Lens.Lens' QualificationType (Lude.Maybe Lude.Bool)
qtIsRequestable = Lens.lens (isRequestable :: QualificationType -> Lude.Maybe Lude.Bool) (\s a -> s {isRequestable = a} :: QualificationType)
{-# DEPRECATED qtIsRequestable "Use generic-lens or generic-optics with 'isRequestable' instead." #-}

-- | The amount of time, in seconds, Workers must wait after taking the Qualification test before they can take it again. Workers can take a Qualification test multiple times if they were not granted the Qualification from a previous attempt, or if the test offers a gradient score and they want a better score. If not specified, retries are disabled and Workers can request a Qualification only once.
--
-- /Note:/ Consider using 'retryDelayInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtRetryDelayInSeconds :: Lens.Lens' QualificationType (Lude.Maybe Lude.Integer)
qtRetryDelayInSeconds = Lens.lens (retryDelayInSeconds :: QualificationType -> Lude.Maybe Lude.Integer) (\s a -> s {retryDelayInSeconds = a} :: QualificationType)
{-# DEPRECATED qtRetryDelayInSeconds "Use generic-lens or generic-optics with 'retryDelayInSeconds' instead." #-}

instance Lude.FromJSON QualificationType where
  parseJSON =
    Lude.withObject
      "QualificationType"
      ( \x ->
          QualificationType'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "TestDurationInSeconds")
            Lude.<*> (x Lude..:? "QualificationTypeStatus")
            Lude.<*> (x Lude..:? "AnswerKey")
            Lude.<*> (x Lude..:? "Test")
            Lude.<*> (x Lude..:? "QualificationTypeId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Keywords")
            Lude.<*> (x Lude..:? "AutoGranted")
            Lude.<*> (x Lude..:? "AutoGrantedValue")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "IsRequestable")
            Lude.<*> (x Lude..:? "RetryDelayInSeconds")
      )
