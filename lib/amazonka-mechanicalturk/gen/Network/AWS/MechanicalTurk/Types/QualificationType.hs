{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.QualificationType
  ( QualificationType (..)
  -- * Smart constructor
  , mkQualificationType
  -- * Lenses
  , qtAnswerKey
  , qtAutoGranted
  , qtAutoGrantedValue
  , qtCreationTime
  , qtDescription
  , qtIsRequestable
  , qtKeywords
  , qtName
  , qtQualificationTypeId
  , qtQualificationTypeStatus
  , qtRetryDelayInSeconds
  , qtTest
  , qtTestDurationInSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.QualificationTypeId as Types
import qualified Network.AWS.MechanicalTurk.Types.QualificationTypeStatus as Types
import qualified Network.AWS.Prelude as Core

-- | The QualificationType data structure represents a Qualification type, a description of a property of a Worker that must match the requirements of a HIT for the Worker to be able to accept the HIT. The type also describes how a Worker can obtain a Qualification of that type, such as through a Qualification test. 
--
-- /See:/ 'mkQualificationType' smart constructor.
data QualificationType = QualificationType'
  { answerKey :: Core.Maybe Core.Text
    -- ^ The answers to the Qualification test specified in the Test parameter.
  , autoGranted :: Core.Maybe Core.Bool
    -- ^ Specifies that requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test. Valid values are True | False.
  , autoGrantedValue :: Core.Maybe Core.Int
    -- ^ The Qualification integer value to use for automatically granted Qualifications, if AutoGranted is true. This is 1 by default. 
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the Qualification type was created. 
  , description :: Core.Maybe Core.Text
    -- ^ A long description for the Qualification type. 
  , isRequestable :: Core.Maybe Core.Bool
    -- ^ Specifies whether the Qualification type is one that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test. This value is False for Qualifications assigned automatically by the system. Valid values are True | False. 
  , keywords :: Core.Maybe Core.Text
    -- ^ One or more words or phrases that describe theQualification type, separated by commas. The Keywords make the type easier to find using a search. 
  , name :: Core.Maybe Core.Text
    -- ^ The name of the Qualification type. The type name is used to identify the type, and to find the type using a Qualification type search. 
  , qualificationTypeId :: Core.Maybe Types.QualificationTypeId
    -- ^ A unique identifier for the Qualification type. A Qualification type is given a Qualification type ID when you call the CreateQualificationType operation. 
  , qualificationTypeStatus :: Core.Maybe Types.QualificationTypeStatus
    -- ^ The status of the Qualification type. A Qualification type's status determines if users can apply to receive a Qualification of this type, and if HITs can be created with requirements based on this type. Valid values are Active | Inactive. 
  , retryDelayInSeconds :: Core.Maybe Core.Integer
    -- ^ The amount of time, in seconds, Workers must wait after taking the Qualification test before they can take it again. Workers can take a Qualification test multiple times if they were not granted the Qualification from a previous attempt, or if the test offers a gradient score and they want a better score. If not specified, retries are disabled and Workers can request a Qualification only once. 
  , test :: Core.Maybe Core.Text
    -- ^ The questions for a Qualification test associated with this Qualification type that a user can take to obtain a Qualification of this type. This parameter must be specified if AnswerKey is present. A Qualification type cannot have both a specified Test parameter and an AutoGranted value of true. 
  , testDurationInSeconds :: Core.Maybe Core.Integer
    -- ^ The amount of time, in seconds, given to a Worker to complete the Qualification test, beginning from the time the Worker requests the Qualification. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'QualificationType' value with any optional fields omitted.
mkQualificationType
    :: QualificationType
mkQualificationType
  = QualificationType'{answerKey = Core.Nothing,
                       autoGranted = Core.Nothing, autoGrantedValue = Core.Nothing,
                       creationTime = Core.Nothing, description = Core.Nothing,
                       isRequestable = Core.Nothing, keywords = Core.Nothing,
                       name = Core.Nothing, qualificationTypeId = Core.Nothing,
                       qualificationTypeStatus = Core.Nothing,
                       retryDelayInSeconds = Core.Nothing, test = Core.Nothing,
                       testDurationInSeconds = Core.Nothing}

-- | The answers to the Qualification test specified in the Test parameter.
--
-- /Note:/ Consider using 'answerKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtAnswerKey :: Lens.Lens' QualificationType (Core.Maybe Core.Text)
qtAnswerKey = Lens.field @"answerKey"
{-# INLINEABLE qtAnswerKey #-}
{-# DEPRECATED answerKey "Use generic-lens or generic-optics with 'answerKey' instead"  #-}

-- | Specifies that requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test. Valid values are True | False.
--
-- /Note:/ Consider using 'autoGranted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtAutoGranted :: Lens.Lens' QualificationType (Core.Maybe Core.Bool)
qtAutoGranted = Lens.field @"autoGranted"
{-# INLINEABLE qtAutoGranted #-}
{-# DEPRECATED autoGranted "Use generic-lens or generic-optics with 'autoGranted' instead"  #-}

-- | The Qualification integer value to use for automatically granted Qualifications, if AutoGranted is true. This is 1 by default. 
--
-- /Note:/ Consider using 'autoGrantedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtAutoGrantedValue :: Lens.Lens' QualificationType (Core.Maybe Core.Int)
qtAutoGrantedValue = Lens.field @"autoGrantedValue"
{-# INLINEABLE qtAutoGrantedValue #-}
{-# DEPRECATED autoGrantedValue "Use generic-lens or generic-optics with 'autoGrantedValue' instead"  #-}

-- | The date and time the Qualification type was created. 
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtCreationTime :: Lens.Lens' QualificationType (Core.Maybe Core.NominalDiffTime)
qtCreationTime = Lens.field @"creationTime"
{-# INLINEABLE qtCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | A long description for the Qualification type. 
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtDescription :: Lens.Lens' QualificationType (Core.Maybe Core.Text)
qtDescription = Lens.field @"description"
{-# INLINEABLE qtDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Specifies whether the Qualification type is one that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test. This value is False for Qualifications assigned automatically by the system. Valid values are True | False. 
--
-- /Note:/ Consider using 'isRequestable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtIsRequestable :: Lens.Lens' QualificationType (Core.Maybe Core.Bool)
qtIsRequestable = Lens.field @"isRequestable"
{-# INLINEABLE qtIsRequestable #-}
{-# DEPRECATED isRequestable "Use generic-lens or generic-optics with 'isRequestable' instead"  #-}

-- | One or more words or phrases that describe theQualification type, separated by commas. The Keywords make the type easier to find using a search. 
--
-- /Note:/ Consider using 'keywords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtKeywords :: Lens.Lens' QualificationType (Core.Maybe Core.Text)
qtKeywords = Lens.field @"keywords"
{-# INLINEABLE qtKeywords #-}
{-# DEPRECATED keywords "Use generic-lens or generic-optics with 'keywords' instead"  #-}

-- | The name of the Qualification type. The type name is used to identify the type, and to find the type using a Qualification type search. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtName :: Lens.Lens' QualificationType (Core.Maybe Core.Text)
qtName = Lens.field @"name"
{-# INLINEABLE qtName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A unique identifier for the Qualification type. A Qualification type is given a Qualification type ID when you call the CreateQualificationType operation. 
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtQualificationTypeId :: Lens.Lens' QualificationType (Core.Maybe Types.QualificationTypeId)
qtQualificationTypeId = Lens.field @"qualificationTypeId"
{-# INLINEABLE qtQualificationTypeId #-}
{-# DEPRECATED qualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead"  #-}

-- | The status of the Qualification type. A Qualification type's status determines if users can apply to receive a Qualification of this type, and if HITs can be created with requirements based on this type. Valid values are Active | Inactive. 
--
-- /Note:/ Consider using 'qualificationTypeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtQualificationTypeStatus :: Lens.Lens' QualificationType (Core.Maybe Types.QualificationTypeStatus)
qtQualificationTypeStatus = Lens.field @"qualificationTypeStatus"
{-# INLINEABLE qtQualificationTypeStatus #-}
{-# DEPRECATED qualificationTypeStatus "Use generic-lens or generic-optics with 'qualificationTypeStatus' instead"  #-}

-- | The amount of time, in seconds, Workers must wait after taking the Qualification test before they can take it again. Workers can take a Qualification test multiple times if they were not granted the Qualification from a previous attempt, or if the test offers a gradient score and they want a better score. If not specified, retries are disabled and Workers can request a Qualification only once. 
--
-- /Note:/ Consider using 'retryDelayInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtRetryDelayInSeconds :: Lens.Lens' QualificationType (Core.Maybe Core.Integer)
qtRetryDelayInSeconds = Lens.field @"retryDelayInSeconds"
{-# INLINEABLE qtRetryDelayInSeconds #-}
{-# DEPRECATED retryDelayInSeconds "Use generic-lens or generic-optics with 'retryDelayInSeconds' instead"  #-}

-- | The questions for a Qualification test associated with this Qualification type that a user can take to obtain a Qualification of this type. This parameter must be specified if AnswerKey is present. A Qualification type cannot have both a specified Test parameter and an AutoGranted value of true. 
--
-- /Note:/ Consider using 'test' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtTest :: Lens.Lens' QualificationType (Core.Maybe Core.Text)
qtTest = Lens.field @"test"
{-# INLINEABLE qtTest #-}
{-# DEPRECATED test "Use generic-lens or generic-optics with 'test' instead"  #-}

-- | The amount of time, in seconds, given to a Worker to complete the Qualification test, beginning from the time the Worker requests the Qualification. 
--
-- /Note:/ Consider using 'testDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtTestDurationInSeconds :: Lens.Lens' QualificationType (Core.Maybe Core.Integer)
qtTestDurationInSeconds = Lens.field @"testDurationInSeconds"
{-# INLINEABLE qtTestDurationInSeconds #-}
{-# DEPRECATED testDurationInSeconds "Use generic-lens or generic-optics with 'testDurationInSeconds' instead"  #-}

instance Core.FromJSON QualificationType where
        parseJSON
          = Core.withObject "QualificationType" Core.$
              \ x ->
                QualificationType' Core.<$>
                  (x Core..:? "AnswerKey") Core.<*> x Core..:? "AutoGranted" Core.<*>
                    x Core..:? "AutoGrantedValue"
                    Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "IsRequestable"
                    Core.<*> x Core..:? "Keywords"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "QualificationTypeId"
                    Core.<*> x Core..:? "QualificationTypeStatus"
                    Core.<*> x Core..:? "RetryDelayInSeconds"
                    Core.<*> x Core..:? "Test"
                    Core.<*> x Core..:? "TestDurationInSeconds"
