{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.QualificationType where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types.QualificationTypeStatus
import Network.AWS.Prelude

-- | The QualificationType data structure represents a Qualification type, a description of a property of a Worker that must match the requirements of a HIT for the Worker to be able to accept the HIT. The type also describes how a Worker can obtain a Qualification of that type, such as through a Qualification test.
--
--
--
-- /See:/ 'qualificationType' smart constructor.
data QualificationType = QualificationType'
  { _qtCreationTime ::
      !(Maybe POSIX),
    _qtTestDurationInSeconds :: !(Maybe Integer),
    _qtQualificationTypeStatus ::
      !(Maybe QualificationTypeStatus),
    _qtAnswerKey :: !(Maybe Text),
    _qtTest :: !(Maybe Text),
    _qtQualificationTypeId :: !(Maybe Text),
    _qtName :: !(Maybe Text),
    _qtKeywords :: !(Maybe Text),
    _qtAutoGranted :: !(Maybe Bool),
    _qtAutoGrantedValue :: !(Maybe Int),
    _qtDescription :: !(Maybe Text),
    _qtIsRequestable :: !(Maybe Bool),
    _qtRetryDelayInSeconds :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QualificationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qtCreationTime' - The date and time the Qualification type was created.
--
-- * 'qtTestDurationInSeconds' - The amount of time, in seconds, given to a Worker to complete the Qualification test, beginning from the time the Worker requests the Qualification.
--
-- * 'qtQualificationTypeStatus' - The status of the Qualification type. A Qualification type's status determines if users can apply to receive a Qualification of this type, and if HITs can be created with requirements based on this type. Valid values are Active | Inactive.
--
-- * 'qtAnswerKey' - The answers to the Qualification test specified in the Test parameter.
--
-- * 'qtTest' - The questions for a Qualification test associated with this Qualification type that a user can take to obtain a Qualification of this type. This parameter must be specified if AnswerKey is present. A Qualification type cannot have both a specified Test parameter and an AutoGranted value of true.
--
-- * 'qtQualificationTypeId' - A unique identifier for the Qualification type. A Qualification type is given a Qualification type ID when you call the CreateQualificationType operation.
--
-- * 'qtName' - The name of the Qualification type. The type name is used to identify the type, and to find the type using a Qualification type search.
--
-- * 'qtKeywords' - One or more words or phrases that describe theQualification type, separated by commas. The Keywords make the type easier to find using a search.
--
-- * 'qtAutoGranted' - Specifies that requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test. Valid values are True | False.
--
-- * 'qtAutoGrantedValue' - The Qualification integer value to use for automatically granted Qualifications, if AutoGranted is true. This is 1 by default.
--
-- * 'qtDescription' - A long description for the Qualification type.
--
-- * 'qtIsRequestable' - Specifies whether the Qualification type is one that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test. This value is False for Qualifications assigned automatically by the system. Valid values are True | False.
--
-- * 'qtRetryDelayInSeconds' - The amount of time, in seconds, Workers must wait after taking the Qualification test before they can take it again. Workers can take a Qualification test multiple times if they were not granted the Qualification from a previous attempt, or if the test offers a gradient score and they want a better score. If not specified, retries are disabled and Workers can request a Qualification only once.
qualificationType ::
  QualificationType
qualificationType =
  QualificationType'
    { _qtCreationTime = Nothing,
      _qtTestDurationInSeconds = Nothing,
      _qtQualificationTypeStatus = Nothing,
      _qtAnswerKey = Nothing,
      _qtTest = Nothing,
      _qtQualificationTypeId = Nothing,
      _qtName = Nothing,
      _qtKeywords = Nothing,
      _qtAutoGranted = Nothing,
      _qtAutoGrantedValue = Nothing,
      _qtDescription = Nothing,
      _qtIsRequestable = Nothing,
      _qtRetryDelayInSeconds = Nothing
    }

-- | The date and time the Qualification type was created.
qtCreationTime :: Lens' QualificationType (Maybe UTCTime)
qtCreationTime = lens _qtCreationTime (\s a -> s {_qtCreationTime = a}) . mapping _Time

-- | The amount of time, in seconds, given to a Worker to complete the Qualification test, beginning from the time the Worker requests the Qualification.
qtTestDurationInSeconds :: Lens' QualificationType (Maybe Integer)
qtTestDurationInSeconds = lens _qtTestDurationInSeconds (\s a -> s {_qtTestDurationInSeconds = a})

-- | The status of the Qualification type. A Qualification type's status determines if users can apply to receive a Qualification of this type, and if HITs can be created with requirements based on this type. Valid values are Active | Inactive.
qtQualificationTypeStatus :: Lens' QualificationType (Maybe QualificationTypeStatus)
qtQualificationTypeStatus = lens _qtQualificationTypeStatus (\s a -> s {_qtQualificationTypeStatus = a})

-- | The answers to the Qualification test specified in the Test parameter.
qtAnswerKey :: Lens' QualificationType (Maybe Text)
qtAnswerKey = lens _qtAnswerKey (\s a -> s {_qtAnswerKey = a})

-- | The questions for a Qualification test associated with this Qualification type that a user can take to obtain a Qualification of this type. This parameter must be specified if AnswerKey is present. A Qualification type cannot have both a specified Test parameter and an AutoGranted value of true.
qtTest :: Lens' QualificationType (Maybe Text)
qtTest = lens _qtTest (\s a -> s {_qtTest = a})

-- | A unique identifier for the Qualification type. A Qualification type is given a Qualification type ID when you call the CreateQualificationType operation.
qtQualificationTypeId :: Lens' QualificationType (Maybe Text)
qtQualificationTypeId = lens _qtQualificationTypeId (\s a -> s {_qtQualificationTypeId = a})

-- | The name of the Qualification type. The type name is used to identify the type, and to find the type using a Qualification type search.
qtName :: Lens' QualificationType (Maybe Text)
qtName = lens _qtName (\s a -> s {_qtName = a})

-- | One or more words or phrases that describe theQualification type, separated by commas. The Keywords make the type easier to find using a search.
qtKeywords :: Lens' QualificationType (Maybe Text)
qtKeywords = lens _qtKeywords (\s a -> s {_qtKeywords = a})

-- | Specifies that requests for the Qualification type are granted immediately, without prompting the Worker with a Qualification test. Valid values are True | False.
qtAutoGranted :: Lens' QualificationType (Maybe Bool)
qtAutoGranted = lens _qtAutoGranted (\s a -> s {_qtAutoGranted = a})

-- | The Qualification integer value to use for automatically granted Qualifications, if AutoGranted is true. This is 1 by default.
qtAutoGrantedValue :: Lens' QualificationType (Maybe Int)
qtAutoGrantedValue = lens _qtAutoGrantedValue (\s a -> s {_qtAutoGrantedValue = a})

-- | A long description for the Qualification type.
qtDescription :: Lens' QualificationType (Maybe Text)
qtDescription = lens _qtDescription (\s a -> s {_qtDescription = a})

-- | Specifies whether the Qualification type is one that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test. This value is False for Qualifications assigned automatically by the system. Valid values are True | False.
qtIsRequestable :: Lens' QualificationType (Maybe Bool)
qtIsRequestable = lens _qtIsRequestable (\s a -> s {_qtIsRequestable = a})

-- | The amount of time, in seconds, Workers must wait after taking the Qualification test before they can take it again. Workers can take a Qualification test multiple times if they were not granted the Qualification from a previous attempt, or if the test offers a gradient score and they want a better score. If not specified, retries are disabled and Workers can request a Qualification only once.
qtRetryDelayInSeconds :: Lens' QualificationType (Maybe Integer)
qtRetryDelayInSeconds = lens _qtRetryDelayInSeconds (\s a -> s {_qtRetryDelayInSeconds = a})

instance FromJSON QualificationType where
  parseJSON =
    withObject
      "QualificationType"
      ( \x ->
          QualificationType'
            <$> (x .:? "CreationTime")
            <*> (x .:? "TestDurationInSeconds")
            <*> (x .:? "QualificationTypeStatus")
            <*> (x .:? "AnswerKey")
            <*> (x .:? "Test")
            <*> (x .:? "QualificationTypeId")
            <*> (x .:? "Name")
            <*> (x .:? "Keywords")
            <*> (x .:? "AutoGranted")
            <*> (x .:? "AutoGrantedValue")
            <*> (x .:? "Description")
            <*> (x .:? "IsRequestable")
            <*> (x .:? "RetryDelayInSeconds")
      )

instance Hashable QualificationType

instance NFData QualificationType
