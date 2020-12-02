{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.QualificationRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The QualificationRequest data structure represents a request a Worker has made for a Qualification.
--
--
--
-- /See:/ 'qualificationRequest' smart constructor.
data QualificationRequest = QualificationRequest'
  { _quaQualificationRequestId ::
      !(Maybe Text),
    _quaTest :: !(Maybe Text),
    _quaQualificationTypeId :: !(Maybe Text),
    _quaAnswer :: !(Maybe Text),
    _quaWorkerId :: !(Maybe Text),
    _quaSubmitTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QualificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'quaQualificationRequestId' - The ID of the Qualification request, a unique identifier generated when the request was submitted.
--
-- * 'quaTest' - The contents of the Qualification test that was presented to the Worker, if the type has a test and the Worker has submitted answers. This value is identical to the QuestionForm associated with the Qualification type at the time the Worker requests the Qualification.
--
-- * 'quaQualificationTypeId' - The ID of the Qualification type the Worker is requesting, as returned by the CreateQualificationType operation.
--
-- * 'quaAnswer' - The Worker's answers for the Qualification type's test contained in a QuestionFormAnswers document, if the type has a test and the Worker has submitted answers. If the Worker does not provide any answers, Answer may be empty.
--
-- * 'quaWorkerId' - The ID of the Worker requesting the Qualification.
--
-- * 'quaSubmitTime' - The date and time the Qualification request had a status of Submitted. This is either the time the Worker submitted answers for a Qualification test, or the time the Worker requested the Qualification if the Qualification type does not have a test.
qualificationRequest ::
  QualificationRequest
qualificationRequest =
  QualificationRequest'
    { _quaQualificationRequestId = Nothing,
      _quaTest = Nothing,
      _quaQualificationTypeId = Nothing,
      _quaAnswer = Nothing,
      _quaWorkerId = Nothing,
      _quaSubmitTime = Nothing
    }

-- | The ID of the Qualification request, a unique identifier generated when the request was submitted.
quaQualificationRequestId :: Lens' QualificationRequest (Maybe Text)
quaQualificationRequestId = lens _quaQualificationRequestId (\s a -> s {_quaQualificationRequestId = a})

-- | The contents of the Qualification test that was presented to the Worker, if the type has a test and the Worker has submitted answers. This value is identical to the QuestionForm associated with the Qualification type at the time the Worker requests the Qualification.
quaTest :: Lens' QualificationRequest (Maybe Text)
quaTest = lens _quaTest (\s a -> s {_quaTest = a})

-- | The ID of the Qualification type the Worker is requesting, as returned by the CreateQualificationType operation.
quaQualificationTypeId :: Lens' QualificationRequest (Maybe Text)
quaQualificationTypeId = lens _quaQualificationTypeId (\s a -> s {_quaQualificationTypeId = a})

-- | The Worker's answers for the Qualification type's test contained in a QuestionFormAnswers document, if the type has a test and the Worker has submitted answers. If the Worker does not provide any answers, Answer may be empty.
quaAnswer :: Lens' QualificationRequest (Maybe Text)
quaAnswer = lens _quaAnswer (\s a -> s {_quaAnswer = a})

-- | The ID of the Worker requesting the Qualification.
quaWorkerId :: Lens' QualificationRequest (Maybe Text)
quaWorkerId = lens _quaWorkerId (\s a -> s {_quaWorkerId = a})

-- | The date and time the Qualification request had a status of Submitted. This is either the time the Worker submitted answers for a Qualification test, or the time the Worker requested the Qualification if the Qualification type does not have a test.
quaSubmitTime :: Lens' QualificationRequest (Maybe UTCTime)
quaSubmitTime = lens _quaSubmitTime (\s a -> s {_quaSubmitTime = a}) . mapping _Time

instance FromJSON QualificationRequest where
  parseJSON =
    withObject
      "QualificationRequest"
      ( \x ->
          QualificationRequest'
            <$> (x .:? "QualificationRequestId")
            <*> (x .:? "Test")
            <*> (x .:? "QualificationTypeId")
            <*> (x .:? "Answer")
            <*> (x .:? "WorkerId")
            <*> (x .:? "SubmitTime")
      )

instance Hashable QualificationRequest

instance NFData QualificationRequest
