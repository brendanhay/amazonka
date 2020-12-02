{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.Assignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.Assignment where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types.AssignmentStatus
import Network.AWS.Prelude

-- | The Assignment data structure represents a single assignment of a HIT to a Worker. The assignment tracks the Worker's efforts to complete the HIT, and contains the results for later retrieval.
--
--
--
-- /See:/ 'assignment' smart constructor.
data Assignment = Assignment'
  { _aAcceptTime :: !(Maybe POSIX),
    _aAnswer :: !(Maybe Text),
    _aAssignmentStatus :: !(Maybe AssignmentStatus),
    _aRequesterFeedback :: !(Maybe Text),
    _aDeadline :: !(Maybe POSIX),
    _aApprovalTime :: !(Maybe POSIX),
    _aRejectionTime :: !(Maybe POSIX),
    _aAutoApprovalTime :: !(Maybe POSIX),
    _aHITId :: !(Maybe Text),
    _aWorkerId :: !(Maybe Text),
    _aAssignmentId :: !(Maybe Text),
    _aSubmitTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Assignment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAcceptTime' - The date and time the Worker accepted the assignment.
--
-- * 'aAnswer' - The Worker's answers submitted for the HIT contained in a QuestionFormAnswers document, if the Worker provides an answer. If the Worker does not provide any answers, Answer may contain a QuestionFormAnswers document, or Answer may be empty.
--
-- * 'aAssignmentStatus' - The status of the assignment.
--
-- * 'aRequesterFeedback' - The feedback string included with the call to the ApproveAssignment operation or the RejectAssignment operation, if the Requester approved or rejected the assignment and specified feedback.
--
-- * 'aDeadline' - The date and time of the deadline for the assignment. This value is derived from the deadline specification for the HIT and the date and time the Worker accepted the HIT.
--
-- * 'aApprovalTime' - If the Worker has submitted results and the Requester has approved the results, ApprovalTime is the date and time the Requester approved the results. This value is omitted from the assignment if the Requester has not yet approved the results.
--
-- * 'aRejectionTime' - If the Worker has submitted results and the Requester has rejected the results, RejectionTime is the date and time the Requester rejected the results.
--
-- * 'aAutoApprovalTime' - If results have been submitted, AutoApprovalTime is the date and time the results of the assignment results are considered Approved automatically if they have not already been explicitly approved or rejected by the Requester. This value is derived from the auto-approval delay specified by the Requester in the HIT. This value is omitted from the assignment if the Worker has not yet submitted results.
--
-- * 'aHITId' - The ID of the HIT.
--
-- * 'aWorkerId' - The ID of the Worker who accepted the HIT.
--
-- * 'aAssignmentId' - A unique identifier for the assignment.
--
-- * 'aSubmitTime' - If the Worker has submitted results, SubmitTime is the date and time the assignment was submitted. This value is omitted from the assignment if the Worker has not yet submitted results.
assignment ::
  Assignment
assignment =
  Assignment'
    { _aAcceptTime = Nothing,
      _aAnswer = Nothing,
      _aAssignmentStatus = Nothing,
      _aRequesterFeedback = Nothing,
      _aDeadline = Nothing,
      _aApprovalTime = Nothing,
      _aRejectionTime = Nothing,
      _aAutoApprovalTime = Nothing,
      _aHITId = Nothing,
      _aWorkerId = Nothing,
      _aAssignmentId = Nothing,
      _aSubmitTime = Nothing
    }

-- | The date and time the Worker accepted the assignment.
aAcceptTime :: Lens' Assignment (Maybe UTCTime)
aAcceptTime = lens _aAcceptTime (\s a -> s {_aAcceptTime = a}) . mapping _Time

-- | The Worker's answers submitted for the HIT contained in a QuestionFormAnswers document, if the Worker provides an answer. If the Worker does not provide any answers, Answer may contain a QuestionFormAnswers document, or Answer may be empty.
aAnswer :: Lens' Assignment (Maybe Text)
aAnswer = lens _aAnswer (\s a -> s {_aAnswer = a})

-- | The status of the assignment.
aAssignmentStatus :: Lens' Assignment (Maybe AssignmentStatus)
aAssignmentStatus = lens _aAssignmentStatus (\s a -> s {_aAssignmentStatus = a})

-- | The feedback string included with the call to the ApproveAssignment operation or the RejectAssignment operation, if the Requester approved or rejected the assignment and specified feedback.
aRequesterFeedback :: Lens' Assignment (Maybe Text)
aRequesterFeedback = lens _aRequesterFeedback (\s a -> s {_aRequesterFeedback = a})

-- | The date and time of the deadline for the assignment. This value is derived from the deadline specification for the HIT and the date and time the Worker accepted the HIT.
aDeadline :: Lens' Assignment (Maybe UTCTime)
aDeadline = lens _aDeadline (\s a -> s {_aDeadline = a}) . mapping _Time

-- | If the Worker has submitted results and the Requester has approved the results, ApprovalTime is the date and time the Requester approved the results. This value is omitted from the assignment if the Requester has not yet approved the results.
aApprovalTime :: Lens' Assignment (Maybe UTCTime)
aApprovalTime = lens _aApprovalTime (\s a -> s {_aApprovalTime = a}) . mapping _Time

-- | If the Worker has submitted results and the Requester has rejected the results, RejectionTime is the date and time the Requester rejected the results.
aRejectionTime :: Lens' Assignment (Maybe UTCTime)
aRejectionTime = lens _aRejectionTime (\s a -> s {_aRejectionTime = a}) . mapping _Time

-- | If results have been submitted, AutoApprovalTime is the date and time the results of the assignment results are considered Approved automatically if they have not already been explicitly approved or rejected by the Requester. This value is derived from the auto-approval delay specified by the Requester in the HIT. This value is omitted from the assignment if the Worker has not yet submitted results.
aAutoApprovalTime :: Lens' Assignment (Maybe UTCTime)
aAutoApprovalTime = lens _aAutoApprovalTime (\s a -> s {_aAutoApprovalTime = a}) . mapping _Time

-- | The ID of the HIT.
aHITId :: Lens' Assignment (Maybe Text)
aHITId = lens _aHITId (\s a -> s {_aHITId = a})

-- | The ID of the Worker who accepted the HIT.
aWorkerId :: Lens' Assignment (Maybe Text)
aWorkerId = lens _aWorkerId (\s a -> s {_aWorkerId = a})

-- | A unique identifier for the assignment.
aAssignmentId :: Lens' Assignment (Maybe Text)
aAssignmentId = lens _aAssignmentId (\s a -> s {_aAssignmentId = a})

-- | If the Worker has submitted results, SubmitTime is the date and time the assignment was submitted. This value is omitted from the assignment if the Worker has not yet submitted results.
aSubmitTime :: Lens' Assignment (Maybe UTCTime)
aSubmitTime = lens _aSubmitTime (\s a -> s {_aSubmitTime = a}) . mapping _Time

instance FromJSON Assignment where
  parseJSON =
    withObject
      "Assignment"
      ( \x ->
          Assignment'
            <$> (x .:? "AcceptTime")
            <*> (x .:? "Answer")
            <*> (x .:? "AssignmentStatus")
            <*> (x .:? "RequesterFeedback")
            <*> (x .:? "Deadline")
            <*> (x .:? "ApprovalTime")
            <*> (x .:? "RejectionTime")
            <*> (x .:? "AutoApprovalTime")
            <*> (x .:? "HITId")
            <*> (x .:? "WorkerId")
            <*> (x .:? "AssignmentId")
            <*> (x .:? "SubmitTime")
      )

instance Hashable Assignment

instance NFData Assignment
