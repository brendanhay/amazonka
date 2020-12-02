{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.CreateHIT
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateHIT@ operation creates a new Human Intelligence Task (HIT). The new HIT is made available for Workers to find and accept on the Amazon Mechanical Turk website.
--
--
-- This operation allows you to specify a new HIT by passing in values for the properties of the HIT, such as its title, reward amount and number of assignments. When you pass these values to @CreateHIT@ , a new HIT is created for you, with a new @HITTypeID@ . The HITTypeID can be used to create additional HITs in the future without needing to specify common parameters such as the title, description and reward amount each time.
--
-- An alternative way to create HITs is to first generate a HITTypeID using the @CreateHITType@ operation and then call the @CreateHITWithHITType@ operation. This is the recommended best practice for Requesters who are creating large numbers of HITs.
--
-- CreateHIT also supports several ways to provide question data: by providing a value for the @Question@ parameter that fully specifies the contents of the HIT, or by providing a @HitLayoutId@ and associated @HitLayoutParameters@ .
--
module Network.AWS.MechanicalTurk.CreateHIT
    (
    -- * Creating a Request
      createHIT
    , CreateHIT
    -- * Request Lenses
    , chitHITReviewPolicy
    , chitUniqueRequestToken
    , chitAutoApprovalDelayInSeconds
    , chitRequesterAnnotation
    , chitMaxAssignments
    , chitKeywords
    , chitHITLayoutId
    , chitHITLayoutParameters
    , chitQualificationRequirements
    , chitQuestion
    , chitAssignmentReviewPolicy
    , chitLifetimeInSeconds
    , chitAssignmentDurationInSeconds
    , chitReward
    , chitTitle
    , chitDescription

    -- * Destructuring the Response
    , createHITResponse
    , CreateHITResponse
    -- * Response Lenses
    , chitrsHIT
    , chitrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createHIT' smart constructor.
data CreateHIT = CreateHIT'
  { _chitHITReviewPolicy             :: !(Maybe ReviewPolicy)
  , _chitUniqueRequestToken          :: !(Maybe Text)
  , _chitAutoApprovalDelayInSeconds  :: !(Maybe Integer)
  , _chitRequesterAnnotation         :: !(Maybe Text)
  , _chitMaxAssignments              :: !(Maybe Int)
  , _chitKeywords                    :: !(Maybe Text)
  , _chitHITLayoutId                 :: !(Maybe Text)
  , _chitHITLayoutParameters         :: !(Maybe [HITLayoutParameter])
  , _chitQualificationRequirements   :: !(Maybe [QualificationRequirement])
  , _chitQuestion                    :: !(Maybe Text)
  , _chitAssignmentReviewPolicy      :: !(Maybe ReviewPolicy)
  , _chitLifetimeInSeconds           :: !Integer
  , _chitAssignmentDurationInSeconds :: !Integer
  , _chitReward                      :: !Text
  , _chitTitle                       :: !Text
  , _chitDescription                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHIT' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chitHITReviewPolicy' - The HIT-level Review Policy applies to the HIT. You can specify for Mechanical Turk to take various actions based on the policy.
--
-- * 'chitUniqueRequestToken' - A unique identifier for this request which allows you to retry the call on error without creating duplicate HITs. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the HIT already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return a AWS.MechanicalTurk.HitAlreadyExists error with a message containing the HITId.
--
-- * 'chitAutoApprovalDelayInSeconds' - The number of seconds after an assignment for the HIT has been submitted, after which the assignment is considered Approved automatically unless the Requester explicitly rejects it.
--
-- * 'chitRequesterAnnotation' - An arbitrary data field. The RequesterAnnotation parameter lets your application attach arbitrary data to the HIT for tracking purposes. For example, this parameter could be an identifier internal to the Requester's application that corresponds with the HIT.  The RequesterAnnotation parameter for a HIT is only visible to the Requester who created the HIT. It is not shown to the Worker, or any other Requester.  The RequesterAnnotation parameter may be different for each HIT you submit. It does not affect how your HITs are grouped.
--
-- * 'chitMaxAssignments' - The number of times the HIT can be accepted and completed before the HIT becomes unavailable.
--
-- * 'chitKeywords' - One or more words or phrases that describe the HIT, separated by commas. These words are used in searches to find HITs.
--
-- * 'chitHITLayoutId' - The HITLayoutId allows you to use a pre-existing HIT design with placeholder values and create an additional HIT by providing those values as HITLayoutParameters.  Constraints: Either a Question parameter or a HITLayoutId parameter must be provided.
--
-- * 'chitHITLayoutParameters' - If the HITLayoutId is provided, any placeholder values must be filled in with values using the HITLayoutParameter structure. For more information, see HITLayout.
--
-- * 'chitQualificationRequirements' - Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
--
-- * 'chitQuestion' - The data the person completing the HIT uses to produce the results.  Constraints: Must be a QuestionForm data structure, an ExternalQuestion data structure, or an HTMLQuestion data structure. The XML question data must not be larger than 64 kilobytes (65,535 bytes) in size, including whitespace.  Either a Question parameter or a HITLayoutId parameter must be provided.
--
-- * 'chitAssignmentReviewPolicy' - The Assignment-level Review Policy applies to the assignments under the HIT. You can specify for Mechanical Turk to take various actions based on the policy.
--
-- * 'chitLifetimeInSeconds' - An amount of time, in seconds, after which the HIT is no longer available for users to accept. After the lifetime of the HIT elapses, the HIT no longer appears in HIT searches, even if not all of the assignments for the HIT have been accepted.
--
-- * 'chitAssignmentDurationInSeconds' - The amount of time, in seconds, that a Worker has to complete the HIT after accepting it. If a Worker does not complete the assignment within the specified duration, the assignment is considered abandoned. If the HIT is still active (that is, its lifetime has not elapsed), the assignment becomes available for other users to find and accept.
--
-- * 'chitReward' - The amount of money the Requester will pay a Worker for successfully completing the HIT.
--
-- * 'chitTitle' - The title of the HIT. A title should be short and descriptive about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT title appears in search results, and everywhere the HIT is mentioned.
--
-- * 'chitDescription' - A general description of the HIT. A description includes detailed information about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT description appears in the expanded view of search results, and in the HIT and assignment screens. A good description gives the user enough information to evaluate the HIT before accepting it.
createHIT
    :: Integer -- ^ 'chitLifetimeInSeconds'
    -> Integer -- ^ 'chitAssignmentDurationInSeconds'
    -> Text -- ^ 'chitReward'
    -> Text -- ^ 'chitTitle'
    -> Text -- ^ 'chitDescription'
    -> CreateHIT
createHIT pLifetimeInSeconds_ pAssignmentDurationInSeconds_ pReward_ pTitle_ pDescription_ =
  CreateHIT'
    { _chitHITReviewPolicy = Nothing
    , _chitUniqueRequestToken = Nothing
    , _chitAutoApprovalDelayInSeconds = Nothing
    , _chitRequesterAnnotation = Nothing
    , _chitMaxAssignments = Nothing
    , _chitKeywords = Nothing
    , _chitHITLayoutId = Nothing
    , _chitHITLayoutParameters = Nothing
    , _chitQualificationRequirements = Nothing
    , _chitQuestion = Nothing
    , _chitAssignmentReviewPolicy = Nothing
    , _chitLifetimeInSeconds = pLifetimeInSeconds_
    , _chitAssignmentDurationInSeconds = pAssignmentDurationInSeconds_
    , _chitReward = pReward_
    , _chitTitle = pTitle_
    , _chitDescription = pDescription_
    }


-- | The HIT-level Review Policy applies to the HIT. You can specify for Mechanical Turk to take various actions based on the policy.
chitHITReviewPolicy :: Lens' CreateHIT (Maybe ReviewPolicy)
chitHITReviewPolicy = lens _chitHITReviewPolicy (\ s a -> s{_chitHITReviewPolicy = a})

-- | A unique identifier for this request which allows you to retry the call on error without creating duplicate HITs. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the HIT already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return a AWS.MechanicalTurk.HitAlreadyExists error with a message containing the HITId.
chitUniqueRequestToken :: Lens' CreateHIT (Maybe Text)
chitUniqueRequestToken = lens _chitUniqueRequestToken (\ s a -> s{_chitUniqueRequestToken = a})

-- | The number of seconds after an assignment for the HIT has been submitted, after which the assignment is considered Approved automatically unless the Requester explicitly rejects it.
chitAutoApprovalDelayInSeconds :: Lens' CreateHIT (Maybe Integer)
chitAutoApprovalDelayInSeconds = lens _chitAutoApprovalDelayInSeconds (\ s a -> s{_chitAutoApprovalDelayInSeconds = a})

-- | An arbitrary data field. The RequesterAnnotation parameter lets your application attach arbitrary data to the HIT for tracking purposes. For example, this parameter could be an identifier internal to the Requester's application that corresponds with the HIT.  The RequesterAnnotation parameter for a HIT is only visible to the Requester who created the HIT. It is not shown to the Worker, or any other Requester.  The RequesterAnnotation parameter may be different for each HIT you submit. It does not affect how your HITs are grouped.
chitRequesterAnnotation :: Lens' CreateHIT (Maybe Text)
chitRequesterAnnotation = lens _chitRequesterAnnotation (\ s a -> s{_chitRequesterAnnotation = a})

-- | The number of times the HIT can be accepted and completed before the HIT becomes unavailable.
chitMaxAssignments :: Lens' CreateHIT (Maybe Int)
chitMaxAssignments = lens _chitMaxAssignments (\ s a -> s{_chitMaxAssignments = a})

-- | One or more words or phrases that describe the HIT, separated by commas. These words are used in searches to find HITs.
chitKeywords :: Lens' CreateHIT (Maybe Text)
chitKeywords = lens _chitKeywords (\ s a -> s{_chitKeywords = a})

-- | The HITLayoutId allows you to use a pre-existing HIT design with placeholder values and create an additional HIT by providing those values as HITLayoutParameters.  Constraints: Either a Question parameter or a HITLayoutId parameter must be provided.
chitHITLayoutId :: Lens' CreateHIT (Maybe Text)
chitHITLayoutId = lens _chitHITLayoutId (\ s a -> s{_chitHITLayoutId = a})

-- | If the HITLayoutId is provided, any placeholder values must be filled in with values using the HITLayoutParameter structure. For more information, see HITLayout.
chitHITLayoutParameters :: Lens' CreateHIT [HITLayoutParameter]
chitHITLayoutParameters = lens _chitHITLayoutParameters (\ s a -> s{_chitHITLayoutParameters = a}) . _Default . _Coerce

-- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
chitQualificationRequirements :: Lens' CreateHIT [QualificationRequirement]
chitQualificationRequirements = lens _chitQualificationRequirements (\ s a -> s{_chitQualificationRequirements = a}) . _Default . _Coerce

-- | The data the person completing the HIT uses to produce the results.  Constraints: Must be a QuestionForm data structure, an ExternalQuestion data structure, or an HTMLQuestion data structure. The XML question data must not be larger than 64 kilobytes (65,535 bytes) in size, including whitespace.  Either a Question parameter or a HITLayoutId parameter must be provided.
chitQuestion :: Lens' CreateHIT (Maybe Text)
chitQuestion = lens _chitQuestion (\ s a -> s{_chitQuestion = a})

-- | The Assignment-level Review Policy applies to the assignments under the HIT. You can specify for Mechanical Turk to take various actions based on the policy.
chitAssignmentReviewPolicy :: Lens' CreateHIT (Maybe ReviewPolicy)
chitAssignmentReviewPolicy = lens _chitAssignmentReviewPolicy (\ s a -> s{_chitAssignmentReviewPolicy = a})

-- | An amount of time, in seconds, after which the HIT is no longer available for users to accept. After the lifetime of the HIT elapses, the HIT no longer appears in HIT searches, even if not all of the assignments for the HIT have been accepted.
chitLifetimeInSeconds :: Lens' CreateHIT Integer
chitLifetimeInSeconds = lens _chitLifetimeInSeconds (\ s a -> s{_chitLifetimeInSeconds = a})

-- | The amount of time, in seconds, that a Worker has to complete the HIT after accepting it. If a Worker does not complete the assignment within the specified duration, the assignment is considered abandoned. If the HIT is still active (that is, its lifetime has not elapsed), the assignment becomes available for other users to find and accept.
chitAssignmentDurationInSeconds :: Lens' CreateHIT Integer
chitAssignmentDurationInSeconds = lens _chitAssignmentDurationInSeconds (\ s a -> s{_chitAssignmentDurationInSeconds = a})

-- | The amount of money the Requester will pay a Worker for successfully completing the HIT.
chitReward :: Lens' CreateHIT Text
chitReward = lens _chitReward (\ s a -> s{_chitReward = a})

-- | The title of the HIT. A title should be short and descriptive about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT title appears in search results, and everywhere the HIT is mentioned.
chitTitle :: Lens' CreateHIT Text
chitTitle = lens _chitTitle (\ s a -> s{_chitTitle = a})

-- | A general description of the HIT. A description includes detailed information about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT description appears in the expanded view of search results, and in the HIT and assignment screens. A good description gives the user enough information to evaluate the HIT before accepting it.
chitDescription :: Lens' CreateHIT Text
chitDescription = lens _chitDescription (\ s a -> s{_chitDescription = a})

instance AWSRequest CreateHIT where
        type Rs CreateHIT = CreateHITResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 CreateHITResponse' <$>
                   (x .?> "HIT") <*> (pure (fromEnum s)))

instance Hashable CreateHIT where

instance NFData CreateHIT where

instance ToHeaders CreateHIT where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.CreateHIT" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateHIT where
        toJSON CreateHIT'{..}
          = object
              (catMaybes
                 [("HITReviewPolicy" .=) <$> _chitHITReviewPolicy,
                  ("UniqueRequestToken" .=) <$>
                    _chitUniqueRequestToken,
                  ("AutoApprovalDelayInSeconds" .=) <$>
                    _chitAutoApprovalDelayInSeconds,
                  ("RequesterAnnotation" .=) <$>
                    _chitRequesterAnnotation,
                  ("MaxAssignments" .=) <$> _chitMaxAssignments,
                  ("Keywords" .=) <$> _chitKeywords,
                  ("HITLayoutId" .=) <$> _chitHITLayoutId,
                  ("HITLayoutParameters" .=) <$>
                    _chitHITLayoutParameters,
                  ("QualificationRequirements" .=) <$>
                    _chitQualificationRequirements,
                  ("Question" .=) <$> _chitQuestion,
                  ("AssignmentReviewPolicy" .=) <$>
                    _chitAssignmentReviewPolicy,
                  Just ("LifetimeInSeconds" .= _chitLifetimeInSeconds),
                  Just
                    ("AssignmentDurationInSeconds" .=
                       _chitAssignmentDurationInSeconds),
                  Just ("Reward" .= _chitReward),
                  Just ("Title" .= _chitTitle),
                  Just ("Description" .= _chitDescription)])

instance ToPath CreateHIT where
        toPath = const "/"

instance ToQuery CreateHIT where
        toQuery = const mempty

-- | /See:/ 'createHITResponse' smart constructor.
data CreateHITResponse = CreateHITResponse'
  { _chitrsHIT            :: !(Maybe HIT)
  , _chitrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHITResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chitrsHIT' - Contains the newly created HIT data. For a description of the HIT data structure as it appears in responses, see the HIT Data Structure documentation.
--
-- * 'chitrsResponseStatus' - -- | The response status code.
createHITResponse
    :: Int -- ^ 'chitrsResponseStatus'
    -> CreateHITResponse
createHITResponse pResponseStatus_ =
  CreateHITResponse'
    {_chitrsHIT = Nothing, _chitrsResponseStatus = pResponseStatus_}


-- | Contains the newly created HIT data. For a description of the HIT data structure as it appears in responses, see the HIT Data Structure documentation.
chitrsHIT :: Lens' CreateHITResponse (Maybe HIT)
chitrsHIT = lens _chitrsHIT (\ s a -> s{_chitrsHIT = a})

-- | -- | The response status code.
chitrsResponseStatus :: Lens' CreateHITResponse Int
chitrsResponseStatus = lens _chitrsResponseStatus (\ s a -> s{_chitrsResponseStatus = a})

instance NFData CreateHITResponse where
