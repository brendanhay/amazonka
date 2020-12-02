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
-- Module      : Network.AWS.MechanicalTurk.CreateHITType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateHITType@ operation creates a new HIT type. This operation allows you to define a standard set of HIT properties to use when creating HITs. If you register a HIT type with values that match an existing HIT type, the HIT type ID of the existing type will be returned.
--
--
module Network.AWS.MechanicalTurk.CreateHITType
    (
    -- * Creating a Request
      createHITType
    , CreateHITType
    -- * Request Lenses
    , chittAutoApprovalDelayInSeconds
    , chittKeywords
    , chittQualificationRequirements
    , chittAssignmentDurationInSeconds
    , chittReward
    , chittTitle
    , chittDescription

    -- * Destructuring the Response
    , createHITTypeResponse
    , CreateHITTypeResponse
    -- * Response Lenses
    , chittrsHITTypeId
    , chittrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createHITType' smart constructor.
data CreateHITType = CreateHITType'
  { _chittAutoApprovalDelayInSeconds  :: !(Maybe Integer)
  , _chittKeywords                    :: !(Maybe Text)
  , _chittQualificationRequirements   :: !(Maybe [QualificationRequirement])
  , _chittAssignmentDurationInSeconds :: !Integer
  , _chittReward                      :: !Text
  , _chittTitle                       :: !Text
  , _chittDescription                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHITType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chittAutoApprovalDelayInSeconds' - The number of seconds after an assignment for the HIT has been submitted, after which the assignment is considered Approved automatically unless the Requester explicitly rejects it.
--
-- * 'chittKeywords' - One or more words or phrases that describe the HIT, separated by commas. These words are used in searches to find HITs.
--
-- * 'chittQualificationRequirements' - Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
--
-- * 'chittAssignmentDurationInSeconds' - The amount of time, in seconds, that a Worker has to complete the HIT after accepting it. If a Worker does not complete the assignment within the specified duration, the assignment is considered abandoned. If the HIT is still active (that is, its lifetime has not elapsed), the assignment becomes available for other users to find and accept.
--
-- * 'chittReward' - The amount of money the Requester will pay a Worker for successfully completing the HIT.
--
-- * 'chittTitle' - The title of the HIT. A title should be short and descriptive about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT title appears in search results, and everywhere the HIT is mentioned.
--
-- * 'chittDescription' - A general description of the HIT. A description includes detailed information about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT description appears in the expanded view of search results, and in the HIT and assignment screens. A good description gives the user enough information to evaluate the HIT before accepting it.
createHITType
    :: Integer -- ^ 'chittAssignmentDurationInSeconds'
    -> Text -- ^ 'chittReward'
    -> Text -- ^ 'chittTitle'
    -> Text -- ^ 'chittDescription'
    -> CreateHITType
createHITType pAssignmentDurationInSeconds_ pReward_ pTitle_ pDescription_ =
  CreateHITType'
    { _chittAutoApprovalDelayInSeconds = Nothing
    , _chittKeywords = Nothing
    , _chittQualificationRequirements = Nothing
    , _chittAssignmentDurationInSeconds = pAssignmentDurationInSeconds_
    , _chittReward = pReward_
    , _chittTitle = pTitle_
    , _chittDescription = pDescription_
    }


-- | The number of seconds after an assignment for the HIT has been submitted, after which the assignment is considered Approved automatically unless the Requester explicitly rejects it.
chittAutoApprovalDelayInSeconds :: Lens' CreateHITType (Maybe Integer)
chittAutoApprovalDelayInSeconds = lens _chittAutoApprovalDelayInSeconds (\ s a -> s{_chittAutoApprovalDelayInSeconds = a})

-- | One or more words or phrases that describe the HIT, separated by commas. These words are used in searches to find HITs.
chittKeywords :: Lens' CreateHITType (Maybe Text)
chittKeywords = lens _chittKeywords (\ s a -> s{_chittKeywords = a})

-- | Conditions that a Worker's Qualifications must meet in order to accept the HIT. A HIT can have between zero and ten Qualification requirements. All requirements must be met in order for a Worker to accept the HIT. Additionally, other actions can be restricted using the @ActionsGuarded@ field on each @QualificationRequirement@ structure.
chittQualificationRequirements :: Lens' CreateHITType [QualificationRequirement]
chittQualificationRequirements = lens _chittQualificationRequirements (\ s a -> s{_chittQualificationRequirements = a}) . _Default . _Coerce

-- | The amount of time, in seconds, that a Worker has to complete the HIT after accepting it. If a Worker does not complete the assignment within the specified duration, the assignment is considered abandoned. If the HIT is still active (that is, its lifetime has not elapsed), the assignment becomes available for other users to find and accept.
chittAssignmentDurationInSeconds :: Lens' CreateHITType Integer
chittAssignmentDurationInSeconds = lens _chittAssignmentDurationInSeconds (\ s a -> s{_chittAssignmentDurationInSeconds = a})

-- | The amount of money the Requester will pay a Worker for successfully completing the HIT.
chittReward :: Lens' CreateHITType Text
chittReward = lens _chittReward (\ s a -> s{_chittReward = a})

-- | The title of the HIT. A title should be short and descriptive about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT title appears in search results, and everywhere the HIT is mentioned.
chittTitle :: Lens' CreateHITType Text
chittTitle = lens _chittTitle (\ s a -> s{_chittTitle = a})

-- | A general description of the HIT. A description includes detailed information about the kind of task the HIT contains. On the Amazon Mechanical Turk web site, the HIT description appears in the expanded view of search results, and in the HIT and assignment screens. A good description gives the user enough information to evaluate the HIT before accepting it.
chittDescription :: Lens' CreateHITType Text
chittDescription = lens _chittDescription (\ s a -> s{_chittDescription = a})

instance AWSRequest CreateHITType where
        type Rs CreateHITType = CreateHITTypeResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 CreateHITTypeResponse' <$>
                   (x .?> "HITTypeId") <*> (pure (fromEnum s)))

instance Hashable CreateHITType where

instance NFData CreateHITType where

instance ToHeaders CreateHITType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.CreateHITType" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateHITType where
        toJSON CreateHITType'{..}
          = object
              (catMaybes
                 [("AutoApprovalDelayInSeconds" .=) <$>
                    _chittAutoApprovalDelayInSeconds,
                  ("Keywords" .=) <$> _chittKeywords,
                  ("QualificationRequirements" .=) <$>
                    _chittQualificationRequirements,
                  Just
                    ("AssignmentDurationInSeconds" .=
                       _chittAssignmentDurationInSeconds),
                  Just ("Reward" .= _chittReward),
                  Just ("Title" .= _chittTitle),
                  Just ("Description" .= _chittDescription)])

instance ToPath CreateHITType where
        toPath = const "/"

instance ToQuery CreateHITType where
        toQuery = const mempty

-- | /See:/ 'createHITTypeResponse' smart constructor.
data CreateHITTypeResponse = CreateHITTypeResponse'
  { _chittrsHITTypeId      :: !(Maybe Text)
  , _chittrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHITTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chittrsHITTypeId' - The ID of the newly registered HIT type.
--
-- * 'chittrsResponseStatus' - -- | The response status code.
createHITTypeResponse
    :: Int -- ^ 'chittrsResponseStatus'
    -> CreateHITTypeResponse
createHITTypeResponse pResponseStatus_ =
  CreateHITTypeResponse'
    {_chittrsHITTypeId = Nothing, _chittrsResponseStatus = pResponseStatus_}


-- | The ID of the newly registered HIT type.
chittrsHITTypeId :: Lens' CreateHITTypeResponse (Maybe Text)
chittrsHITTypeId = lens _chittrsHITTypeId (\ s a -> s{_chittrsHITTypeId = a})

-- | -- | The response status code.
chittrsResponseStatus :: Lens' CreateHITTypeResponse Int
chittrsResponseStatus = lens _chittrsResponseStatus (\ s a -> s{_chittrsResponseStatus = a})

instance NFData CreateHITTypeResponse where
