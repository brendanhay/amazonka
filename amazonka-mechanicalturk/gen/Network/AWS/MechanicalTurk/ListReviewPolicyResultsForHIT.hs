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
-- Module      : Network.AWS.MechanicalTurk.ListReviewPolicyResultsForHIT
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListReviewPolicyResultsForHIT@ operation retrieves the computed results and the actions taken in the course of executing your Review Policies for a given HIT. For information about how to specify Review Policies when you call CreateHIT, see Review Policies. The ListReviewPolicyResultsForHIT operation can return results for both Assignment-level and HIT-level review results.
--
--
module Network.AWS.MechanicalTurk.ListReviewPolicyResultsForHIT
    (
    -- * Creating a Request
      listReviewPolicyResultsForHIT
    , ListReviewPolicyResultsForHIT
    -- * Request Lenses
    , lrprfhitRetrieveResults
    , lrprfhitPolicyLevels
    , lrprfhitRetrieveActions
    , lrprfhitNextToken
    , lrprfhitMaxResults
    , lrprfhitHITId

    -- * Destructuring the Response
    , listReviewPolicyResultsForHITResponse
    , ListReviewPolicyResultsForHITResponse
    -- * Response Lenses
    , lrprfhitrsHITReviewPolicy
    , lrprfhitrsHITReviewReport
    , lrprfhitrsNextToken
    , lrprfhitrsAssignmentReviewReport
    , lrprfhitrsHITId
    , lrprfhitrsAssignmentReviewPolicy
    , lrprfhitrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listReviewPolicyResultsForHIT' smart constructor.
data ListReviewPolicyResultsForHIT = ListReviewPolicyResultsForHIT'
  { _lrprfhitRetrieveResults :: !(Maybe Bool)
  , _lrprfhitPolicyLevels    :: !(Maybe [ReviewPolicyLevel])
  , _lrprfhitRetrieveActions :: !(Maybe Bool)
  , _lrprfhitNextToken       :: !(Maybe Text)
  , _lrprfhitMaxResults      :: !(Maybe Nat)
  , _lrprfhitHITId           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListReviewPolicyResultsForHIT' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrprfhitRetrieveResults' - Specify if the operation should retrieve a list of the results computed by the Review Policies.
--
-- * 'lrprfhitPolicyLevels' - The Policy Level(s) to retrieve review results for - HIT or Assignment. If omitted, the default behavior is to retrieve all data for both policy levels. For a list of all the described policies, see Review Policies.
--
-- * 'lrprfhitRetrieveActions' - Specify if the operation should retrieve a list of the actions taken executing the Review Policies and their outcomes.
--
-- * 'lrprfhitNextToken' - Pagination token
--
-- * 'lrprfhitMaxResults' - Limit the number of results returned.
--
-- * 'lrprfhitHITId' - The unique identifier of the HIT to retrieve review results for.
listReviewPolicyResultsForHIT
    :: Text -- ^ 'lrprfhitHITId'
    -> ListReviewPolicyResultsForHIT
listReviewPolicyResultsForHIT pHITId_ =
  ListReviewPolicyResultsForHIT'
    { _lrprfhitRetrieveResults = Nothing
    , _lrprfhitPolicyLevels = Nothing
    , _lrprfhitRetrieveActions = Nothing
    , _lrprfhitNextToken = Nothing
    , _lrprfhitMaxResults = Nothing
    , _lrprfhitHITId = pHITId_
    }


-- | Specify if the operation should retrieve a list of the results computed by the Review Policies.
lrprfhitRetrieveResults :: Lens' ListReviewPolicyResultsForHIT (Maybe Bool)
lrprfhitRetrieveResults = lens _lrprfhitRetrieveResults (\ s a -> s{_lrprfhitRetrieveResults = a})

-- | The Policy Level(s) to retrieve review results for - HIT or Assignment. If omitted, the default behavior is to retrieve all data for both policy levels. For a list of all the described policies, see Review Policies.
lrprfhitPolicyLevels :: Lens' ListReviewPolicyResultsForHIT [ReviewPolicyLevel]
lrprfhitPolicyLevels = lens _lrprfhitPolicyLevels (\ s a -> s{_lrprfhitPolicyLevels = a}) . _Default . _Coerce

-- | Specify if the operation should retrieve a list of the actions taken executing the Review Policies and their outcomes.
lrprfhitRetrieveActions :: Lens' ListReviewPolicyResultsForHIT (Maybe Bool)
lrprfhitRetrieveActions = lens _lrprfhitRetrieveActions (\ s a -> s{_lrprfhitRetrieveActions = a})

-- | Pagination token
lrprfhitNextToken :: Lens' ListReviewPolicyResultsForHIT (Maybe Text)
lrprfhitNextToken = lens _lrprfhitNextToken (\ s a -> s{_lrprfhitNextToken = a})

-- | Limit the number of results returned.
lrprfhitMaxResults :: Lens' ListReviewPolicyResultsForHIT (Maybe Natural)
lrprfhitMaxResults = lens _lrprfhitMaxResults (\ s a -> s{_lrprfhitMaxResults = a}) . mapping _Nat

-- | The unique identifier of the HIT to retrieve review results for.
lrprfhitHITId :: Lens' ListReviewPolicyResultsForHIT Text
lrprfhitHITId = lens _lrprfhitHITId (\ s a -> s{_lrprfhitHITId = a})

instance AWSRequest ListReviewPolicyResultsForHIT
         where
        type Rs ListReviewPolicyResultsForHIT =
             ListReviewPolicyResultsForHITResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 ListReviewPolicyResultsForHITResponse' <$>
                   (x .?> "HITReviewPolicy") <*>
                     (x .?> "HITReviewReport")
                     <*> (x .?> "NextToken")
                     <*> (x .?> "AssignmentReviewReport")
                     <*> (x .?> "HITId")
                     <*> (x .?> "AssignmentReviewPolicy")
                     <*> (pure (fromEnum s)))

instance Hashable ListReviewPolicyResultsForHIT where

instance NFData ListReviewPolicyResultsForHIT where

instance ToHeaders ListReviewPolicyResultsForHIT
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.ListReviewPolicyResultsForHIT"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListReviewPolicyResultsForHIT where
        toJSON ListReviewPolicyResultsForHIT'{..}
          = object
              (catMaybes
                 [("RetrieveResults" .=) <$> _lrprfhitRetrieveResults,
                  ("PolicyLevels" .=) <$> _lrprfhitPolicyLevels,
                  ("RetrieveActions" .=) <$> _lrprfhitRetrieveActions,
                  ("NextToken" .=) <$> _lrprfhitNextToken,
                  ("MaxResults" .=) <$> _lrprfhitMaxResults,
                  Just ("HITId" .= _lrprfhitHITId)])

instance ToPath ListReviewPolicyResultsForHIT where
        toPath = const "/"

instance ToQuery ListReviewPolicyResultsForHIT where
        toQuery = const mempty

-- | /See:/ 'listReviewPolicyResultsForHITResponse' smart constructor.
data ListReviewPolicyResultsForHITResponse = ListReviewPolicyResultsForHITResponse'
  { _lrprfhitrsHITReviewPolicy        :: !(Maybe ReviewPolicy)
  , _lrprfhitrsHITReviewReport        :: !(Maybe ReviewReport)
  , _lrprfhitrsNextToken              :: !(Maybe Text)
  , _lrprfhitrsAssignmentReviewReport :: !(Maybe ReviewReport)
  , _lrprfhitrsHITId                  :: !(Maybe Text)
  , _lrprfhitrsAssignmentReviewPolicy :: !(Maybe ReviewPolicy)
  , _lrprfhitrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListReviewPolicyResultsForHITResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrprfhitrsHITReviewPolicy' - The name of the HIT-level Review Policy. This contains only the PolicyName element.
--
-- * 'lrprfhitrsHITReviewReport' - Contains both ReviewResult and ReviewAction elements for a particular HIT.
--
-- * 'lrprfhitrsNextToken' - Undocumented member.
--
-- * 'lrprfhitrsAssignmentReviewReport' - Contains both ReviewResult and ReviewAction elements for an Assignment.
--
-- * 'lrprfhitrsHITId' - The HITId of the HIT for which results have been returned.
--
-- * 'lrprfhitrsAssignmentReviewPolicy' - The name of the Assignment-level Review Policy. This contains only the PolicyName element.
--
-- * 'lrprfhitrsResponseStatus' - -- | The response status code.
listReviewPolicyResultsForHITResponse
    :: Int -- ^ 'lrprfhitrsResponseStatus'
    -> ListReviewPolicyResultsForHITResponse
listReviewPolicyResultsForHITResponse pResponseStatus_ =
  ListReviewPolicyResultsForHITResponse'
    { _lrprfhitrsHITReviewPolicy = Nothing
    , _lrprfhitrsHITReviewReport = Nothing
    , _lrprfhitrsNextToken = Nothing
    , _lrprfhitrsAssignmentReviewReport = Nothing
    , _lrprfhitrsHITId = Nothing
    , _lrprfhitrsAssignmentReviewPolicy = Nothing
    , _lrprfhitrsResponseStatus = pResponseStatus_
    }


-- | The name of the HIT-level Review Policy. This contains only the PolicyName element.
lrprfhitrsHITReviewPolicy :: Lens' ListReviewPolicyResultsForHITResponse (Maybe ReviewPolicy)
lrprfhitrsHITReviewPolicy = lens _lrprfhitrsHITReviewPolicy (\ s a -> s{_lrprfhitrsHITReviewPolicy = a})

-- | Contains both ReviewResult and ReviewAction elements for a particular HIT.
lrprfhitrsHITReviewReport :: Lens' ListReviewPolicyResultsForHITResponse (Maybe ReviewReport)
lrprfhitrsHITReviewReport = lens _lrprfhitrsHITReviewReport (\ s a -> s{_lrprfhitrsHITReviewReport = a})

-- | Undocumented member.
lrprfhitrsNextToken :: Lens' ListReviewPolicyResultsForHITResponse (Maybe Text)
lrprfhitrsNextToken = lens _lrprfhitrsNextToken (\ s a -> s{_lrprfhitrsNextToken = a})

-- | Contains both ReviewResult and ReviewAction elements for an Assignment.
lrprfhitrsAssignmentReviewReport :: Lens' ListReviewPolicyResultsForHITResponse (Maybe ReviewReport)
lrprfhitrsAssignmentReviewReport = lens _lrprfhitrsAssignmentReviewReport (\ s a -> s{_lrprfhitrsAssignmentReviewReport = a})

-- | The HITId of the HIT for which results have been returned.
lrprfhitrsHITId :: Lens' ListReviewPolicyResultsForHITResponse (Maybe Text)
lrprfhitrsHITId = lens _lrprfhitrsHITId (\ s a -> s{_lrprfhitrsHITId = a})

-- | The name of the Assignment-level Review Policy. This contains only the PolicyName element.
lrprfhitrsAssignmentReviewPolicy :: Lens' ListReviewPolicyResultsForHITResponse (Maybe ReviewPolicy)
lrprfhitrsAssignmentReviewPolicy = lens _lrprfhitrsAssignmentReviewPolicy (\ s a -> s{_lrprfhitrsAssignmentReviewPolicy = a})

-- | -- | The response status code.
lrprfhitrsResponseStatus :: Lens' ListReviewPolicyResultsForHITResponse Int
lrprfhitrsResponseStatus = lens _lrprfhitrsResponseStatus (\ s a -> s{_lrprfhitrsResponseStatus = a})

instance NFData ListReviewPolicyResultsForHITResponse
         where
