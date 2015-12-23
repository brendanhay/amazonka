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
-- Module      : Network.AWS.Inspector.ListAssessmentAgents
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the agents of the assessment specified by the assessment ARN.
--
-- /See:/ <http://docs.aws.amazon.com/inspector/latest/APIReference/API_ListAssessmentAgents.html AWS API Reference> for ListAssessmentAgents.
module Network.AWS.Inspector.ListAssessmentAgents
    (
    -- * Creating a Request
      listAssessmentAgents
    , ListAssessmentAgents
    -- * Request Lenses
    , laasNextToken
    , laasFilter
    , laasMaxResults
    , laasAssessmentARN

    -- * Destructuring the Response
    , listAssessmentAgentsResponse
    , ListAssessmentAgentsResponse
    -- * Response Lenses
    , laarsAgentList
    , laarsNextToken
    , laarsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAssessmentAgents' smart constructor.
data ListAssessmentAgents = ListAssessmentAgents'
    { _laasNextToken     :: !(Maybe Text)
    , _laasFilter        :: !(Maybe AgentsFilter)
    , _laasMaxResults    :: !(Maybe Int)
    , _laasAssessmentARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAssessmentAgents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laasNextToken'
--
-- * 'laasFilter'
--
-- * 'laasMaxResults'
--
-- * 'laasAssessmentARN'
listAssessmentAgents
    :: Text -- ^ 'laasAssessmentARN'
    -> ListAssessmentAgents
listAssessmentAgents pAssessmentARN_ =
    ListAssessmentAgents'
    { _laasNextToken = Nothing
    , _laasFilter = Nothing
    , _laasMaxResults = Nothing
    , _laasAssessmentARN = pAssessmentARN_
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to \'null\' on your first call to the
-- __ListAssessmentAgents__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from
-- previous response to continue listing data.
laasNextToken :: Lens' ListAssessmentAgents (Maybe Text)
laasNextToken = lens _laasNextToken (\ s a -> s{_laasNextToken = a});

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
laasFilter :: Lens' ListAssessmentAgents (Maybe AgentsFilter)
laasFilter = lens _laasFilter (\ s a -> s{_laasFilter = a});

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
laasMaxResults :: Lens' ListAssessmentAgents (Maybe Int)
laasMaxResults = lens _laasMaxResults (\ s a -> s{_laasMaxResults = a});

-- | The ARN specifying the assessment whose agents you want to list.
laasAssessmentARN :: Lens' ListAssessmentAgents Text
laasAssessmentARN = lens _laasAssessmentARN (\ s a -> s{_laasAssessmentARN = a});

instance AWSRequest ListAssessmentAgents where
        type Rs ListAssessmentAgents =
             ListAssessmentAgentsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 ListAssessmentAgentsResponse' <$>
                   (x .?> "agentList" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListAssessmentAgents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.ListAssessmentAgents" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAssessmentAgents where
        toJSON ListAssessmentAgents'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _laasNextToken,
                  ("filter" .=) <$> _laasFilter,
                  ("maxResults" .=) <$> _laasMaxResults,
                  Just ("assessmentArn" .= _laasAssessmentARN)])

instance ToPath ListAssessmentAgents where
        toPath = const "/"

instance ToQuery ListAssessmentAgents where
        toQuery = const mempty

-- | /See:/ 'listAssessmentAgentsResponse' smart constructor.
data ListAssessmentAgentsResponse = ListAssessmentAgentsResponse'
    { _laarsAgentList      :: !(Maybe [Agent])
    , _laarsNextToken      :: !(Maybe Text)
    , _laarsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAssessmentAgentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laarsAgentList'
--
-- * 'laarsNextToken'
--
-- * 'laarsResponseStatus'
listAssessmentAgentsResponse
    :: Int -- ^ 'laarsResponseStatus'
    -> ListAssessmentAgentsResponse
listAssessmentAgentsResponse pResponseStatus_ =
    ListAssessmentAgentsResponse'
    { _laarsAgentList = Nothing
    , _laarsNextToken = Nothing
    , _laarsResponseStatus = pResponseStatus_
    }

-- | A list of ARNs specifying the agents returned by the action.
laarsAgentList :: Lens' ListAssessmentAgentsResponse [Agent]
laarsAgentList = lens _laarsAgentList (\ s a -> s{_laarsAgentList = a}) . _Default . _Coerce;

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to \'null\'.
laarsNextToken :: Lens' ListAssessmentAgentsResponse (Maybe Text)
laarsNextToken = lens _laarsNextToken (\ s a -> s{_laarsNextToken = a});

-- | The response status code.
laarsResponseStatus :: Lens' ListAssessmentAgentsResponse Int
laarsResponseStatus = lens _laarsResponseStatus (\ s a -> s{_laarsResponseStatus = a});
