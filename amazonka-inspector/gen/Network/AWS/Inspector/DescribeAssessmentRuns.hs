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
-- Module      : Network.AWS.Inspector.DescribeAssessmentRuns
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment runs that are specified by the ARNs of the assessment runs.
--
--
module Network.AWS.Inspector.DescribeAssessmentRuns
    (
    -- * Creating a Request
      describeAssessmentRuns
    , DescribeAssessmentRuns
    -- * Request Lenses
    , darAssessmentRunARNs

    -- * Destructuring the Response
    , describeAssessmentRunsResponse
    , DescribeAssessmentRunsResponse
    -- * Response Lenses
    , darrsResponseStatus
    , darrsAssessmentRuns
    , darrsFailedItems
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAssessmentRuns' smart constructor.
newtype DescribeAssessmentRuns = DescribeAssessmentRuns'
  { _darAssessmentRunARNs :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAssessmentRuns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darAssessmentRunARNs' - The ARN that specifies the assessment run that you want to describe.
describeAssessmentRuns
    :: NonEmpty Text -- ^ 'darAssessmentRunARNs'
    -> DescribeAssessmentRuns
describeAssessmentRuns pAssessmentRunARNs_ =
  DescribeAssessmentRuns' {_darAssessmentRunARNs = _List1 # pAssessmentRunARNs_}


-- | The ARN that specifies the assessment run that you want to describe.
darAssessmentRunARNs :: Lens' DescribeAssessmentRuns (NonEmpty Text)
darAssessmentRunARNs = lens _darAssessmentRunARNs (\ s a -> s{_darAssessmentRunARNs = a}) . _List1

instance AWSRequest DescribeAssessmentRuns where
        type Rs DescribeAssessmentRuns =
             DescribeAssessmentRunsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAssessmentRunsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "assessmentRuns" .!@ mempty)
                     <*> (x .?> "failedItems" .!@ mempty))

instance Hashable DescribeAssessmentRuns where

instance NFData DescribeAssessmentRuns where

instance ToHeaders DescribeAssessmentRuns where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeAssessmentRuns" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAssessmentRuns where
        toJSON DescribeAssessmentRuns'{..}
          = object
              (catMaybes
                 [Just
                    ("assessmentRunArns" .= _darAssessmentRunARNs)])

instance ToPath DescribeAssessmentRuns where
        toPath = const "/"

instance ToQuery DescribeAssessmentRuns where
        toQuery = const mempty

-- | /See:/ 'describeAssessmentRunsResponse' smart constructor.
data DescribeAssessmentRunsResponse = DescribeAssessmentRunsResponse'
  { _darrsResponseStatus :: !Int
  , _darrsAssessmentRuns :: ![AssessmentRun]
  , _darrsFailedItems    :: !(Map Text FailedItemDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAssessmentRunsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darrsResponseStatus' - -- | The response status code.
--
-- * 'darrsAssessmentRuns' - Information about the assessment run.
--
-- * 'darrsFailedItems' - Assessment run details that cannot be described. An error code is provided for each failed item.
describeAssessmentRunsResponse
    :: Int -- ^ 'darrsResponseStatus'
    -> DescribeAssessmentRunsResponse
describeAssessmentRunsResponse pResponseStatus_ =
  DescribeAssessmentRunsResponse'
    { _darrsResponseStatus = pResponseStatus_
    , _darrsAssessmentRuns = mempty
    , _darrsFailedItems = mempty
    }


-- | -- | The response status code.
darrsResponseStatus :: Lens' DescribeAssessmentRunsResponse Int
darrsResponseStatus = lens _darrsResponseStatus (\ s a -> s{_darrsResponseStatus = a})

-- | Information about the assessment run.
darrsAssessmentRuns :: Lens' DescribeAssessmentRunsResponse [AssessmentRun]
darrsAssessmentRuns = lens _darrsAssessmentRuns (\ s a -> s{_darrsAssessmentRuns = a}) . _Coerce

-- | Assessment run details that cannot be described. An error code is provided for each failed item.
darrsFailedItems :: Lens' DescribeAssessmentRunsResponse (HashMap Text FailedItemDetails)
darrsFailedItems = lens _darrsFailedItems (\ s a -> s{_darrsFailedItems = a}) . _Map

instance NFData DescribeAssessmentRunsResponse where
