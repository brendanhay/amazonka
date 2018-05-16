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
-- Module      : Network.AWS.CodePipeline.PollForJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about any jobs for AWS CodePipeline to act upon. PollForJobs is only valid for action types with "Custom" in the owner field. If the action type contains "AWS" or "ThirdParty" in the owner field, the PollForJobs action returns an error.
--
--
-- /Important:/ When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.
--
module Network.AWS.CodePipeline.PollForJobs
    (
    -- * Creating a Request
      pollForJobs
    , PollForJobs
    -- * Request Lenses
    , pfjMaxBatchSize
    , pfjQueryParam
    , pfjActionTypeId

    -- * Destructuring the Response
    , pollForJobsResponse
    , PollForJobsResponse
    -- * Response Lenses
    , pfjrsJobs
    , pfjrsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a PollForJobs action.
--
--
--
-- /See:/ 'pollForJobs' smart constructor.
data PollForJobs = PollForJobs'
  { _pfjMaxBatchSize :: !(Maybe Nat)
  , _pfjQueryParam   :: !(Maybe (Map Text Text))
  , _pfjActionTypeId :: !ActionTypeId
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PollForJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfjMaxBatchSize' - The maximum number of jobs to return in a poll for jobs call.
--
-- * 'pfjQueryParam' - A map of property names and values. For an action type with no queryable properties, this value must be null or an empty map. For an action type with a queryable property, you must supply that property as a key in the map. Only jobs whose action configuration matches the mapped value will be returned.
--
-- * 'pfjActionTypeId' - Represents information about an action type.
pollForJobs
    :: ActionTypeId -- ^ 'pfjActionTypeId'
    -> PollForJobs
pollForJobs pActionTypeId_ =
  PollForJobs'
    { _pfjMaxBatchSize = Nothing
    , _pfjQueryParam = Nothing
    , _pfjActionTypeId = pActionTypeId_
    }


-- | The maximum number of jobs to return in a poll for jobs call.
pfjMaxBatchSize :: Lens' PollForJobs (Maybe Natural)
pfjMaxBatchSize = lens _pfjMaxBatchSize (\ s a -> s{_pfjMaxBatchSize = a}) . mapping _Nat

-- | A map of property names and values. For an action type with no queryable properties, this value must be null or an empty map. For an action type with a queryable property, you must supply that property as a key in the map. Only jobs whose action configuration matches the mapped value will be returned.
pfjQueryParam :: Lens' PollForJobs (HashMap Text Text)
pfjQueryParam = lens _pfjQueryParam (\ s a -> s{_pfjQueryParam = a}) . _Default . _Map

-- | Represents information about an action type.
pfjActionTypeId :: Lens' PollForJobs ActionTypeId
pfjActionTypeId = lens _pfjActionTypeId (\ s a -> s{_pfjActionTypeId = a})

instance AWSRequest PollForJobs where
        type Rs PollForJobs = PollForJobsResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 PollForJobsResponse' <$>
                   (x .?> "jobs" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable PollForJobs where

instance NFData PollForJobs where

instance ToHeaders PollForJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.PollForJobs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PollForJobs where
        toJSON PollForJobs'{..}
          = object
              (catMaybes
                 [("maxBatchSize" .=) <$> _pfjMaxBatchSize,
                  ("queryParam" .=) <$> _pfjQueryParam,
                  Just ("actionTypeId" .= _pfjActionTypeId)])

instance ToPath PollForJobs where
        toPath = const "/"

instance ToQuery PollForJobs where
        toQuery = const mempty

-- | Represents the output of a PollForJobs action.
--
--
--
-- /See:/ 'pollForJobsResponse' smart constructor.
data PollForJobsResponse = PollForJobsResponse'
  { _pfjrsJobs           :: !(Maybe [Job])
  , _pfjrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PollForJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfjrsJobs' - Information about the jobs to take action on.
--
-- * 'pfjrsResponseStatus' - -- | The response status code.
pollForJobsResponse
    :: Int -- ^ 'pfjrsResponseStatus'
    -> PollForJobsResponse
pollForJobsResponse pResponseStatus_ =
  PollForJobsResponse'
    {_pfjrsJobs = Nothing, _pfjrsResponseStatus = pResponseStatus_}


-- | Information about the jobs to take action on.
pfjrsJobs :: Lens' PollForJobsResponse [Job]
pfjrsJobs = lens _pfjrsJobs (\ s a -> s{_pfjrsJobs = a}) . _Default . _Coerce

-- | -- | The response status code.
pfjrsResponseStatus :: Lens' PollForJobsResponse Int
pfjrsResponseStatus = lens _pfjrsResponseStatus (\ s a -> s{_pfjrsResponseStatus = a})

instance NFData PollForJobsResponse where
