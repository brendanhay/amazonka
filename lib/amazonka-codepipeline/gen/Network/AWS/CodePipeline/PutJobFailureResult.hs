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
-- Module      : Network.AWS.CodePipeline.PutJobFailureResult
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a job as returned to the pipeline by a job worker. Only used for custom actions.
--
--
module Network.AWS.CodePipeline.PutJobFailureResult
    (
    -- * Creating a Request
      putJobFailureResult
    , PutJobFailureResult
    -- * Request Lenses
    , pjfrJobId
    , pjfrFailureDetails

    -- * Destructuring the Response
    , putJobFailureResultResponse
    , PutJobFailureResultResponse
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a PutJobFailureResult action.
--
--
--
-- /See:/ 'putJobFailureResult' smart constructor.
data PutJobFailureResult = PutJobFailureResult'
  { _pjfrJobId          :: !Text
  , _pjfrFailureDetails :: !FailureDetails
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutJobFailureResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pjfrJobId' - The unique system-generated ID of the job that failed. This is the same ID returned from PollForJobs.
--
-- * 'pjfrFailureDetails' - The details about the failure of a job.
putJobFailureResult
    :: Text -- ^ 'pjfrJobId'
    -> FailureDetails -- ^ 'pjfrFailureDetails'
    -> PutJobFailureResult
putJobFailureResult pJobId_ pFailureDetails_ =
  PutJobFailureResult'
    {_pjfrJobId = pJobId_, _pjfrFailureDetails = pFailureDetails_}


-- | The unique system-generated ID of the job that failed. This is the same ID returned from PollForJobs.
pjfrJobId :: Lens' PutJobFailureResult Text
pjfrJobId = lens _pjfrJobId (\ s a -> s{_pjfrJobId = a})

-- | The details about the failure of a job.
pjfrFailureDetails :: Lens' PutJobFailureResult FailureDetails
pjfrFailureDetails = lens _pjfrFailureDetails (\ s a -> s{_pjfrFailureDetails = a})

instance AWSRequest PutJobFailureResult where
        type Rs PutJobFailureResult =
             PutJobFailureResultResponse
        request = postJSON codePipeline
        response = receiveNull PutJobFailureResultResponse'

instance Hashable PutJobFailureResult where

instance NFData PutJobFailureResult where

instance ToHeaders PutJobFailureResult where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.PutJobFailureResult" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutJobFailureResult where
        toJSON PutJobFailureResult'{..}
          = object
              (catMaybes
                 [Just ("jobId" .= _pjfrJobId),
                  Just ("failureDetails" .= _pjfrFailureDetails)])

instance ToPath PutJobFailureResult where
        toPath = const "/"

instance ToQuery PutJobFailureResult where
        toQuery = const mempty

-- | /See:/ 'putJobFailureResultResponse' smart constructor.
data PutJobFailureResultResponse =
  PutJobFailureResultResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutJobFailureResultResponse' with the minimum fields required to make a request.
--
putJobFailureResultResponse
    :: PutJobFailureResultResponse
putJobFailureResultResponse = PutJobFailureResultResponse'


instance NFData PutJobFailureResultResponse where
