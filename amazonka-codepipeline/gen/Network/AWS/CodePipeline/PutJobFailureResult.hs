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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a job as returned to the pipeline by a job
-- worker. Only used for custom actions.
--
-- /See:/ <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_PutJobFailureResult.html AWS API Reference> for PutJobFailureResult.
module Network.AWS.CodePipeline.PutJobFailureResult
    (
    -- * Creating a Request
      PutJobFailureResult
    , putJobFailureResult
    -- * Request Lenses
    , pjfrJobId
    , pjfrFailureDetails

    -- * Destructuring the Response
    , PutJobFailureResultResponse
    , putJobFailureResultResponse
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a put job failure result action.
--
-- /See:/ 'putJobFailureResult' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pjfrJobId'
--
-- * 'pjfrFailureDetails'
data PutJobFailureResult = PutJobFailureResult'
    { _pjfrJobId :: !Text
    , _pjfrFailureDetails :: !FailureDetails
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutJobFailureResult' smart constructor.
putJobFailureResult :: Text -> FailureDetails -> PutJobFailureResult
putJobFailureResult pJobId_ pFailureDetails_ = 
    PutJobFailureResult'
    { _pjfrJobId = pJobId_
    , _pjfrFailureDetails = pFailureDetails_
    }

-- | The unique system-generated ID of the job that failed. This is the same
-- ID returned from PollForJobs.
pjfrJobId :: Lens' PutJobFailureResult Text
pjfrJobId = lens _pjfrJobId (\ s a -> s{_pjfrJobId = a});

-- | The details about the failure of a job.
pjfrFailureDetails :: Lens' PutJobFailureResult FailureDetails
pjfrFailureDetails = lens _pjfrFailureDetails (\ s a -> s{_pjfrFailureDetails = a});

instance AWSRequest PutJobFailureResult where
        type Sv PutJobFailureResult = CodePipeline
        type Rs PutJobFailureResult =
             PutJobFailureResultResponse
        request = postJSON
        response = receiveNull PutJobFailureResultResponse'

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
              ["jobId" .= _pjfrJobId,
               "failureDetails" .= _pjfrFailureDetails]

instance ToPath PutJobFailureResult where
        toPath = const "/"

instance ToQuery PutJobFailureResult where
        toQuery = const mempty

-- | /See:/ 'putJobFailureResultResponse' smart constructor.
data PutJobFailureResultResponse =
    PutJobFailureResultResponse' 
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutJobFailureResultResponse' smart constructor.
putJobFailureResultResponse :: PutJobFailureResultResponse
putJobFailureResultResponse = PutJobFailureResultResponse'
