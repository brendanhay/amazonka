{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutJobFailureResult
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a job as returned to the pipeline by a job
-- worker. Only used for custom actions.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_PutJobFailureResult.html>
module Network.AWS.CodePipeline.PutJobFailureResult
    (
    -- * Request
      PutJobFailureResult
    -- ** Request constructor
    , putJobFailureResult
    -- ** Request lenses
    , pjfrrqJobId
    , pjfrrqFailureDetails

    -- * Response
    , PutJobFailureResultResponse
    -- ** Response constructor
    , putJobFailureResultResponse
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a put job failure result action.
--
-- /See:/ 'putJobFailureResult' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pjfrrqJobId'
--
-- * 'pjfrrqFailureDetails'
data PutJobFailureResult = PutJobFailureResult'
    { _pjfrrqJobId          :: !Text
    , _pjfrrqFailureDetails :: !FailureDetails
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutJobFailureResult' smart constructor.
putJobFailureResult :: Text -> FailureDetails -> PutJobFailureResult
putJobFailureResult pJobId_ pFailureDetails_ =
    PutJobFailureResult'
    { _pjfrrqJobId = pJobId_
    , _pjfrrqFailureDetails = pFailureDetails_
    }

-- | The unique system-generated ID of the job that failed. This is the same
-- ID returned from PollForJobs.
pjfrrqJobId :: Lens' PutJobFailureResult Text
pjfrrqJobId = lens _pjfrrqJobId (\ s a -> s{_pjfrrqJobId = a});

-- | The details about the failure of a job.
pjfrrqFailureDetails :: Lens' PutJobFailureResult FailureDetails
pjfrrqFailureDetails = lens _pjfrrqFailureDetails (\ s a -> s{_pjfrrqFailureDetails = a});

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
              ["jobId" .= _pjfrrqJobId,
               "failureDetails" .= _pjfrrqFailureDetails]

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
