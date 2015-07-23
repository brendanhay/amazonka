{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutJobSuccessResult
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Represents the success of a job as returned to the pipeline by a job
-- worker. Only used for custom actions.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_PutJobSuccessResult.html>
module Network.AWS.CodePipeline.PutJobSuccessResult
    (
    -- * Request
      PutJobSuccessResult
    -- ** Request constructor
    , putJobSuccessResult
    -- ** Request lenses
    , pjsrrqContinuationToken
    , pjsrrqExecutionDetails
    , pjsrrqCurrentRevision
    , pjsrrqJobId

    -- * Response
    , PutJobSuccessResultResponse
    -- ** Response constructor
    , putJobSuccessResultResponse
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a put job success result action.
--
-- /See:/ 'putJobSuccessResult' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pjsrrqContinuationToken'
--
-- * 'pjsrrqExecutionDetails'
--
-- * 'pjsrrqCurrentRevision'
--
-- * 'pjsrrqJobId'
data PutJobSuccessResult = PutJobSuccessResult'
    { _pjsrrqContinuationToken :: !(Maybe Text)
    , _pjsrrqExecutionDetails  :: !(Maybe ExecutionDetails)
    , _pjsrrqCurrentRevision   :: !(Maybe CurrentRevision)
    , _pjsrrqJobId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutJobSuccessResult' smart constructor.
putJobSuccessResult :: Text -> PutJobSuccessResult
putJobSuccessResult pJobId_ =
    PutJobSuccessResult'
    { _pjsrrqContinuationToken = Nothing
    , _pjsrrqExecutionDetails = Nothing
    , _pjsrrqCurrentRevision = Nothing
    , _pjsrrqJobId = pJobId_
    }

-- | A system-generated token, such as a AWS CodeDeploy deployment ID, that
-- the successful job used to complete a job asynchronously.
pjsrrqContinuationToken :: Lens' PutJobSuccessResult (Maybe Text)
pjsrrqContinuationToken = lens _pjsrrqContinuationToken (\ s a -> s{_pjsrrqContinuationToken = a});

-- | The execution details of the successful job, such as the actions taken
-- by the job worker.
pjsrrqExecutionDetails :: Lens' PutJobSuccessResult (Maybe ExecutionDetails)
pjsrrqExecutionDetails = lens _pjsrrqExecutionDetails (\ s a -> s{_pjsrrqExecutionDetails = a});

-- | The ID of the current revision of the artifact successfully worked upon
-- by the job.
pjsrrqCurrentRevision :: Lens' PutJobSuccessResult (Maybe CurrentRevision)
pjsrrqCurrentRevision = lens _pjsrrqCurrentRevision (\ s a -> s{_pjsrrqCurrentRevision = a});

-- | The unique system-generated ID of the job that succeeded. This is the
-- same ID returned from PollForJobs.
pjsrrqJobId :: Lens' PutJobSuccessResult Text
pjsrrqJobId = lens _pjsrrqJobId (\ s a -> s{_pjsrrqJobId = a});

instance AWSRequest PutJobSuccessResult where
        type Sv PutJobSuccessResult = CodePipeline
        type Rs PutJobSuccessResult =
             PutJobSuccessResultResponse
        request = postJSON
        response = receiveNull PutJobSuccessResultResponse'

instance ToHeaders PutJobSuccessResult where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.PutJobSuccessResult" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutJobSuccessResult where
        toJSON PutJobSuccessResult'{..}
          = object
              ["continuationToken" .= _pjsrrqContinuationToken,
               "executionDetails" .= _pjsrrqExecutionDetails,
               "currentRevision" .= _pjsrrqCurrentRevision,
               "jobId" .= _pjsrrqJobId]

instance ToPath PutJobSuccessResult where
        toPath = const "/"

instance ToQuery PutJobSuccessResult where
        toQuery = const mempty

-- | /See:/ 'putJobSuccessResultResponse' smart constructor.
data PutJobSuccessResultResponse =
    PutJobSuccessResultResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutJobSuccessResultResponse' smart constructor.
putJobSuccessResultResponse :: PutJobSuccessResultResponse
putJobSuccessResultResponse = PutJobSuccessResultResponse'
