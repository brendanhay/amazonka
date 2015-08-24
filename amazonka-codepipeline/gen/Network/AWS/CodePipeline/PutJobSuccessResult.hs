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
-- Module      : Network.AWS.CodePipeline.PutJobSuccessResult
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the success of a job as returned to the pipeline by a job
-- worker. Only used for custom actions.
--
-- /See:/ <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_PutJobSuccessResult.html AWS API Reference> for PutJobSuccessResult.
module Network.AWS.CodePipeline.PutJobSuccessResult
    (
    -- * Creating a Request
      putJobSuccessResult
    , PutJobSuccessResult
    -- * Request Lenses
    , pjsrContinuationToken
    , pjsrExecutionDetails
    , pjsrCurrentRevision
    , pjsrJobId

    -- * Destructuring the Response
    , putJobSuccessResultResponse
    , PutJobSuccessResultResponse
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.CodePipeline.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a put job success result action.
--
-- /See:/ 'putJobSuccessResult' smart constructor.
data PutJobSuccessResult = PutJobSuccessResult'
    { _pjsrContinuationToken :: !(Maybe Text)
    , _pjsrExecutionDetails  :: !(Maybe ExecutionDetails)
    , _pjsrCurrentRevision   :: !(Maybe CurrentRevision)
    , _pjsrJobId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutJobSuccessResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pjsrContinuationToken'
--
-- * 'pjsrExecutionDetails'
--
-- * 'pjsrCurrentRevision'
--
-- * 'pjsrJobId'
putJobSuccessResult
    :: Text -- ^ 'pjsrJobId'
    -> PutJobSuccessResult
putJobSuccessResult pJobId_ =
    PutJobSuccessResult'
    { _pjsrContinuationToken = Nothing
    , _pjsrExecutionDetails = Nothing
    , _pjsrCurrentRevision = Nothing
    , _pjsrJobId = pJobId_
    }

-- | A system-generated token, such as a AWS CodeDeploy deployment ID, that
-- the successful job used to complete a job asynchronously.
pjsrContinuationToken :: Lens' PutJobSuccessResult (Maybe Text)
pjsrContinuationToken = lens _pjsrContinuationToken (\ s a -> s{_pjsrContinuationToken = a});

-- | The execution details of the successful job, such as the actions taken
-- by the job worker.
pjsrExecutionDetails :: Lens' PutJobSuccessResult (Maybe ExecutionDetails)
pjsrExecutionDetails = lens _pjsrExecutionDetails (\ s a -> s{_pjsrExecutionDetails = a});

-- | The ID of the current revision of the artifact successfully worked upon
-- by the job.
pjsrCurrentRevision :: Lens' PutJobSuccessResult (Maybe CurrentRevision)
pjsrCurrentRevision = lens _pjsrCurrentRevision (\ s a -> s{_pjsrCurrentRevision = a});

-- | The unique system-generated ID of the job that succeeded. This is the
-- same ID returned from PollForJobs.
pjsrJobId :: Lens' PutJobSuccessResult Text
pjsrJobId = lens _pjsrJobId (\ s a -> s{_pjsrJobId = a});

instance AWSRequest PutJobSuccessResult where
        type Rs PutJobSuccessResult =
             PutJobSuccessResultResponse
        request = postJSON codePipeline
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
              (catMaybes
                 [("continuationToken" .=) <$> _pjsrContinuationToken,
                  ("executionDetails" .=) <$> _pjsrExecutionDetails,
                  ("currentRevision" .=) <$> _pjsrCurrentRevision,
                  Just ("jobId" .= _pjsrJobId)])

instance ToPath PutJobSuccessResult where
        toPath = const "/"

instance ToQuery PutJobSuccessResult where
        toQuery = const mempty

-- | /See:/ 'putJobSuccessResultResponse' smart constructor.
data PutJobSuccessResultResponse =
    PutJobSuccessResultResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutJobSuccessResultResponse' with the minimum fields required to make a request.
--
putJobSuccessResultResponse
    :: PutJobSuccessResultResponse
putJobSuccessResultResponse = PutJobSuccessResultResponse'
