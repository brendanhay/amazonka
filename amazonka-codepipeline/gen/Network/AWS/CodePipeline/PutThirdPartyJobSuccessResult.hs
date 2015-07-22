{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutThirdPartyJobSuccessResult
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Represents the success of a third party job as returned to the pipeline
-- by a job worker. Only used for partner actions.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_PutThirdPartyJobSuccessResult.html>
module Network.AWS.CodePipeline.PutThirdPartyJobSuccessResult
    (
    -- * Request
      PutThirdPartyJobSuccessResult
    -- ** Request constructor
    , putThirdPartyJobSuccessResult
    -- ** Request lenses
    , ptpjsrrqContinuationToken
    , ptpjsrrqExecutionDetails
    , ptpjsrrqCurrentRevision
    , ptpjsrrqJobId
    , ptpjsrrqClientToken

    -- * Response
    , PutThirdPartyJobSuccessResultResponse
    -- ** Response constructor
    , putThirdPartyJobSuccessResultResponse
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a put third party job success result action.
--
-- /See:/ 'putThirdPartyJobSuccessResult' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptpjsrrqContinuationToken'
--
-- * 'ptpjsrrqExecutionDetails'
--
-- * 'ptpjsrrqCurrentRevision'
--
-- * 'ptpjsrrqJobId'
--
-- * 'ptpjsrrqClientToken'
data PutThirdPartyJobSuccessResult = PutThirdPartyJobSuccessResult'
    { _ptpjsrrqContinuationToken :: !(Maybe Text)
    , _ptpjsrrqExecutionDetails  :: !(Maybe ExecutionDetails)
    , _ptpjsrrqCurrentRevision   :: !(Maybe CurrentRevision)
    , _ptpjsrrqJobId             :: !Text
    , _ptpjsrrqClientToken       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutThirdPartyJobSuccessResult' smart constructor.
putThirdPartyJobSuccessResult :: Text -> Text -> PutThirdPartyJobSuccessResult
putThirdPartyJobSuccessResult pJobId pClientToken =
    PutThirdPartyJobSuccessResult'
    { _ptpjsrrqContinuationToken = Nothing
    , _ptpjsrrqExecutionDetails = Nothing
    , _ptpjsrrqCurrentRevision = Nothing
    , _ptpjsrrqJobId = pJobId
    , _ptpjsrrqClientToken = pClientToken
    }

-- | A system-generated token, such as a AWS CodeDeploy deployment ID, that a
-- job uses in order to continue the job asynchronously.
ptpjsrrqContinuationToken :: Lens' PutThirdPartyJobSuccessResult (Maybe Text)
ptpjsrrqContinuationToken = lens _ptpjsrrqContinuationToken (\ s a -> s{_ptpjsrrqContinuationToken = a});

-- | FIXME: Undocumented member.
ptpjsrrqExecutionDetails :: Lens' PutThirdPartyJobSuccessResult (Maybe ExecutionDetails)
ptpjsrrqExecutionDetails = lens _ptpjsrrqExecutionDetails (\ s a -> s{_ptpjsrrqExecutionDetails = a});

-- | FIXME: Undocumented member.
ptpjsrrqCurrentRevision :: Lens' PutThirdPartyJobSuccessResult (Maybe CurrentRevision)
ptpjsrrqCurrentRevision = lens _ptpjsrrqCurrentRevision (\ s a -> s{_ptpjsrrqCurrentRevision = a});

-- | The ID of the job that successfully completed. This is the same ID
-- returned from PollForThirdPartyJobs.
ptpjsrrqJobId :: Lens' PutThirdPartyJobSuccessResult Text
ptpjsrrqJobId = lens _ptpjsrrqJobId (\ s a -> s{_ptpjsrrqJobId = a});

-- | The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
ptpjsrrqClientToken :: Lens' PutThirdPartyJobSuccessResult Text
ptpjsrrqClientToken = lens _ptpjsrrqClientToken (\ s a -> s{_ptpjsrrqClientToken = a});

instance AWSRequest PutThirdPartyJobSuccessResult
         where
        type Sv PutThirdPartyJobSuccessResult = CodePipeline
        type Rs PutThirdPartyJobSuccessResult =
             PutThirdPartyJobSuccessResultResponse
        request = postJSON
        response
          = receiveNull PutThirdPartyJobSuccessResultResponse'

instance ToHeaders PutThirdPartyJobSuccessResult
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.PutThirdPartyJobSuccessResult"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutThirdPartyJobSuccessResult where
        toJSON PutThirdPartyJobSuccessResult'{..}
          = object
              ["continuationToken" .= _ptpjsrrqContinuationToken,
               "executionDetails" .= _ptpjsrrqExecutionDetails,
               "currentRevision" .= _ptpjsrrqCurrentRevision,
               "jobId" .= _ptpjsrrqJobId,
               "clientToken" .= _ptpjsrrqClientToken]

instance ToPath PutThirdPartyJobSuccessResult where
        toPath = const "/"

instance ToQuery PutThirdPartyJobSuccessResult where
        toQuery = const mempty

-- | /See:/ 'putThirdPartyJobSuccessResultResponse' smart constructor.
data PutThirdPartyJobSuccessResultResponse =
    PutThirdPartyJobSuccessResultResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutThirdPartyJobSuccessResultResponse' smart constructor.
putThirdPartyJobSuccessResultResponse :: PutThirdPartyJobSuccessResultResponse
putThirdPartyJobSuccessResultResponse = PutThirdPartyJobSuccessResultResponse'
