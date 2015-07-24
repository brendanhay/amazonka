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
    , ptpjsrContinuationToken
    , ptpjsrExecutionDetails
    , ptpjsrCurrentRevision
    , ptpjsrJobId
    , ptpjsrClientToken

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
-- * 'ptpjsrContinuationToken'
--
-- * 'ptpjsrExecutionDetails'
--
-- * 'ptpjsrCurrentRevision'
--
-- * 'ptpjsrJobId'
--
-- * 'ptpjsrClientToken'
data PutThirdPartyJobSuccessResult = PutThirdPartyJobSuccessResult'
    { _ptpjsrContinuationToken :: !(Maybe Text)
    , _ptpjsrExecutionDetails  :: !(Maybe ExecutionDetails)
    , _ptpjsrCurrentRevision   :: !(Maybe CurrentRevision)
    , _ptpjsrJobId             :: !Text
    , _ptpjsrClientToken       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutThirdPartyJobSuccessResult' smart constructor.
putThirdPartyJobSuccessResult :: Text -> Text -> PutThirdPartyJobSuccessResult
putThirdPartyJobSuccessResult pJobId_ pClientToken_ =
    PutThirdPartyJobSuccessResult'
    { _ptpjsrContinuationToken = Nothing
    , _ptpjsrExecutionDetails = Nothing
    , _ptpjsrCurrentRevision = Nothing
    , _ptpjsrJobId = pJobId_
    , _ptpjsrClientToken = pClientToken_
    }

-- | A system-generated token, such as a AWS CodeDeploy deployment ID, that a
-- job uses in order to continue the job asynchronously.
ptpjsrContinuationToken :: Lens' PutThirdPartyJobSuccessResult (Maybe Text)
ptpjsrContinuationToken = lens _ptpjsrContinuationToken (\ s a -> s{_ptpjsrContinuationToken = a});

-- | FIXME: Undocumented member.
ptpjsrExecutionDetails :: Lens' PutThirdPartyJobSuccessResult (Maybe ExecutionDetails)
ptpjsrExecutionDetails = lens _ptpjsrExecutionDetails (\ s a -> s{_ptpjsrExecutionDetails = a});

-- | FIXME: Undocumented member.
ptpjsrCurrentRevision :: Lens' PutThirdPartyJobSuccessResult (Maybe CurrentRevision)
ptpjsrCurrentRevision = lens _ptpjsrCurrentRevision (\ s a -> s{_ptpjsrCurrentRevision = a});

-- | The ID of the job that successfully completed. This is the same ID
-- returned from PollForThirdPartyJobs.
ptpjsrJobId :: Lens' PutThirdPartyJobSuccessResult Text
ptpjsrJobId = lens _ptpjsrJobId (\ s a -> s{_ptpjsrJobId = a});

-- | The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
ptpjsrClientToken :: Lens' PutThirdPartyJobSuccessResult Text
ptpjsrClientToken = lens _ptpjsrClientToken (\ s a -> s{_ptpjsrClientToken = a});

instance AWSRequest PutThirdPartyJobSuccessResult
         where
        type Sv PutThirdPartyJobSuccessResult = CodePipeline
        type Rs PutThirdPartyJobSuccessResult =
             PutThirdPartyJobSuccessResultResponse
        request = postJSON "PutThirdPartyJobSuccessResult"
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
              ["continuationToken" .= _ptpjsrContinuationToken,
               "executionDetails" .= _ptpjsrExecutionDetails,
               "currentRevision" .= _ptpjsrCurrentRevision,
               "jobId" .= _ptpjsrJobId,
               "clientToken" .= _ptpjsrClientToken]

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
