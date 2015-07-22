{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a third party job as returned to the pipeline
-- by a job worker. Only used for partner actions.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_PutThirdPartyJobFailureResult.html>
module Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
    (
    -- * Request
      PutThirdPartyJobFailureResult
    -- ** Request constructor
    , putThirdPartyJobFailureResult
    -- ** Request lenses
    , ptpjfrrqJobId
    , ptpjfrrqClientToken
    , ptpjfrrqFailureDetails

    -- * Response
    , PutThirdPartyJobFailureResultResponse
    -- ** Response constructor
    , putThirdPartyJobFailureResultResponse
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a third party job failure result action.
--
-- /See:/ 'putThirdPartyJobFailureResult' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptpjfrrqJobId'
--
-- * 'ptpjfrrqClientToken'
--
-- * 'ptpjfrrqFailureDetails'
data PutThirdPartyJobFailureResult = PutThirdPartyJobFailureResult'
    { _ptpjfrrqJobId          :: !Text
    , _ptpjfrrqClientToken    :: !Text
    , _ptpjfrrqFailureDetails :: !FailureDetails
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutThirdPartyJobFailureResult' smart constructor.
putThirdPartyJobFailureResult :: Text -> Text -> FailureDetails -> PutThirdPartyJobFailureResult
putThirdPartyJobFailureResult pJobId pClientToken pFailureDetails =
    PutThirdPartyJobFailureResult'
    { _ptpjfrrqJobId = pJobId
    , _ptpjfrrqClientToken = pClientToken
    , _ptpjfrrqFailureDetails = pFailureDetails
    }

-- | The ID of the job that failed. This is the same ID returned from
-- PollForThirdPartyJobs.
ptpjfrrqJobId :: Lens' PutThirdPartyJobFailureResult Text
ptpjfrrqJobId = lens _ptpjfrrqJobId (\ s a -> s{_ptpjfrrqJobId = a});

-- | The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
ptpjfrrqClientToken :: Lens' PutThirdPartyJobFailureResult Text
ptpjfrrqClientToken = lens _ptpjfrrqClientToken (\ s a -> s{_ptpjfrrqClientToken = a});

-- | FIXME: Undocumented member.
ptpjfrrqFailureDetails :: Lens' PutThirdPartyJobFailureResult FailureDetails
ptpjfrrqFailureDetails = lens _ptpjfrrqFailureDetails (\ s a -> s{_ptpjfrrqFailureDetails = a});

instance AWSRequest PutThirdPartyJobFailureResult
         where
        type Sv PutThirdPartyJobFailureResult = CodePipeline
        type Rs PutThirdPartyJobFailureResult =
             PutThirdPartyJobFailureResultResponse
        request = postJSON
        response
          = receiveNull PutThirdPartyJobFailureResultResponse'

instance ToHeaders PutThirdPartyJobFailureResult
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.PutThirdPartyJobFailureResult"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutThirdPartyJobFailureResult where
        toJSON PutThirdPartyJobFailureResult'{..}
          = object
              ["jobId" .= _ptpjfrrqJobId,
               "clientToken" .= _ptpjfrrqClientToken,
               "failureDetails" .= _ptpjfrrqFailureDetails]

instance ToPath PutThirdPartyJobFailureResult where
        toPath = const "/"

instance ToQuery PutThirdPartyJobFailureResult where
        toQuery = const mempty

-- | /See:/ 'putThirdPartyJobFailureResultResponse' smart constructor.
data PutThirdPartyJobFailureResultResponse =
    PutThirdPartyJobFailureResultResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutThirdPartyJobFailureResultResponse' smart constructor.
putThirdPartyJobFailureResultResponse :: PutThirdPartyJobFailureResultResponse
putThirdPartyJobFailureResultResponse = PutThirdPartyJobFailureResultResponse'
