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
-- Module      : Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a third party job as returned to the pipeline by a job worker. Only used for partner actions.
--
--
module Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
    (
    -- * Creating a Request
      putThirdPartyJobFailureResult
    , PutThirdPartyJobFailureResult
    -- * Request Lenses
    , ptpjfrJobId
    , ptpjfrClientToken
    , ptpjfrFailureDetails

    -- * Destructuring the Response
    , putThirdPartyJobFailureResultResponse
    , PutThirdPartyJobFailureResultResponse
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a PutThirdPartyJobFailureResult action.
--
--
--
-- /See:/ 'putThirdPartyJobFailureResult' smart constructor.
data PutThirdPartyJobFailureResult = PutThirdPartyJobFailureResult'
  { _ptpjfrJobId          :: !Text
  , _ptpjfrClientToken    :: !Text
  , _ptpjfrFailureDetails :: !FailureDetails
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutThirdPartyJobFailureResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptpjfrJobId' - The ID of the job that failed. This is the same ID returned from PollForThirdPartyJobs.
--
-- * 'ptpjfrClientToken' - The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
--
-- * 'ptpjfrFailureDetails' - Represents information about failure details.
putThirdPartyJobFailureResult
    :: Text -- ^ 'ptpjfrJobId'
    -> Text -- ^ 'ptpjfrClientToken'
    -> FailureDetails -- ^ 'ptpjfrFailureDetails'
    -> PutThirdPartyJobFailureResult
putThirdPartyJobFailureResult pJobId_ pClientToken_ pFailureDetails_ =
  PutThirdPartyJobFailureResult'
    { _ptpjfrJobId = pJobId_
    , _ptpjfrClientToken = pClientToken_
    , _ptpjfrFailureDetails = pFailureDetails_
    }


-- | The ID of the job that failed. This is the same ID returned from PollForThirdPartyJobs.
ptpjfrJobId :: Lens' PutThirdPartyJobFailureResult Text
ptpjfrJobId = lens _ptpjfrJobId (\ s a -> s{_ptpjfrJobId = a})

-- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
ptpjfrClientToken :: Lens' PutThirdPartyJobFailureResult Text
ptpjfrClientToken = lens _ptpjfrClientToken (\ s a -> s{_ptpjfrClientToken = a})

-- | Represents information about failure details.
ptpjfrFailureDetails :: Lens' PutThirdPartyJobFailureResult FailureDetails
ptpjfrFailureDetails = lens _ptpjfrFailureDetails (\ s a -> s{_ptpjfrFailureDetails = a})

instance AWSRequest PutThirdPartyJobFailureResult
         where
        type Rs PutThirdPartyJobFailureResult =
             PutThirdPartyJobFailureResultResponse
        request = postJSON codePipeline
        response
          = receiveNull PutThirdPartyJobFailureResultResponse'

instance Hashable PutThirdPartyJobFailureResult where

instance NFData PutThirdPartyJobFailureResult where

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
              (catMaybes
                 [Just ("jobId" .= _ptpjfrJobId),
                  Just ("clientToken" .= _ptpjfrClientToken),
                  Just ("failureDetails" .= _ptpjfrFailureDetails)])

instance ToPath PutThirdPartyJobFailureResult where
        toPath = const "/"

instance ToQuery PutThirdPartyJobFailureResult where
        toQuery = const mempty

-- | /See:/ 'putThirdPartyJobFailureResultResponse' smart constructor.
data PutThirdPartyJobFailureResultResponse =
  PutThirdPartyJobFailureResultResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutThirdPartyJobFailureResultResponse' with the minimum fields required to make a request.
--
putThirdPartyJobFailureResultResponse
    :: PutThirdPartyJobFailureResultResponse
putThirdPartyJobFailureResultResponse = PutThirdPartyJobFailureResultResponse'


instance NFData PutThirdPartyJobFailureResultResponse
         where
