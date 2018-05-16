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
-- Module      : Network.AWS.CodePipeline.AcknowledgeThirdPartyJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms a job worker has received the specified job. Only used for partner actions.
--
--
module Network.AWS.CodePipeline.AcknowledgeThirdPartyJob
    (
    -- * Creating a Request
      acknowledgeThirdPartyJob
    , AcknowledgeThirdPartyJob
    -- * Request Lenses
    , atpjJobId
    , atpjNonce
    , atpjClientToken

    -- * Destructuring the Response
    , acknowledgeThirdPartyJobResponse
    , AcknowledgeThirdPartyJobResponse
    -- * Response Lenses
    , atpjrsStatus
    , atpjrsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of an AcknowledgeThirdPartyJob action.
--
--
--
-- /See:/ 'acknowledgeThirdPartyJob' smart constructor.
data AcknowledgeThirdPartyJob = AcknowledgeThirdPartyJob'
  { _atpjJobId       :: !Text
  , _atpjNonce       :: !Text
  , _atpjClientToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcknowledgeThirdPartyJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atpjJobId' - The unique system-generated ID of the job.
--
-- * 'atpjNonce' - A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response to a 'GetThirdPartyJobDetails' request.
--
-- * 'atpjClientToken' - The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
acknowledgeThirdPartyJob
    :: Text -- ^ 'atpjJobId'
    -> Text -- ^ 'atpjNonce'
    -> Text -- ^ 'atpjClientToken'
    -> AcknowledgeThirdPartyJob
acknowledgeThirdPartyJob pJobId_ pNonce_ pClientToken_ =
  AcknowledgeThirdPartyJob'
    { _atpjJobId = pJobId_
    , _atpjNonce = pNonce_
    , _atpjClientToken = pClientToken_
    }


-- | The unique system-generated ID of the job.
atpjJobId :: Lens' AcknowledgeThirdPartyJob Text
atpjJobId = lens _atpjJobId (\ s a -> s{_atpjJobId = a})

-- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response to a 'GetThirdPartyJobDetails' request.
atpjNonce :: Lens' AcknowledgeThirdPartyJob Text
atpjNonce = lens _atpjNonce (\ s a -> s{_atpjNonce = a})

-- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
atpjClientToken :: Lens' AcknowledgeThirdPartyJob Text
atpjClientToken = lens _atpjClientToken (\ s a -> s{_atpjClientToken = a})

instance AWSRequest AcknowledgeThirdPartyJob where
        type Rs AcknowledgeThirdPartyJob =
             AcknowledgeThirdPartyJobResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 AcknowledgeThirdPartyJobResponse' <$>
                   (x .?> "status") <*> (pure (fromEnum s)))

instance Hashable AcknowledgeThirdPartyJob where

instance NFData AcknowledgeThirdPartyJob where

instance ToHeaders AcknowledgeThirdPartyJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.AcknowledgeThirdPartyJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AcknowledgeThirdPartyJob where
        toJSON AcknowledgeThirdPartyJob'{..}
          = object
              (catMaybes
                 [Just ("jobId" .= _atpjJobId),
                  Just ("nonce" .= _atpjNonce),
                  Just ("clientToken" .= _atpjClientToken)])

instance ToPath AcknowledgeThirdPartyJob where
        toPath = const "/"

instance ToQuery AcknowledgeThirdPartyJob where
        toQuery = const mempty

-- | Represents the output of an AcknowledgeThirdPartyJob action.
--
--
--
-- /See:/ 'acknowledgeThirdPartyJobResponse' smart constructor.
data AcknowledgeThirdPartyJobResponse = AcknowledgeThirdPartyJobResponse'
  { _atpjrsStatus         :: !(Maybe JobStatus)
  , _atpjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcknowledgeThirdPartyJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atpjrsStatus' - The status information for the third party job, if any.
--
-- * 'atpjrsResponseStatus' - -- | The response status code.
acknowledgeThirdPartyJobResponse
    :: Int -- ^ 'atpjrsResponseStatus'
    -> AcknowledgeThirdPartyJobResponse
acknowledgeThirdPartyJobResponse pResponseStatus_ =
  AcknowledgeThirdPartyJobResponse'
    {_atpjrsStatus = Nothing, _atpjrsResponseStatus = pResponseStatus_}


-- | The status information for the third party job, if any.
atpjrsStatus :: Lens' AcknowledgeThirdPartyJobResponse (Maybe JobStatus)
atpjrsStatus = lens _atpjrsStatus (\ s a -> s{_atpjrsStatus = a})

-- | -- | The response status code.
atpjrsResponseStatus :: Lens' AcknowledgeThirdPartyJobResponse Int
atpjrsResponseStatus = lens _atpjrsResponseStatus (\ s a -> s{_atpjrsResponseStatus = a})

instance NFData AcknowledgeThirdPartyJobResponse
         where
