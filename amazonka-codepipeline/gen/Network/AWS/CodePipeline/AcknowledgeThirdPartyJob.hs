{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.AcknowledgeThirdPartyJob
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Confirms a job worker has received the specified job. Only used for
-- partner actions.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_AcknowledgeThirdPartyJob.html>
module Network.AWS.CodePipeline.AcknowledgeThirdPartyJob
    (
    -- * Request
      AcknowledgeThirdPartyJob
    -- ** Request constructor
    , acknowledgeThirdPartyJob
    -- ** Request lenses
    , atpjJobId
    , atpjNonce
    , atpjClientToken

    -- * Response
    , AcknowledgeThirdPartyJobResponse
    -- ** Response constructor
    , acknowledgeThirdPartyJobResponse
    -- ** Response lenses
    , atpjrsStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an acknowledge third party job action.
--
-- /See:/ 'acknowledgeThirdPartyJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atpjJobId'
--
-- * 'atpjNonce'
--
-- * 'atpjClientToken'
data AcknowledgeThirdPartyJob = AcknowledgeThirdPartyJob'
    { _atpjJobId       :: !Text
    , _atpjNonce       :: !Text
    , _atpjClientToken :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AcknowledgeThirdPartyJob' smart constructor.
acknowledgeThirdPartyJob :: Text -> Text -> Text -> AcknowledgeThirdPartyJob
acknowledgeThirdPartyJob pJobId_ pNonce_ pClientToken_ =
    AcknowledgeThirdPartyJob'
    { _atpjJobId = pJobId_
    , _atpjNonce = pNonce_
    , _atpjClientToken = pClientToken_
    }

-- | The unique system-generated ID of the job.
atpjJobId :: Lens' AcknowledgeThirdPartyJob Text
atpjJobId = lens _atpjJobId (\ s a -> s{_atpjJobId = a});

-- | A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. This number must
-- be returned in the response.
atpjNonce :: Lens' AcknowledgeThirdPartyJob Text
atpjNonce = lens _atpjNonce (\ s a -> s{_atpjNonce = a});

-- | The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
atpjClientToken :: Lens' AcknowledgeThirdPartyJob Text
atpjClientToken = lens _atpjClientToken (\ s a -> s{_atpjClientToken = a});

instance AWSRequest AcknowledgeThirdPartyJob where
        type Sv AcknowledgeThirdPartyJob = CodePipeline
        type Rs AcknowledgeThirdPartyJob =
             AcknowledgeThirdPartyJobResponse
        request = postJSON "AcknowledgeThirdPartyJob"
        response
          = receiveJSON
              (\ s h x ->
                 AcknowledgeThirdPartyJobResponse' <$>
                   (pure (fromEnum s)))

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
              ["jobId" .= _atpjJobId, "nonce" .= _atpjNonce,
               "clientToken" .= _atpjClientToken]

instance ToPath AcknowledgeThirdPartyJob where
        toPath = const "/"

instance ToQuery AcknowledgeThirdPartyJob where
        toQuery = const mempty

-- | Represents the output of an acknowledge third party job action.
--
-- /See:/ 'acknowledgeThirdPartyJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atpjrsStatus'
newtype AcknowledgeThirdPartyJobResponse = AcknowledgeThirdPartyJobResponse'
    { _atpjrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AcknowledgeThirdPartyJobResponse' smart constructor.
acknowledgeThirdPartyJobResponse :: Int -> AcknowledgeThirdPartyJobResponse
acknowledgeThirdPartyJobResponse pStatus_ =
    AcknowledgeThirdPartyJobResponse'
    { _atpjrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
atpjrsStatus :: Lens' AcknowledgeThirdPartyJobResponse Int
atpjrsStatus = lens _atpjrsStatus (\ s a -> s{_atpjrsStatus = a});
