{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.AcknowledgeJob
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified job and whether that job has been
-- received by the job worker. Only used for custom actions.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_AcknowledgeJob.html>
module Network.AWS.CodePipeline.AcknowledgeJob
    (
    -- * Request
      AcknowledgeJob
    -- ** Request constructor
    , acknowledgeJob
    -- ** Request lenses
    , ajJobId
    , ajNonce

    -- * Response
    , AcknowledgeJobResponse
    -- ** Response constructor
    , acknowledgeJobResponse
    -- ** Response lenses
    , ajrStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an acknowledge job action.
--
-- /See:/ 'acknowledgeJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ajJobId'
--
-- * 'ajNonce'
data AcknowledgeJob = AcknowledgeJob'
    { _ajJobId :: !Text
    , _ajNonce :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AcknowledgeJob' smart constructor.
acknowledgeJob :: Text -> Text -> AcknowledgeJob
acknowledgeJob pJobId pNonce =
    AcknowledgeJob'
    { _ajJobId = pJobId
    , _ajNonce = pNonce
    }

-- | The unique system-generated ID of the job for which you want to confirm
-- receipt.
ajJobId :: Lens' AcknowledgeJob Text
ajJobId = lens _ajJobId (\ s a -> s{_ajJobId = a});

-- | A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. This number must
-- be returned in the response.
ajNonce :: Lens' AcknowledgeJob Text
ajNonce = lens _ajNonce (\ s a -> s{_ajNonce = a});

instance AWSRequest AcknowledgeJob where
        type Sv AcknowledgeJob = CodePipeline
        type Rs AcknowledgeJob = AcknowledgeJobResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 AcknowledgeJobResponse' <$> (pure (fromEnum s)))

instance ToHeaders AcknowledgeJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.AcknowledgeJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AcknowledgeJob where
        toJSON AcknowledgeJob'{..}
          = object ["jobId" .= _ajJobId, "nonce" .= _ajNonce]

instance ToPath AcknowledgeJob where
        toPath = const "/"

instance ToQuery AcknowledgeJob where
        toQuery = const mempty

-- | Represents the output of an acknowledge job action.
--
-- /See:/ 'acknowledgeJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ajrStatus'
newtype AcknowledgeJobResponse = AcknowledgeJobResponse'
    { _ajrStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AcknowledgeJobResponse' smart constructor.
acknowledgeJobResponse :: Int -> AcknowledgeJobResponse
acknowledgeJobResponse pStatus =
    AcknowledgeJobResponse'
    { _ajrStatus = pStatus
    }

-- | FIXME: Undocumented member.
ajrStatus :: Lens' AcknowledgeJobResponse Int
ajrStatus = lens _ajrStatus (\ s a -> s{_ajrStatus = a});
