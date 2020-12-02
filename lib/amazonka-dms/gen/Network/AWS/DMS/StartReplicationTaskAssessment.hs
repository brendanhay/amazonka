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
-- Module      : Network.AWS.DMS.StartReplicationTaskAssessment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the replication task assessment for unsupported data types in the source database.
--
--
module Network.AWS.DMS.StartReplicationTaskAssessment
    (
    -- * Creating a Request
      startReplicationTaskAssessment
    , StartReplicationTaskAssessment
    -- * Request Lenses
    , srtaReplicationTaskARN

    -- * Destructuring the Response
    , startReplicationTaskAssessmentResponse
    , StartReplicationTaskAssessmentResponse
    -- * Response Lenses
    , srtarsReplicationTask
    , srtarsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'startReplicationTaskAssessment' smart constructor.
newtype StartReplicationTaskAssessment = StartReplicationTaskAssessment'
  { _srtaReplicationTaskARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartReplicationTaskAssessment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtaReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
startReplicationTaskAssessment
    :: Text -- ^ 'srtaReplicationTaskARN'
    -> StartReplicationTaskAssessment
startReplicationTaskAssessment pReplicationTaskARN_ =
  StartReplicationTaskAssessment'
    {_srtaReplicationTaskARN = pReplicationTaskARN_}


-- | The Amazon Resource Name (ARN) of the replication task.
srtaReplicationTaskARN :: Lens' StartReplicationTaskAssessment Text
srtaReplicationTaskARN = lens _srtaReplicationTaskARN (\ s a -> s{_srtaReplicationTaskARN = a})

instance AWSRequest StartReplicationTaskAssessment
         where
        type Rs StartReplicationTaskAssessment =
             StartReplicationTaskAssessmentResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 StartReplicationTaskAssessmentResponse' <$>
                   (x .?> "ReplicationTask") <*> (pure (fromEnum s)))

instance Hashable StartReplicationTaskAssessment
         where

instance NFData StartReplicationTaskAssessment where

instance ToHeaders StartReplicationTaskAssessment
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.StartReplicationTaskAssessment"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartReplicationTaskAssessment where
        toJSON StartReplicationTaskAssessment'{..}
          = object
              (catMaybes
                 [Just
                    ("ReplicationTaskArn" .= _srtaReplicationTaskARN)])

instance ToPath StartReplicationTaskAssessment where
        toPath = const "/"

instance ToQuery StartReplicationTaskAssessment where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'startReplicationTaskAssessmentResponse' smart constructor.
data StartReplicationTaskAssessmentResponse = StartReplicationTaskAssessmentResponse'
  { _srtarsReplicationTask :: !(Maybe ReplicationTask)
  , _srtarsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartReplicationTaskAssessmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtarsReplicationTask' - The assessed replication task.
--
-- * 'srtarsResponseStatus' - -- | The response status code.
startReplicationTaskAssessmentResponse
    :: Int -- ^ 'srtarsResponseStatus'
    -> StartReplicationTaskAssessmentResponse
startReplicationTaskAssessmentResponse pResponseStatus_ =
  StartReplicationTaskAssessmentResponse'
    {_srtarsReplicationTask = Nothing, _srtarsResponseStatus = pResponseStatus_}


-- | The assessed replication task.
srtarsReplicationTask :: Lens' StartReplicationTaskAssessmentResponse (Maybe ReplicationTask)
srtarsReplicationTask = lens _srtarsReplicationTask (\ s a -> s{_srtarsReplicationTask = a})

-- | -- | The response status code.
srtarsResponseStatus :: Lens' StartReplicationTaskAssessmentResponse Int
srtarsResponseStatus = lens _srtarsResponseStatus (\ s a -> s{_srtarsResponseStatus = a})

instance NFData
           StartReplicationTaskAssessmentResponse
         where
