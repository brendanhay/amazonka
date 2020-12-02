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
-- Module      : Network.AWS.IoT.CancelJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a job.
--
--
module Network.AWS.IoT.CancelJob
    (
    -- * Creating a Request
      cancelJob
    , CancelJob
    -- * Request Lenses
    , cComment
    , cJobId

    -- * Destructuring the Response
    , cancelJobResponse
    , CancelJobResponse
    -- * Response Lenses
    , crsJobId
    , crsJobARN
    , crsDescription
    , crsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelJob' smart constructor.
data CancelJob = CancelJob'
  { _cComment :: !(Maybe Text)
  , _cJobId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cComment' - An optional comment string describing why the job was canceled.
--
-- * 'cJobId' - The unique identifier you assigned to this job when it was created.
cancelJob
    :: Text -- ^ 'cJobId'
    -> CancelJob
cancelJob pJobId_ = CancelJob' {_cComment = Nothing, _cJobId = pJobId_}


-- | An optional comment string describing why the job was canceled.
cComment :: Lens' CancelJob (Maybe Text)
cComment = lens _cComment (\ s a -> s{_cComment = a})

-- | The unique identifier you assigned to this job when it was created.
cJobId :: Lens' CancelJob Text
cJobId = lens _cJobId (\ s a -> s{_cJobId = a})

instance AWSRequest CancelJob where
        type Rs CancelJob = CancelJobResponse
        request = putJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CancelJobResponse' <$>
                   (x .?> "jobId") <*> (x .?> "jobArn") <*>
                     (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable CancelJob where

instance NFData CancelJob where

instance ToHeaders CancelJob where
        toHeaders = const mempty

instance ToJSON CancelJob where
        toJSON CancelJob'{..}
          = object (catMaybes [("comment" .=) <$> _cComment])

instance ToPath CancelJob where
        toPath CancelJob'{..}
          = mconcat ["/jobs/", toBS _cJobId, "/cancel"]

instance ToQuery CancelJob where
        toQuery = const mempty

-- | /See:/ 'cancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
  { _crsJobId          :: !(Maybe Text)
  , _crsJobARN         :: !(Maybe Text)
  , _crsDescription    :: !(Maybe Text)
  , _crsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsJobId' - The unique identifier you assigned to this job when it was created.
--
-- * 'crsJobARN' - The job ARN.
--
-- * 'crsDescription' - A short text description of the job.
--
-- * 'crsResponseStatus' - -- | The response status code.
cancelJobResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CancelJobResponse
cancelJobResponse pResponseStatus_ =
  CancelJobResponse'
    { _crsJobId = Nothing
    , _crsJobARN = Nothing
    , _crsDescription = Nothing
    , _crsResponseStatus = pResponseStatus_
    }


-- | The unique identifier you assigned to this job when it was created.
crsJobId :: Lens' CancelJobResponse (Maybe Text)
crsJobId = lens _crsJobId (\ s a -> s{_crsJobId = a})

-- | The job ARN.
crsJobARN :: Lens' CancelJobResponse (Maybe Text)
crsJobARN = lens _crsJobARN (\ s a -> s{_crsJobARN = a})

-- | A short text description of the job.
crsDescription :: Lens' CancelJobResponse (Maybe Text)
crsDescription = lens _crsDescription (\ s a -> s{_crsDescription = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CancelJobResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CancelJobResponse where
