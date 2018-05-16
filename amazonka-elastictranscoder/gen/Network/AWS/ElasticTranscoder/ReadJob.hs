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
-- Module      : Network.AWS.ElasticTranscoder.ReadJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ReadJob operation returns detailed information about a job.
--
--
module Network.AWS.ElasticTranscoder.ReadJob
    (
    -- * Creating a Request
      readJob
    , ReadJob
    -- * Request Lenses
    , rjId

    -- * Destructuring the Response
    , readJobResponse
    , ReadJobResponse
    -- * Response Lenses
    , rjrsResponseStatus
    , rjrsJob
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @ReadJobRequest@ structure.
--
--
--
-- /See:/ 'readJob' smart constructor.
newtype ReadJob = ReadJob'
  { _rjId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReadJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rjId' - The identifier of the job for which you want to get detailed information.
readJob
    :: Text -- ^ 'rjId'
    -> ReadJob
readJob pId_ = ReadJob' {_rjId = pId_}


-- | The identifier of the job for which you want to get detailed information.
rjId :: Lens' ReadJob Text
rjId = lens _rjId (\ s a -> s{_rjId = a})

instance AWSRequest ReadJob where
        type Rs ReadJob = ReadJobResponse
        request = get elasticTranscoder
        response
          = receiveJSON
              (\ s h x ->
                 ReadJobResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Job"))

instance Hashable ReadJob where

instance NFData ReadJob where

instance ToHeaders ReadJob where
        toHeaders = const mempty

instance ToPath ReadJob where
        toPath ReadJob'{..}
          = mconcat ["/2012-09-25/jobs/", toBS _rjId]

instance ToQuery ReadJob where
        toQuery = const mempty

-- | The @ReadJobResponse@ structure.
--
--
--
-- /See:/ 'readJobResponse' smart constructor.
data ReadJobResponse = ReadJobResponse'
  { _rjrsResponseStatus :: !Int
  , _rjrsJob            :: !Job'
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReadJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rjrsResponseStatus' - -- | The response status code.
--
-- * 'rjrsJob' - A section of the response body that provides information about the job.
readJobResponse
    :: Int -- ^ 'rjrsResponseStatus'
    -> Job' -- ^ 'rjrsJob'
    -> ReadJobResponse
readJobResponse pResponseStatus_ pJob_ =
  ReadJobResponse' {_rjrsResponseStatus = pResponseStatus_, _rjrsJob = pJob_}


-- | -- | The response status code.
rjrsResponseStatus :: Lens' ReadJobResponse Int
rjrsResponseStatus = lens _rjrsResponseStatus (\ s a -> s{_rjrsResponseStatus = a})

-- | A section of the response body that provides information about the job.
rjrsJob :: Lens' ReadJobResponse Job'
rjrsJob = lens _rjrsJob (\ s a -> s{_rjrsJob = a})

instance NFData ReadJobResponse where
