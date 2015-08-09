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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ReadJob operation returns detailed information about a job.
--
-- /See:/ <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ReadJob.html AWS API Reference> for ReadJob.
module Network.AWS.ElasticTranscoder.ReadJob
    (
    -- * Creating a Request
      ReadJob
    , readJob
    -- * Request Lenses
    , rjId

    -- * Destructuring the Response
    , ReadJobResponse
    , readJobResponse
    -- * Response Lenses
    , rjrsStatus
    , rjrsJob
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.ElasticTranscoder.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @ReadJobRequest@ structure.
--
-- /See:/ 'readJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rjId'
newtype ReadJob = ReadJob'
    { _rjId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReadJob' smart constructor.
readJob :: Text -> ReadJob
readJob pId_ =
    ReadJob'
    { _rjId = pId_
    }

-- | The identifier of the job for which you want to get detailed
-- information.
rjId :: Lens' ReadJob Text
rjId = lens _rjId (\ s a -> s{_rjId = a});

instance AWSRequest ReadJob where
        type Sv ReadJob = ElasticTranscoder
        type Rs ReadJob = ReadJobResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ReadJobResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Job"))

instance ToHeaders ReadJob where
        toHeaders = const mempty

instance ToPath ReadJob where
        toPath ReadJob'{..}
          = mconcat ["/2012-09-25/jobs/", toBS _rjId]

instance ToQuery ReadJob where
        toQuery = const mempty

-- | The @ReadJobResponse@ structure.
--
-- /See:/ 'readJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rjrsStatus'
--
-- * 'rjrsJob'
data ReadJobResponse = ReadJobResponse'
    { _rjrsStatus :: !Int
    , _rjrsJob    :: !Job'
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReadJobResponse' smart constructor.
readJobResponse :: Int -> Job' -> ReadJobResponse
readJobResponse pStatus_ pJob_ =
    ReadJobResponse'
    { _rjrsStatus = pStatus_
    , _rjrsJob = pJob_
    }

-- | Undocumented member.
rjrsStatus :: Lens' ReadJobResponse Int
rjrsStatus = lens _rjrsStatus (\ s a -> s{_rjrsStatus = a});

-- | A section of the response body that provides information about the job.
rjrsJob :: Lens' ReadJobResponse Job'
rjrsJob = lens _rjrsJob (\ s a -> s{_rjrsJob = a});
