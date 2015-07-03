{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElasticTranscoder.ReadJob
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The ReadJob operation returns detailed information about a job.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ReadJob.html>
module Network.AWS.ElasticTranscoder.ReadJob
    (
    -- * Request
      ReadJob
    -- ** Request constructor
    , readJob
    -- ** Request lenses
    , rjId

    -- * Response
    , ReadJobResponse
    -- ** Response constructor
    , readJobResponse
    -- ** Response lenses
    , rjrStatus
    , rjrJob
    ) where

import           Network.AWS.ElasticTranscoder.Types
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
    } deriving (Eq,Read,Show)

-- | 'ReadJob' smart constructor.
readJob :: Text -> ReadJob
readJob pId =
    ReadJob'
    { _rjId = pId
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
          = mconcat ["/2012-09-25/jobs/", toText _rjId]

instance ToQuery ReadJob where
        toQuery = const mempty

-- | The @ReadJobResponse@ structure.
--
-- /See:/ 'readJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rjrStatus'
--
-- * 'rjrJob'
data ReadJobResponse = ReadJobResponse'
    { _rjrStatus :: !Int
    , _rjrJob    :: !Job'
    } deriving (Eq,Read,Show)

-- | 'ReadJobResponse' smart constructor.
readJobResponse :: Int -> Job' -> ReadJobResponse
readJobResponse pStatus pJob =
    ReadJobResponse'
    { _rjrStatus = pStatus
    , _rjrJob = pJob
    }

-- | FIXME: Undocumented member.
rjrStatus :: Lens' ReadJobResponse Int
rjrStatus = lens _rjrStatus (\ s a -> s{_rjrStatus = a});

-- | A section of the response body that provides information about the job.
rjrJob :: Lens' ReadJobResponse Job'
rjrJob = lens _rjrJob (\ s a -> s{_rjrJob = a});
