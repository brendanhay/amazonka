{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.ReadJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

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
    , rjrJob
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.ElasticTranscoder.Types
import qualified GHC.Exts

newtype ReadJob = ReadJob
    { _rjId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'ReadJob' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rjId' @::@ 'Text'
--
readJob :: Text -- ^ 'rjId'
        -> ReadJob
readJob p1 = ReadJob
    { _rjId = p1
    }

-- | The identifier of the job for which you want to get detailed information.
rjId :: Lens' ReadJob Text
rjId = lens _rjId (\s a -> s { _rjId = a })

newtype ReadJobResponse = ReadJobResponse
    { _rjrJob :: Maybe Job'
    } deriving (Eq, Show, Generic)

-- | 'ReadJobResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rjrJob' @::@ 'Maybe' 'Job''
--
readJobResponse :: ReadJobResponse
readJobResponse = ReadJobResponse
    { _rjrJob = Nothing
    }

-- | A section of the response body that provides information about the job.
rjrJob :: Lens' ReadJobResponse (Maybe Job')
rjrJob = lens _rjrJob (\s a -> s { _rjrJob = a })

instance ToPath ReadJob where
    toPath ReadJob{..} = mconcat
        [ "/2012-09-25/jobs/"
        , toText _rjId
        ]

instance ToQuery ReadJob where
    toQuery = const mempty

instance ToHeaders ReadJob

instance ToJSON ReadJob where
    toJSON = const Null

instance AWSRequest ReadJob where
    type Sv ReadJob = ElasticTranscoder
    type Rs ReadJob = ReadJobResponse

    request  = get
    response = jsonResponse

instance FromJSON ReadJobResponse where
    parseJSON = withObject "ReadJobResponse" $ \o -> ReadJobResponse
        <$> o .: "Job"
