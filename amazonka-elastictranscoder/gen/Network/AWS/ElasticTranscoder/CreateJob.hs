{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElasticTranscoder.CreateJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | When you create a job, Elastic Transcoder returns JSON data that includes
-- the values that you specified plus information about the job that is
-- created. If you have specified more than one output for your jobs (for
-- example, one output for the Kindle Fire and another output for the Apple
-- iPhone 4s), you currently must use the Elastic Transcoder API to list the
-- jobs (as opposed to the AWS Console).
module Network.AWS.ElasticTranscoder.CreateJob
    (
    -- * Request
      CreateJob
    -- ** Request constructor
    , createJob
    -- ** Request lenses
    , cjInput
    , cjOutput
    , cjOutputKeyPrefix
    , cjOutputs
    , cjPipelineId
    , cjPlaylists

    -- * Response
    , CreateJobResponse
    -- ** Response constructor
    , createJobResponse
    -- ** Response lenses
    , cjrJob
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ElasticTranscoder.Types

data CreateJob = CreateJob
    { _cjInput           :: JobInput
    , _cjOutput          :: Maybe CreateJobOutput
    , _cjOutputKeyPrefix :: Maybe Text
    , _cjOutputs         :: [CreateJobOutput]
    , _cjPipelineId      :: Text
    , _cjPlaylists       :: [CreateJobPlaylist]
    } deriving (Eq, Show, Generic)

-- | 'CreateJob' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjInput' @::@ 'JobInput'
--
-- * 'cjOutput' @::@ 'Maybe' 'CreateJobOutput'
--
-- * 'cjOutputKeyPrefix' @::@ 'Maybe' 'Text'
--
-- * 'cjOutputs' @::@ ['CreateJobOutput']
--
-- * 'cjPipelineId' @::@ 'Text'
--
-- * 'cjPlaylists' @::@ ['CreateJobPlaylist']
--
createJob :: Text -- ^ 'cjPipelineId'
          -> JobInput -- ^ 'cjInput'
          -> CreateJob
createJob p1 p2 = CreateJob
    { _cjPipelineId      = p1
    , _cjInput           = p2
    , _cjOutput          = Nothing
    , _cjOutputs         = mempty
    , _cjOutputKeyPrefix = Nothing
    , _cjPlaylists       = mempty
    }

-- | A section of the request body that provides information about the file
-- that is being transcoded.
cjInput :: Lens' CreateJob JobInput
cjInput = lens _cjInput (\s a -> s { _cjInput = a })

cjOutput :: Lens' CreateJob (Maybe CreateJobOutput)
cjOutput = lens _cjOutput (\s a -> s { _cjOutput = a })

-- | The value, if any, that you want Elastic Transcoder to prepend to the
-- names of all files that this job creates, including output files,
-- thumbnails, and playlists.
cjOutputKeyPrefix :: Lens' CreateJob (Maybe Text)
cjOutputKeyPrefix =
    lens _cjOutputKeyPrefix (\s a -> s { _cjOutputKeyPrefix = a })

-- | A section of the request body that provides information about the
-- transcoded (target) files. We recommend that you use the Outputs syntax
-- instead of the Output syntax.
cjOutputs :: Lens' CreateJob [CreateJobOutput]
cjOutputs = lens _cjOutputs (\s a -> s { _cjOutputs = a })

-- | The Id of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the
-- Amazon S3 bucket from which Elastic Transcoder gets the files to
-- transcode and the bucket into which Elastic Transcoder puts the
-- transcoded files.
cjPipelineId :: Lens' CreateJob Text
cjPipelineId = lens _cjPipelineId (\s a -> s { _cjPipelineId = a })

-- | If you specify a preset in PresetId for which the value of Container is
-- fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information
-- about the master playlists that you want Elastic Transcoder to create.
-- The maximum number of master playlists in a job is 30.
cjPlaylists :: Lens' CreateJob [CreateJobPlaylist]
cjPlaylists = lens _cjPlaylists (\s a -> s { _cjPlaylists = a })

instance ToPath CreateJob where
    toPath = const "/2012-09-25/jobs"

instance ToQuery CreateJob where
    toQuery = const mempty

instance ToHeaders CreateJob

instance ToBody CreateJob where
    toBody = toBody . encode . _cjPipelineId

newtype CreateJobResponse = CreateJobResponse
    { _cjrJob :: Maybe Job
    } deriving (Eq, Show, Generic)

-- | 'CreateJobResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjrJob' @::@ 'Maybe' 'Job'
--
createJobResponse :: CreateJobResponse
createJobResponse = CreateJobResponse
    { _cjrJob = Nothing
    }

-- | A section of the response body that provides information about the job
-- that is created.
cjrJob :: Lens' CreateJobResponse (Maybe Job)
cjrJob = lens _cjrJob (\s a -> s { _cjrJob = a })

-- FromJSON

instance AWSRequest CreateJob where
    type Sv CreateJob = ElasticTranscoder
    type Rs CreateJob = CreateJobResponse

    request  = post'
    response = jsonResponse $ \h o -> CreateJobResponse
        <$> o .: "Job"
