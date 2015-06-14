{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticTranscoder.CreateJob
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | When you create a job, Elastic Transcoder returns JSON data that
-- includes the values that you specified plus information about the job
-- that is created.
--
-- If you have specified more than one output for your jobs (for example,
-- one output for the Kindle Fire and another output for the Apple iPhone
-- 4s), you currently must use the Elastic Transcoder API to list the jobs
-- (as opposed to the AWS Console).
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/CreateJob.html>
module Network.AWS.ElasticTranscoder.CreateJob
    (
    -- * Request
      CreateJob
    -- ** Request constructor
    , createJob
    -- ** Request lenses
    , cjOutputs
    , cjUserMetadata
    , cjOutput
    , cjPlaylists
    , cjOutputKeyPrefix
    , cjPipelineId
    , cjInput

    -- * Response
    , CreateJobResponse
    -- ** Response constructor
    , createJobResponse
    -- ** Response lenses
    , cjrJob'
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticTranscoder.Types

-- | /See:/ 'createJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjOutputs'
--
-- * 'cjUserMetadata'
--
-- * 'cjOutput'
--
-- * 'cjPlaylists'
--
-- * 'cjOutputKeyPrefix'
--
-- * 'cjPipelineId'
--
-- * 'cjInput'
data CreateJob = CreateJob'{_cjOutputs :: Maybe [CreateJobOutput], _cjUserMetadata :: Maybe (HashMap Text Text), _cjOutput :: Maybe CreateJobOutput, _cjPlaylists :: Maybe [CreateJobPlaylist], _cjOutputKeyPrefix :: Maybe Text, _cjPipelineId :: Text, _cjInput :: JobInput} deriving (Eq, Read, Show)

-- | 'CreateJob' smart constructor.
createJob :: Text -> JobInput -> CreateJob
createJob pPipelineId pInput = CreateJob'{_cjOutputs = Nothing, _cjUserMetadata = Nothing, _cjOutput = Nothing, _cjPlaylists = Nothing, _cjOutputKeyPrefix = Nothing, _cjPipelineId = pPipelineId, _cjInput = pInput};

-- | A section of the request body that provides information about the
-- transcoded (target) files. We recommend that you use the @Outputs@
-- syntax instead of the @Output@ syntax.
cjOutputs :: Lens' CreateJob (Maybe [CreateJobOutput])
cjOutputs = lens _cjOutputs (\ s a -> s{_cjOutputs = a});

-- | User-defined metadata that you want to associate with an Elastic
-- Transcoder job. You specify metadata in @key\/value@ pairs, and you can
-- add up to 10 @key\/value@ pairs per job. Elastic Transcoder does not
-- guarantee that @key\/value@ pairs will be returned in the same order in
-- which you specify them.
cjUserMetadata :: Lens' CreateJob (Maybe (HashMap Text Text))
cjUserMetadata = lens _cjUserMetadata (\ s a -> s{_cjUserMetadata = a}) . mapping _Coerce;

-- | FIXME: Undocumented member.
cjOutput :: Lens' CreateJob (Maybe CreateJobOutput)
cjOutput = lens _cjOutput (\ s a -> s{_cjOutput = a});

-- | If you specify a preset in @PresetId@ for which the value of @Container@
-- is fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information
-- about the master playlists that you want Elastic Transcoder to create.
--
-- The maximum number of master playlists in a job is 30.
cjPlaylists :: Lens' CreateJob (Maybe [CreateJobPlaylist])
cjPlaylists = lens _cjPlaylists (\ s a -> s{_cjPlaylists = a});

-- | The value, if any, that you want Elastic Transcoder to prepend to the
-- names of all files that this job creates, including output files,
-- thumbnails, and playlists.
cjOutputKeyPrefix :: Lens' CreateJob (Maybe Text)
cjOutputKeyPrefix = lens _cjOutputKeyPrefix (\ s a -> s{_cjOutputKeyPrefix = a});

-- | The @Id@ of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the
-- Amazon S3 bucket from which Elastic Transcoder gets the files to
-- transcode and the bucket into which Elastic Transcoder puts the
-- transcoded files.
cjPipelineId :: Lens' CreateJob Text
cjPipelineId = lens _cjPipelineId (\ s a -> s{_cjPipelineId = a});

-- | A section of the request body that provides information about the file
-- that is being transcoded.
cjInput :: Lens' CreateJob JobInput
cjInput = lens _cjInput (\ s a -> s{_cjInput = a});

instance AWSRequest CreateJob where
        type Sv CreateJob = ElasticTranscoder
        type Rs CreateJob = CreateJobResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x -> CreateJobResponse' <$> x .?> "Job'")

instance ToHeaders CreateJob where
        toHeaders = const mempty

instance ToJSON CreateJob where
        toJSON CreateJob'{..}
          = object
              ["Outputs" .= _cjOutputs,
               "UserMetadata" .= _cjUserMetadata,
               "Output" .= _cjOutput, "Playlists" .= _cjPlaylists,
               "OutputKeyPrefix" .= _cjOutputKeyPrefix,
               "PipelineId" .= _cjPipelineId, "Input" .= _cjInput]

instance ToPath CreateJob where
        toPath = const "/2012-09-25/jobs"

instance ToQuery CreateJob where
        toQuery = const mempty

-- | /See:/ 'createJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjrJob''
newtype CreateJobResponse = CreateJobResponse'{_cjrJob' :: Maybe Job'} deriving (Eq, Read, Show)

-- | 'CreateJobResponse' smart constructor.
createJobResponse :: CreateJobResponse
createJobResponse = CreateJobResponse'{_cjrJob' = Nothing};

-- | A section of the response body that provides information about the job
-- that is created.
cjrJob' :: Lens' CreateJobResponse (Maybe Job')
cjrJob' = lens _cjrJob' (\ s a -> s{_cjrJob' = a});
