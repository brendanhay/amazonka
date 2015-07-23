{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.CreateJob
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- When you create a job, Elastic Transcoder returns JSON data that
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
    , cjrqOutputs
    , cjrqUserMetadata
    , cjrqOutput
    , cjrqPlaylists
    , cjrqOutputKeyPrefix
    , cjrqPipelineId
    , cjrqInput

    -- * Response
    , CreateJobResponse
    -- ** Response constructor
    , createJobResponse
    -- ** Response lenses
    , cjrsJob
    , cjrsStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @CreateJobRequest@ structure.
--
-- /See:/ 'createJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjrqOutputs'
--
-- * 'cjrqUserMetadata'
--
-- * 'cjrqOutput'
--
-- * 'cjrqPlaylists'
--
-- * 'cjrqOutputKeyPrefix'
--
-- * 'cjrqPipelineId'
--
-- * 'cjrqInput'
data CreateJob = CreateJob'
    { _cjrqOutputs         :: !(Maybe [CreateJobOutput])
    , _cjrqUserMetadata    :: !(Maybe (Map Text Text))
    , _cjrqOutput          :: !(Maybe CreateJobOutput)
    , _cjrqPlaylists       :: !(Maybe [CreateJobPlaylist])
    , _cjrqOutputKeyPrefix :: !(Maybe Text)
    , _cjrqPipelineId      :: !Text
    , _cjrqInput           :: !JobInput
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateJob' smart constructor.
createJob :: Text -> JobInput -> CreateJob
createJob pPipelineId_ pInput_ =
    CreateJob'
    { _cjrqOutputs = Nothing
    , _cjrqUserMetadata = Nothing
    , _cjrqOutput = Nothing
    , _cjrqPlaylists = Nothing
    , _cjrqOutputKeyPrefix = Nothing
    , _cjrqPipelineId = pPipelineId_
    , _cjrqInput = pInput_
    }

-- | A section of the request body that provides information about the
-- transcoded (target) files. We recommend that you use the @Outputs@
-- syntax instead of the @Output@ syntax.
cjrqOutputs :: Lens' CreateJob [CreateJobOutput]
cjrqOutputs = lens _cjrqOutputs (\ s a -> s{_cjrqOutputs = a}) . _Default;

-- | User-defined metadata that you want to associate with an Elastic
-- Transcoder job. You specify metadata in @key\/value@ pairs, and you can
-- add up to 10 @key\/value@ pairs per job. Elastic Transcoder does not
-- guarantee that @key\/value@ pairs will be returned in the same order in
-- which you specify them.
cjrqUserMetadata :: Lens' CreateJob (HashMap Text Text)
cjrqUserMetadata = lens _cjrqUserMetadata (\ s a -> s{_cjrqUserMetadata = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
cjrqOutput :: Lens' CreateJob (Maybe CreateJobOutput)
cjrqOutput = lens _cjrqOutput (\ s a -> s{_cjrqOutput = a});

-- | If you specify a preset in @PresetId@ for which the value of @Container@
-- is fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information
-- about the master playlists that you want Elastic Transcoder to create.
--
-- The maximum number of master playlists in a job is 30.
cjrqPlaylists :: Lens' CreateJob [CreateJobPlaylist]
cjrqPlaylists = lens _cjrqPlaylists (\ s a -> s{_cjrqPlaylists = a}) . _Default;

-- | The value, if any, that you want Elastic Transcoder to prepend to the
-- names of all files that this job creates, including output files,
-- thumbnails, and playlists.
cjrqOutputKeyPrefix :: Lens' CreateJob (Maybe Text)
cjrqOutputKeyPrefix = lens _cjrqOutputKeyPrefix (\ s a -> s{_cjrqOutputKeyPrefix = a});

-- | The @Id@ of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the
-- Amazon S3 bucket from which Elastic Transcoder gets the files to
-- transcode and the bucket into which Elastic Transcoder puts the
-- transcoded files.
cjrqPipelineId :: Lens' CreateJob Text
cjrqPipelineId = lens _cjrqPipelineId (\ s a -> s{_cjrqPipelineId = a});

-- | A section of the request body that provides information about the file
-- that is being transcoded.
cjrqInput :: Lens' CreateJob JobInput
cjrqInput = lens _cjrqInput (\ s a -> s{_cjrqInput = a});

instance AWSRequest CreateJob where
        type Sv CreateJob = ElasticTranscoder
        type Rs CreateJob = CreateJobResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateJobResponse' <$>
                   (x .?> "Job") <*> (pure (fromEnum s)))

instance ToHeaders CreateJob where
        toHeaders = const mempty

instance ToJSON CreateJob where
        toJSON CreateJob'{..}
          = object
              ["Outputs" .= _cjrqOutputs,
               "UserMetadata" .= _cjrqUserMetadata,
               "Output" .= _cjrqOutput,
               "Playlists" .= _cjrqPlaylists,
               "OutputKeyPrefix" .= _cjrqOutputKeyPrefix,
               "PipelineId" .= _cjrqPipelineId,
               "Input" .= _cjrqInput]

instance ToPath CreateJob where
        toPath = const "/2012-09-25/jobs"

instance ToQuery CreateJob where
        toQuery = const mempty

-- | The CreateJobResponse structure.
--
-- /See:/ 'createJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjrsJob'
--
-- * 'cjrsStatus'
data CreateJobResponse = CreateJobResponse'
    { _cjrsJob    :: !(Maybe Job')
    , _cjrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateJobResponse' smart constructor.
createJobResponse :: Int -> CreateJobResponse
createJobResponse pStatus_ =
    CreateJobResponse'
    { _cjrsJob = Nothing
    , _cjrsStatus = pStatus_
    }

-- | A section of the response body that provides information about the job
-- that is created.
cjrsJob :: Lens' CreateJobResponse (Maybe Job')
cjrsJob = lens _cjrsJob (\ s a -> s{_cjrsJob = a});

-- | FIXME: Undocumented member.
cjrsStatus :: Lens' CreateJobResponse Int
cjrsStatus = lens _cjrsStatus (\ s a -> s{_cjrsStatus = a});
