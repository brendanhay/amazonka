{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticTranscoder.ReadPipeline
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

-- | The ReadPipeline operation gets detailed information about a pipeline.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ReadPipeline.html>
module Network.AWS.ElasticTranscoder.ReadPipeline
    (
    -- * Request
      ReadPipeline
    -- ** Request constructor
    , readPipeline
    -- ** Request lenses
    , reaId

    -- * Response
    , ReadPipelineResponse
    -- ** Response constructor
    , readPipelineResponse
    -- ** Response lenses
    , rprWarnings
    , rprPipeline
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticTranscoder.Types

-- | /See:/ 'readPipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reaId'
newtype ReadPipeline = ReadPipeline'{_reaId :: Text} deriving (Eq, Read, Show)

-- | 'ReadPipeline' smart constructor.
readPipeline :: Text -> ReadPipeline
readPipeline pId = ReadPipeline'{_reaId = pId};

-- | The identifier of the pipeline to read.
reaId :: Lens' ReadPipeline Text
reaId = lens _reaId (\ s a -> s{_reaId = a});

instance AWSRequest ReadPipeline where
        type Sv ReadPipeline = ElasticTranscoder
        type Rs ReadPipeline = ReadPipelineResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ReadPipelineResponse' <$>
                   x .?> "Warnings" .!@ mempty <*> x .?> "Pipeline")

instance ToHeaders ReadPipeline where
        toHeaders = const mempty

instance ToPath ReadPipeline where
        toPath ReadPipeline'{..}
          = mconcat ["/2012-09-25/pipelines/", toText _reaId]

instance ToQuery ReadPipeline where
        toQuery = const mempty

-- | /See:/ 'readPipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rprWarnings'
--
-- * 'rprPipeline'
data ReadPipelineResponse = ReadPipelineResponse'{_rprWarnings :: [Warning], _rprPipeline :: Maybe Pipeline} deriving (Eq, Read, Show)

-- | 'ReadPipelineResponse' smart constructor.
readPipelineResponse :: ReadPipelineResponse
readPipelineResponse = ReadPipelineResponse'{_rprWarnings = mempty, _rprPipeline = Nothing};

-- | Elastic Transcoder returns a warning if the resources used by your
-- pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets,
-- Amazon SNS notification topics, and AWS KMS key, reduces processing time
-- and prevents cross-regional charges.
rprWarnings :: Lens' ReadPipelineResponse [Warning]
rprWarnings = lens _rprWarnings (\ s a -> s{_rprWarnings = a});

-- | A section of the response body that provides information about the
-- pipeline.
rprPipeline :: Lens' ReadPipelineResponse (Maybe Pipeline)
rprPipeline = lens _rprPipeline (\ s a -> s{_rprPipeline = a});
