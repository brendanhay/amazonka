{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , reaWarnings
    , reaPipeline
    , reaStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @ReadPipelineRequest@ structure.
--
-- /See:/ 'readPipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reaId'
newtype ReadPipeline = ReadPipeline'
    { _reaId :: Text
    } deriving (Eq,Read,Show)

-- | 'ReadPipeline' smart constructor.
readPipeline :: Text -> ReadPipeline
readPipeline pId =
    ReadPipeline'
    { _reaId = pId
    }

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
                   (x .?> "Warnings" .!@ mempty) <*> (x .?> "Pipeline")
                     <*> (pure s))

instance ToHeaders ReadPipeline where
        toHeaders = const mempty

instance ToPath ReadPipeline where
        toPath ReadPipeline'{..}
          = mconcat ["/2012-09-25/pipelines/", toText _reaId]

instance ToQuery ReadPipeline where
        toQuery = const mempty

-- | The @ReadPipelineResponse@ structure.
--
-- /See:/ 'readPipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reaWarnings'
--
-- * 'reaPipeline'
--
-- * 'reaStatus'
data ReadPipelineResponse = ReadPipelineResponse'
    { _reaWarnings :: !(Maybe [Warning])
    , _reaPipeline :: !(Maybe Pipeline)
    , _reaStatus   :: !Status
    } deriving (Eq,Read,Show)

-- | 'ReadPipelineResponse' smart constructor.
readPipelineResponse :: Status -> ReadPipelineResponse
readPipelineResponse pStatus =
    ReadPipelineResponse'
    { _reaWarnings = Nothing
    , _reaPipeline = Nothing
    , _reaStatus = pStatus
    }

-- | Elastic Transcoder returns a warning if the resources used by your
-- pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets,
-- Amazon SNS notification topics, and AWS KMS key, reduces processing time
-- and prevents cross-regional charges.
reaWarnings :: Lens' ReadPipelineResponse [Warning]
reaWarnings = lens _reaWarnings (\ s a -> s{_reaWarnings = a}) . _Default;

-- | A section of the response body that provides information about the
-- pipeline.
reaPipeline :: Lens' ReadPipelineResponse (Maybe Pipeline)
reaPipeline = lens _reaPipeline (\ s a -> s{_reaPipeline = a});

-- | FIXME: Undocumented member.
reaStatus :: Lens' ReadPipelineResponse Status
reaStatus = lens _reaStatus (\ s a -> s{_reaStatus = a});
