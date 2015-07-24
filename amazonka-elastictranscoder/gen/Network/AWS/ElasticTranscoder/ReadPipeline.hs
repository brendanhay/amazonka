{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ReadPipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The ReadPipeline operation gets detailed information about a pipeline.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ReadPipeline.html>
module Network.AWS.ElasticTranscoder.ReadPipeline
    (
    -- * Request
      ReadPipeline
    -- ** Request constructor
    , readPipeline
    -- ** Request lenses
    , rId

    -- * Response
    , ReadPipelineResponse
    -- ** Response constructor
    , readPipelineResponse
    -- ** Response lenses
    , rrsWarnings
    , rrsPipeline
    , rrsStatus
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
-- * 'rId'
newtype ReadPipeline = ReadPipeline'
    { _rId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReadPipeline' smart constructor.
readPipeline :: Text -> ReadPipeline
readPipeline pId_ =
    ReadPipeline'
    { _rId = pId_
    }

-- | The identifier of the pipeline to read.
rId :: Lens' ReadPipeline Text
rId = lens _rId (\ s a -> s{_rId = a});

instance AWSRequest ReadPipeline where
        type Sv ReadPipeline = ElasticTranscoder
        type Rs ReadPipeline = ReadPipelineResponse
        request = get "ReadPipeline"
        response
          = receiveJSON
              (\ s h x ->
                 ReadPipelineResponse' <$>
                   (x .?> "Warnings" .!@ mempty) <*> (x .?> "Pipeline")
                     <*> (pure (fromEnum s)))

instance ToHeaders ReadPipeline where
        toHeaders = const mempty

instance ToPath ReadPipeline where
        toPath ReadPipeline'{..}
          = mconcat ["/2012-09-25/pipelines/", toText _rId]

instance ToQuery ReadPipeline where
        toQuery = const mempty

-- | The @ReadPipelineResponse@ structure.
--
-- /See:/ 'readPipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrsWarnings'
--
-- * 'rrsPipeline'
--
-- * 'rrsStatus'
data ReadPipelineResponse = ReadPipelineResponse'
    { _rrsWarnings :: !(Maybe [Warning])
    , _rrsPipeline :: !(Maybe Pipeline)
    , _rrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReadPipelineResponse' smart constructor.
readPipelineResponse :: Int -> ReadPipelineResponse
readPipelineResponse pStatus_ =
    ReadPipelineResponse'
    { _rrsWarnings = Nothing
    , _rrsPipeline = Nothing
    , _rrsStatus = pStatus_
    }

-- | Elastic Transcoder returns a warning if the resources used by your
-- pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets,
-- Amazon SNS notification topics, and AWS KMS key, reduces processing time
-- and prevents cross-regional charges.
rrsWarnings :: Lens' ReadPipelineResponse [Warning]
rrsWarnings = lens _rrsWarnings (\ s a -> s{_rrsWarnings = a}) . _Default;

-- | A section of the response body that provides information about the
-- pipeline.
rrsPipeline :: Lens' ReadPipelineResponse (Maybe Pipeline)
rrsPipeline = lens _rrsPipeline (\ s a -> s{_rrsPipeline = a});

-- | FIXME: Undocumented member.
rrsStatus :: Lens' ReadPipelineResponse Int
rrsStatus = lens _rrsStatus (\ s a -> s{_rrsStatus = a});
