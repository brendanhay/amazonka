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
-- Module      : Network.AWS.ElasticTranscoder.ReadPipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ReadPipeline operation gets detailed information about a pipeline.
--
--
module Network.AWS.ElasticTranscoder.ReadPipeline
    (
    -- * Creating a Request
      readPipeline
    , ReadPipeline
    -- * Request Lenses
    , rId

    -- * Destructuring the Response
    , readPipelineResponse
    , ReadPipelineResponse
    -- * Response Lenses
    , rrsWarnings
    , rrsPipeline
    , rrsResponseStatus
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @ReadPipelineRequest@ structure.
--
--
--
-- /See:/ 'readPipeline' smart constructor.
newtype ReadPipeline = ReadPipeline'
  { _rId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReadPipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rId' - The identifier of the pipeline to read.
readPipeline
    :: Text -- ^ 'rId'
    -> ReadPipeline
readPipeline pId_ = ReadPipeline' {_rId = pId_}


-- | The identifier of the pipeline to read.
rId :: Lens' ReadPipeline Text
rId = lens _rId (\ s a -> s{_rId = a})

instance AWSRequest ReadPipeline where
        type Rs ReadPipeline = ReadPipelineResponse
        request = get elasticTranscoder
        response
          = receiveJSON
              (\ s h x ->
                 ReadPipelineResponse' <$>
                   (x .?> "Warnings" .!@ mempty) <*> (x .?> "Pipeline")
                     <*> (pure (fromEnum s)))

instance Hashable ReadPipeline where

instance NFData ReadPipeline where

instance ToHeaders ReadPipeline where
        toHeaders = const mempty

instance ToPath ReadPipeline where
        toPath ReadPipeline'{..}
          = mconcat ["/2012-09-25/pipelines/", toBS _rId]

instance ToQuery ReadPipeline where
        toQuery = const mempty

-- | The @ReadPipelineResponse@ structure.
--
--
--
-- /See:/ 'readPipelineResponse' smart constructor.
data ReadPipelineResponse = ReadPipelineResponse'
  { _rrsWarnings       :: !(Maybe [Warning])
  , _rrsPipeline       :: !(Maybe Pipeline)
  , _rrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReadPipelineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsWarnings' - Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline. Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
--
-- * 'rrsPipeline' - A section of the response body that provides information about the pipeline.
--
-- * 'rrsResponseStatus' - -- | The response status code.
readPipelineResponse
    :: Int -- ^ 'rrsResponseStatus'
    -> ReadPipelineResponse
readPipelineResponse pResponseStatus_ =
  ReadPipelineResponse'
    { _rrsWarnings = Nothing
    , _rrsPipeline = Nothing
    , _rrsResponseStatus = pResponseStatus_
    }


-- | Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline. Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
rrsWarnings :: Lens' ReadPipelineResponse [Warning]
rrsWarnings = lens _rrsWarnings (\ s a -> s{_rrsWarnings = a}) . _Default . _Coerce

-- | A section of the response body that provides information about the pipeline.
rrsPipeline :: Lens' ReadPipelineResponse (Maybe Pipeline)
rrsPipeline = lens _rrsPipeline (\ s a -> s{_rrsPipeline = a})

-- | -- | The response status code.
rrsResponseStatus :: Lens' ReadPipelineResponse Int
rrsResponseStatus = lens _rrsResponseStatus (\ s a -> s{_rrsResponseStatus = a})

instance NFData ReadPipelineResponse where
