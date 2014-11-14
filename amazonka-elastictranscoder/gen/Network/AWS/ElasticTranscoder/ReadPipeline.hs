{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.ElasticTranscoder.ReadPipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ReadPipeline operation gets detailed information about a pipeline.
module Network.AWS.ElasticTranscoder.ReadPipeline
    (
    -- * Request
      ReadPipeline
    -- ** Request constructor
    , readPipeline
    -- ** Request lenses
    , rp1Id

    -- * Response
    , ReadPipelineResponse
    -- ** Response constructor
    , readPipelineResponse
    -- ** Response lenses
    , rprPipeline
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ElasticTranscoder.Types

newtype ReadPipeline = ReadPipeline
    { _rp1Id :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'ReadPipeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rp1Id' @::@ 'Text'
--
readPipeline :: Text -- ^ 'rp1Id'
             -> ReadPipeline
readPipeline p1 = ReadPipeline
    { _rp1Id = p1
    }

-- | The identifier of the pipeline to read.
rp1Id :: Lens' ReadPipeline Text
rp1Id = lens _rp1Id (\s a -> s { _rp1Id = a })

instance ToPath ReadPipeline where
    toPath ReadPipeline{..} = mconcat
        [ "/2012-09-25/pipelines/"
        , toText _rp1Id
        ]

instance ToQuery ReadPipeline where
    toQuery = const mempty

instance ToHeaders ReadPipeline

newtype ReadPipelineResponse = ReadPipelineResponse
    { _rprPipeline :: Maybe Pipeline
    } deriving (Eq, Show, Generic)

-- | 'ReadPipelineResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rprPipeline' @::@ 'Maybe' 'Pipeline'
--
readPipelineResponse :: ReadPipelineResponse
readPipelineResponse = ReadPipelineResponse
    { _rprPipeline = Nothing
    }

-- | A section of the response body that provides information about the
-- pipeline.
rprPipeline :: Lens' ReadPipelineResponse (Maybe Pipeline)
rprPipeline = lens _rprPipeline (\s a -> s { _rprPipeline = a })

instance AWSRequest ReadPipeline where
    type Sv ReadPipeline = ElasticTranscoder
    type Rs ReadPipeline = ReadPipelineResponse

    request  = get
    response = jsonResponse $ \h o -> ReadPipelineResponse
        <$> o .: "Pipeline"
