{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , rp1Id

    -- * Response
    , ReadPipelineResponse
    -- ** Response constructor
    , readPipelineResponse
    -- ** Response lenses
    , rprPipeline
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.ElasticTranscoder.Types
import qualified GHC.Exts

newtype ReadPipeline = ReadPipeline
    { _rp1Id :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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

newtype ReadPipelineResponse = ReadPipelineResponse
    { _rprPipeline :: Maybe Pipeline
    } deriving (Eq, Read, Show)

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

-- | A section of the response body that provides information about the pipeline.
rprPipeline :: Lens' ReadPipelineResponse (Maybe Pipeline)
rprPipeline = lens _rprPipeline (\s a -> s { _rprPipeline = a })

instance ToPath ReadPipeline where
    toPath ReadPipeline{..} = mconcat
        [ "/2012-09-25/pipelines/"
        , toText _rp1Id
        ]

instance ToQuery ReadPipeline where
    toQuery = const mempty

instance ToHeaders ReadPipeline

instance ToJSON ReadPipeline where
    toJSON = const (toJSON Empty)

instance AWSRequest ReadPipeline where
    type Sv ReadPipeline = ElasticTranscoder
    type Rs ReadPipeline = ReadPipelineResponse

    request  = get
    response = jsonResponse

instance FromJSON ReadPipelineResponse where
    parseJSON = withObject "ReadPipelineResponse" $ \o -> ReadPipelineResponse
        <$> o .:? "Pipeline"
