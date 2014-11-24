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

-- Module      : Network.AWS.DataPipeline.CreatePipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new empty pipeline. When this action succeeds, you can then use
-- the 'PutPipelineDefinition' action to populate the pipeline.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_CreatePipeline.html>
module Network.AWS.DataPipeline.CreatePipeline
    (
    -- * Request
      CreatePipeline
    -- ** Request constructor
    , createPipeline
    -- ** Request lenses
    , cpDescription
    , cpName
    , cpUniqueId

    -- * Response
    , CreatePipelineResponse
    -- ** Response constructor
    , createPipelineResponse
    -- ** Response lenses
    , cprPipelineId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data CreatePipeline = CreatePipeline
    { _cpDescription :: Maybe Text
    , _cpName        :: Text
    , _cpUniqueId    :: Text
    } deriving (Eq, Ord, Show)

-- | 'CreatePipeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpDescription' @::@ 'Maybe' 'Text'
--
-- * 'cpName' @::@ 'Text'
--
-- * 'cpUniqueId' @::@ 'Text'
--
createPipeline :: Text -- ^ 'cpName'
               -> Text -- ^ 'cpUniqueId'
               -> CreatePipeline
createPipeline p1 p2 = CreatePipeline
    { _cpName        = p1
    , _cpUniqueId    = p2
    , _cpDescription = Nothing
    }

-- | The description of the new pipeline.
cpDescription :: Lens' CreatePipeline (Maybe Text)
cpDescription = lens _cpDescription (\s a -> s { _cpDescription = a })

-- | The name of the new pipeline. You can use the same name for multiple
-- pipelines associated with your AWS account, because AWS Data Pipeline
-- assigns each new pipeline a unique pipeline identifier.
cpName :: Lens' CreatePipeline Text
cpName = lens _cpName (\s a -> s { _cpName = a })

-- | A unique identifier that you specify. This identifier is not the same as
-- the pipeline identifier assigned by AWS Data Pipeline. You are
-- responsible for defining the format and ensuring the uniqueness of this
-- identifier. You use this parameter to ensure idempotency during repeated
-- calls to 'CreatePipeline'. For example, if the first call to
-- 'CreatePipeline' does not return a clear success, you can pass in the
-- same unique identifier and pipeline name combination on a subsequent call
-- to 'CreatePipeline'. 'CreatePipeline' ensures that if a pipeline already
-- exists with the same name and unique identifier, a new pipeline will not
-- be created. Instead, you'll receive the pipeline identifier from the
-- previous attempt. The uniqueness of the name and unique identifier
-- combination is scoped to the AWS account or IAM user credentials.
cpUniqueId :: Lens' CreatePipeline Text
cpUniqueId = lens _cpUniqueId (\s a -> s { _cpUniqueId = a })

newtype CreatePipelineResponse = CreatePipelineResponse
    { _cprPipelineId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'CreatePipelineResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprPipelineId' @::@ 'Text'
--
createPipelineResponse :: Text -- ^ 'cprPipelineId'
                       -> CreatePipelineResponse
createPipelineResponse p1 = CreatePipelineResponse
    { _cprPipelineId = p1
    }

-- | The ID that AWS Data Pipeline assigns the newly created pipeline. The ID
-- is a string of the form: df-06372391ZG65EXAMPLE.
cprPipelineId :: Lens' CreatePipelineResponse Text
cprPipelineId = lens _cprPipelineId (\s a -> s { _cprPipelineId = a })

instance ToPath CreatePipeline where
    toPath = const "/"

instance ToQuery CreatePipeline where
    toQuery = const mempty

instance ToHeaders CreatePipeline

instance ToJSON CreatePipeline where
    toJSON CreatePipeline{..} = object
        [ "name"        .= _cpName
        , "uniqueId"    .= _cpUniqueId
        , "description" .= _cpDescription
        ]

instance AWSRequest CreatePipeline where
    type Sv CreatePipeline = DataPipeline
    type Rs CreatePipeline = CreatePipelineResponse

    request  = post "CreatePipeline"
    response = jsonResponse

instance FromJSON CreatePipelineResponse where
    parseJSON = withObject "CreatePipelineResponse" $ \o -> CreatePipelineResponse
        <$> o .:  "pipelineId"
