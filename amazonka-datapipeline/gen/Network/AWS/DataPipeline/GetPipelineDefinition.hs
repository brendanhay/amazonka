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

-- Module      : Network.AWS.DataPipeline.GetPipelineDefinition
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the definition of the specified pipeline. You can call 'GetPipelineDefinition' to retrieve the pipeline definition you provided using 'PutPipelineDefinition'.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_GetPipelineDefinition.html>
module Network.AWS.DataPipeline.GetPipelineDefinition
    (
    -- * Request
      GetPipelineDefinition
    -- ** Request constructor
    , getPipelineDefinition
    -- ** Request lenses
    , gpdPipelineId
    , gpdVersion

    -- * Response
    , GetPipelineDefinitionResponse
    -- ** Response constructor
    , getPipelineDefinitionResponse
    -- ** Response lenses
    , gpdrParameterObjects
    , gpdrParameterValues
    , gpdrPipelineObjects
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data GetPipelineDefinition = GetPipelineDefinition
    { _gpdPipelineId :: Text
    , _gpdVersion    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'GetPipelineDefinition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpdPipelineId' @::@ 'Text'
--
-- * 'gpdVersion' @::@ 'Maybe' 'Text'
--
getPipelineDefinition :: Text -- ^ 'gpdPipelineId'
                      -> GetPipelineDefinition
getPipelineDefinition p1 = GetPipelineDefinition
    { _gpdPipelineId = p1
    , _gpdVersion    = Nothing
    }

-- | The identifier of the pipeline.
gpdPipelineId :: Lens' GetPipelineDefinition Text
gpdPipelineId = lens _gpdPipelineId (\s a -> s { _gpdPipelineId = a })

-- | The version of the pipeline definition to retrieve. This parameter accepts
-- the values 'latest' (default) and 'active'. Where 'latest' indicates the last
-- definition saved to the pipeline and 'active' indicates the last definition of
-- the pipeline that was activated.
gpdVersion :: Lens' GetPipelineDefinition (Maybe Text)
gpdVersion = lens _gpdVersion (\s a -> s { _gpdVersion = a })

data GetPipelineDefinitionResponse = GetPipelineDefinitionResponse
    { _gpdrParameterObjects :: List "parameterObjects" ParameterObject
    , _gpdrParameterValues  :: List "parameterValues" ParameterValue
    , _gpdrPipelineObjects  :: List "pipelineObjects" PipelineObject
    } deriving (Eq, Read, Show)

-- | 'GetPipelineDefinitionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpdrParameterObjects' @::@ ['ParameterObject']
--
-- * 'gpdrParameterValues' @::@ ['ParameterValue']
--
-- * 'gpdrPipelineObjects' @::@ ['PipelineObject']
--
getPipelineDefinitionResponse :: GetPipelineDefinitionResponse
getPipelineDefinitionResponse = GetPipelineDefinitionResponse
    { _gpdrPipelineObjects  = mempty
    , _gpdrParameterObjects = mempty
    , _gpdrParameterValues  = mempty
    }

-- | Returns a list of parameter objects used in the pipeline definition.
gpdrParameterObjects :: Lens' GetPipelineDefinitionResponse [ParameterObject]
gpdrParameterObjects =
    lens _gpdrParameterObjects (\s a -> s { _gpdrParameterObjects = a })
        . _List

-- | Returns a list of parameter values used in the pipeline definition.
gpdrParameterValues :: Lens' GetPipelineDefinitionResponse [ParameterValue]
gpdrParameterValues =
    lens _gpdrParameterValues (\s a -> s { _gpdrParameterValues = a })
        . _List

-- | An array of objects defined in the pipeline.
gpdrPipelineObjects :: Lens' GetPipelineDefinitionResponse [PipelineObject]
gpdrPipelineObjects =
    lens _gpdrPipelineObjects (\s a -> s { _gpdrPipelineObjects = a })
        . _List

instance ToPath GetPipelineDefinition where
    toPath = const "/"

instance ToQuery GetPipelineDefinition where
    toQuery = const mempty

instance ToHeaders GetPipelineDefinition

instance ToJSON GetPipelineDefinition where
    toJSON GetPipelineDefinition{..} = object
        [ "pipelineId" .= _gpdPipelineId
        , "version"    .= _gpdVersion
        ]

instance AWSRequest GetPipelineDefinition where
    type Sv GetPipelineDefinition = DataPipeline
    type Rs GetPipelineDefinition = GetPipelineDefinitionResponse

    request  = post "GetPipelineDefinition"
    response = jsonResponse

instance FromJSON GetPipelineDefinitionResponse where
    parseJSON = withObject "GetPipelineDefinitionResponse" $ \o -> GetPipelineDefinitionResponse
        <$> o .:? "parameterObjects" .!= mempty
        <*> o .:? "parameterValues" .!= mempty
        <*> o .:? "pipelineObjects" .!= mempty
