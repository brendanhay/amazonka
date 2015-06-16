{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DataPipeline.GetPipelineDefinition
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

-- | Gets the definition of the specified pipeline. You can call
-- @GetPipelineDefinition@ to retrieve the pipeline definition that you
-- provided using PutPipelineDefinition.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_GetPipelineDefinition.html>
module Network.AWS.DataPipeline.GetPipelineDefinition
    (
    -- * Request
      GetPipelineDefinition
    -- ** Request constructor
    , getPipelineDefinition
    -- ** Request lenses
    , gpdVersion
    , gpdPipelineId

    -- * Response
    , GetPipelineDefinitionResponse
    -- ** Response constructor
    , getPipelineDefinitionResponse
    -- ** Response lenses
    , gpdrPipelineObjects
    , gpdrParameterObjects
    , gpdrParameterValues
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.DataPipeline.Types

-- | /See:/ 'getPipelineDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpdVersion'
--
-- * 'gpdPipelineId'
data GetPipelineDefinition = GetPipelineDefinition'{_gpdVersion :: Maybe Text, _gpdPipelineId :: Text} deriving (Eq, Read, Show)

-- | 'GetPipelineDefinition' smart constructor.
getPipelineDefinition :: Text -> GetPipelineDefinition
getPipelineDefinition pPipelineId = GetPipelineDefinition'{_gpdVersion = Nothing, _gpdPipelineId = pPipelineId};

-- | The version of the pipeline definition to retrieve. Set this parameter
-- to @latest@ (default) to use the last definition saved to the pipeline
-- or @active@ to use the last definition that was activated.
gpdVersion :: Lens' GetPipelineDefinition (Maybe Text)
gpdVersion = lens _gpdVersion (\ s a -> s{_gpdVersion = a});

-- | The ID of the pipeline.
gpdPipelineId :: Lens' GetPipelineDefinition Text
gpdPipelineId = lens _gpdPipelineId (\ s a -> s{_gpdPipelineId = a});

instance AWSRequest GetPipelineDefinition where
        type Sv GetPipelineDefinition = DataPipeline
        type Rs GetPipelineDefinition =
             GetPipelineDefinitionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetPipelineDefinitionResponse' <$>
                   (x .?> "pipelineObjects" .!@ mempty) <*>
                     (x .?> "parameterObjects" .!@ mempty)
                     <*> (x .?> "parameterValues" .!@ mempty))

instance ToHeaders GetPipelineDefinition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.GetPipelineDefinition" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPipelineDefinition where
        toJSON GetPipelineDefinition'{..}
          = object
              ["version" .= _gpdVersion,
               "pipelineId" .= _gpdPipelineId]

instance ToPath GetPipelineDefinition where
        toPath = const "/"

instance ToQuery GetPipelineDefinition where
        toQuery = const mempty

-- | /See:/ 'getPipelineDefinitionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpdrPipelineObjects'
--
-- * 'gpdrParameterObjects'
--
-- * 'gpdrParameterValues'
data GetPipelineDefinitionResponse = GetPipelineDefinitionResponse'{_gpdrPipelineObjects :: Maybe [PipelineObject], _gpdrParameterObjects :: Maybe [ParameterObject], _gpdrParameterValues :: Maybe [ParameterValue]} deriving (Eq, Read, Show)

-- | 'GetPipelineDefinitionResponse' smart constructor.
getPipelineDefinitionResponse :: GetPipelineDefinitionResponse
getPipelineDefinitionResponse = GetPipelineDefinitionResponse'{_gpdrPipelineObjects = Nothing, _gpdrParameterObjects = Nothing, _gpdrParameterValues = Nothing};

-- | The objects defined in the pipeline.
gpdrPipelineObjects :: Lens' GetPipelineDefinitionResponse [PipelineObject]
gpdrPipelineObjects = lens _gpdrPipelineObjects (\ s a -> s{_gpdrPipelineObjects = a}) . _Default;

-- | The parameter objects used in the pipeline definition.
gpdrParameterObjects :: Lens' GetPipelineDefinitionResponse [ParameterObject]
gpdrParameterObjects = lens _gpdrParameterObjects (\ s a -> s{_gpdrParameterObjects = a}) . _Default;

-- | The parameter values used in the pipeline definition.
gpdrParameterValues :: Lens' GetPipelineDefinitionResponse [ParameterValue]
gpdrParameterValues = lens _gpdrParameterValues (\ s a -> s{_gpdrParameterValues = a}) . _Default;
