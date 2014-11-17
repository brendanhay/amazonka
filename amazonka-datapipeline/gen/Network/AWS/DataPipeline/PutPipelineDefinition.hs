{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.PutPipelineDefinition
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds tasks, schedules, and preconditions that control the behavior of the
-- pipeline. You can use PutPipelineDefinition to populate a new pipeline.
-- PutPipelineDefinition also validates the configuration as it adds it to the
-- pipeline. Changes to the pipeline are saved unless one of the following
-- three validation errors exists in the pipeline. An object is missing a name
-- or identifier field. A string or reference field is empty. The number of
-- objects in the pipeline exceeds the maximum allowed objects. Pipeline
-- object definitions are passed to the PutPipelineDefinition action and
-- returned by the GetPipelineDefinition action.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_PutPipelineDefinition.html>
module Network.AWS.DataPipeline.PutPipelineDefinition
    (
    -- * Request
      PutPipelineDefinition
    -- ** Request constructor
    , putPipelineDefinition
    -- ** Request lenses
    , ppdPipelineId
    , ppdPipelineObjects

    -- * Response
    , PutPipelineDefinitionResponse
    -- ** Response constructor
    , putPipelineDefinitionResponse
    -- ** Response lenses
    , ppdrErrored
    , ppdrValidationErrors
    , ppdrValidationWarnings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data PutPipelineDefinition = PutPipelineDefinition
    { _ppdPipelineId      :: Text
    , _ppdPipelineObjects :: [PipelineObject]
    } deriving (Eq, Show, Generic)

-- | 'PutPipelineDefinition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ppdPipelineId' @::@ 'Text'
--
-- * 'ppdPipelineObjects' @::@ ['PipelineObject']
--
putPipelineDefinition :: Text -- ^ 'ppdPipelineId'
                      -> PutPipelineDefinition
putPipelineDefinition p1 = PutPipelineDefinition
    { _ppdPipelineId      = p1
    , _ppdPipelineObjects = mempty
    }

-- | The identifier of the pipeline to be configured.
ppdPipelineId :: Lens' PutPipelineDefinition Text
ppdPipelineId = lens _ppdPipelineId (\s a -> s { _ppdPipelineId = a })

-- | The objects that define the pipeline. These will overwrite the existing
-- pipeline definition.
ppdPipelineObjects :: Lens' PutPipelineDefinition [PipelineObject]
ppdPipelineObjects =
    lens _ppdPipelineObjects (\s a -> s { _ppdPipelineObjects = a })

data PutPipelineDefinitionResponse = PutPipelineDefinitionResponse
    { _ppdrErrored            :: Bool
    , _ppdrValidationErrors   :: [ValidationError]
    , _ppdrValidationWarnings :: [ValidationWarning]
    } deriving (Eq, Show, Generic)

-- | 'PutPipelineDefinitionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ppdrErrored' @::@ 'Bool'
--
-- * 'ppdrValidationErrors' @::@ ['ValidationError']
--
-- * 'ppdrValidationWarnings' @::@ ['ValidationWarning']
--
putPipelineDefinitionResponse :: Bool -- ^ 'ppdrErrored'
                              -> PutPipelineDefinitionResponse
putPipelineDefinitionResponse p1 = PutPipelineDefinitionResponse
    { _ppdrErrored            = p1
    , _ppdrValidationErrors   = mempty
    , _ppdrValidationWarnings = mempty
    }

-- | If True, there were validation errors. If errored is True, the pipeline
-- definition is stored but cannot be activated until you correct the
-- pipeline and call PutPipelineDefinition to commit the corrected pipeline.
ppdrErrored :: Lens' PutPipelineDefinitionResponse Bool
ppdrErrored = lens _ppdrErrored (\s a -> s { _ppdrErrored = a })

-- | A list of the validation errors that are associated with the objects
-- defined in pipelineObjects.
ppdrValidationErrors :: Lens' PutPipelineDefinitionResponse [ValidationError]
ppdrValidationErrors =
    lens _ppdrValidationErrors (\s a -> s { _ppdrValidationErrors = a })

-- | A list of the validation warnings that are associated with the objects
-- defined in pipelineObjects.
ppdrValidationWarnings :: Lens' PutPipelineDefinitionResponse [ValidationWarning]
ppdrValidationWarnings =
    lens _ppdrValidationWarnings (\s a -> s { _ppdrValidationWarnings = a })

instance ToPath PutPipelineDefinition where
    toPath = const "/"

instance ToQuery PutPipelineDefinition where
    toQuery = const mempty

instance ToHeaders PutPipelineDefinition
instance ToJSON PutPipelineDefinition where
    toJSON = genericToJSON jsonOptions

instance AWSRequest PutPipelineDefinition where
    type Sv PutPipelineDefinition = DataPipeline
    type Rs PutPipelineDefinition = PutPipelineDefinitionResponse

    request  = post
    response = jsonResponse

instance FromJSON PutPipelineDefinitionResponse where
    parseJSON = genericParseJSON jsonOptions
