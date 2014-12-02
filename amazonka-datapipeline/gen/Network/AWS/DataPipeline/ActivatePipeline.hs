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

-- Module      : Network.AWS.DataPipeline.ActivatePipeline
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

-- | Validates a pipeline and initiates processing. If the pipeline does not pass
-- validation, activation fails. You cannot perform this operation on FINISHED
-- pipelines and attempting to do so will return an InvalidRequestException.
--
-- Call this action to start processing pipeline tasks of a pipeline you've
-- created using the 'CreatePipeline' and 'PutPipelineDefinition' actions. A
-- pipeline cannot be modified after it has been successfully activated.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_ActivatePipeline.html>
module Network.AWS.DataPipeline.ActivatePipeline
    (
    -- * Request
      ActivatePipeline
    -- ** Request constructor
    , activatePipeline
    -- ** Request lenses
    , apParameterValues
    , apPipelineId

    -- * Response
    , ActivatePipelineResponse
    -- ** Response constructor
    , activatePipelineResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data ActivatePipeline = ActivatePipeline
    { _apParameterValues :: List "parameterValues" ParameterValue
    , _apPipelineId      :: Text
    } deriving (Eq, Show)

-- | 'ActivatePipeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apParameterValues' @::@ ['ParameterValue']
--
-- * 'apPipelineId' @::@ 'Text'
--
activatePipeline :: Text -- ^ 'apPipelineId'
                 -> ActivatePipeline
activatePipeline p1 = ActivatePipeline
    { _apPipelineId      = p1
    , _apParameterValues = mempty
    }

-- | Returns a list of parameter values to pass to the pipeline at activation.
apParameterValues :: Lens' ActivatePipeline [ParameterValue]
apParameterValues =
    lens _apParameterValues (\s a -> s { _apParameterValues = a })
        . _List

-- | The identifier of the pipeline to activate.
apPipelineId :: Lens' ActivatePipeline Text
apPipelineId = lens _apPipelineId (\s a -> s { _apPipelineId = a })

data ActivatePipelineResponse = ActivatePipelineResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ActivatePipelineResponse' constructor.
activatePipelineResponse :: ActivatePipelineResponse
activatePipelineResponse = ActivatePipelineResponse

instance ToPath ActivatePipeline where
    toPath = const "/"

instance ToQuery ActivatePipeline where
    toQuery = const mempty

instance ToHeaders ActivatePipeline

instance ToJSON ActivatePipeline where
    toJSON ActivatePipeline{..} = object
        [ "pipelineId"      .= _apPipelineId
        , "parameterValues" .= _apParameterValues
        ]

instance AWSRequest ActivatePipeline where
    type Sv ActivatePipeline = DataPipeline
    type Rs ActivatePipeline = ActivatePipelineResponse

    request  = post "ActivatePipeline"
    response = nullResponse ActivatePipelineResponse
