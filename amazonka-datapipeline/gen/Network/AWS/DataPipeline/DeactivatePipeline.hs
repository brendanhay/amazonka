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

-- Module      : Network.AWS.DataPipeline.DeactivatePipeline
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

-- | Deactivates the specified running pipeline. The pipeline is set to the 'DEACTIVATING' state until the deactivation process completes.
--
-- To resume a deactivated pipeline, use 'ActivatePipeline'. By default, the
-- pipeline resumes from the last completed execution. Optionally, you can
-- specify the date and time to resume the pipeline.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_DeactivatePipeline.html>
module Network.AWS.DataPipeline.DeactivatePipeline
    (
    -- * Request
      DeactivatePipeline
    -- ** Request constructor
    , deactivatePipeline
    -- ** Request lenses
    , dp1CancelActive
    , dp1PipelineId

    -- * Response
    , DeactivatePipelineResponse
    -- ** Response constructor
    , deactivatePipelineResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data DeactivatePipeline = DeactivatePipeline
    { _dp1CancelActive :: Maybe Bool
    , _dp1PipelineId   :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeactivatePipeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dp1CancelActive' @::@ 'Maybe' 'Bool'
--
-- * 'dp1PipelineId' @::@ 'Text'
--
deactivatePipeline :: Text -- ^ 'dp1PipelineId'
                   -> DeactivatePipeline
deactivatePipeline p1 = DeactivatePipeline
    { _dp1PipelineId   = p1
    , _dp1CancelActive = Nothing
    }

-- | Indicates whether to cancel any running objects. The default is true, which
-- sets the state of any running objects to 'CANCELED'. If this value is false,
-- the pipeline is deactivated after all running objects finish.
dp1CancelActive :: Lens' DeactivatePipeline (Maybe Bool)
dp1CancelActive = lens _dp1CancelActive (\s a -> s { _dp1CancelActive = a })

-- | The ID of the pipeline.
dp1PipelineId :: Lens' DeactivatePipeline Text
dp1PipelineId = lens _dp1PipelineId (\s a -> s { _dp1PipelineId = a })

data DeactivatePipelineResponse = DeactivatePipelineResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeactivatePipelineResponse' constructor.
deactivatePipelineResponse :: DeactivatePipelineResponse
deactivatePipelineResponse = DeactivatePipelineResponse

instance ToPath DeactivatePipeline where
    toPath = const "/"

instance ToQuery DeactivatePipeline where
    toQuery = const mempty

instance ToHeaders DeactivatePipeline

instance ToJSON DeactivatePipeline where
    toJSON DeactivatePipeline{..} = object
        [ "pipelineId"   .= _dp1PipelineId
        , "cancelActive" .= _dp1CancelActive
        ]

instance AWSRequest DeactivatePipeline where
    type Sv DeactivatePipeline = DataPipeline
    type Rs DeactivatePipeline = DeactivatePipelineResponse

    request  = post "DeactivatePipeline"
    response = nullResponse DeactivatePipelineResponse
