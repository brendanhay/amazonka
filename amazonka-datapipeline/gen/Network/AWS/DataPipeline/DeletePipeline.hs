{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.DeletePipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Permanently deletes a pipeline, its pipeline definition and its run
-- history. You cannot query or restore a deleted pipeline. AWS Data Pipeline
-- will attempt to cancel instances associated with the pipeline that are
-- currently being processed by task runners. Deleting a pipeline cannot be
-- undone. To temporarily pause a pipeline instead of deleting it, call
-- SetStatus with the status set to Pause on individual components. Components
-- that are paused by SetStatus can be resumed.
--
-- <DeletePipeline.html>
module Network.AWS.DataPipeline.DeletePipeline
    (
    -- * Request
      DeletePipeline
    -- ** Request constructor
    , deletePipeline
    -- ** Request lenses
    , dpPipelineId

    -- * Response
    , DeletePipelineResponse
    -- ** Response constructor
    , deletePipelineResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

newtype DeletePipeline = DeletePipeline
    { _dpPipelineId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeletePipeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpPipelineId' @::@ 'Text'
--
deletePipeline :: Text -- ^ 'dpPipelineId'
               -> DeletePipeline
deletePipeline p1 = DeletePipeline
    { _dpPipelineId = p1
    }

-- | The identifier of the pipeline to be deleted.
dpPipelineId :: Lens' DeletePipeline Text
dpPipelineId = lens _dpPipelineId (\s a -> s { _dpPipelineId = a })

data DeletePipelineResponse = DeletePipelineResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeletePipelineResponse' constructor.
deletePipelineResponse :: DeletePipelineResponse
deletePipelineResponse = DeletePipelineResponse

instance AWSRequest DeletePipeline where
    type Sv DeletePipeline = DataPipeline
    type Rs DeletePipeline = DeletePipelineResponse

    request  = post
    response = nullResponse DeletePipelineResponse

instance ToPath DeletePipeline where
    toPath = const "/"

instance ToHeaders DeletePipeline

instance ToQuery DeletePipeline where
    toQuery = const mempty

instance ToJSON DeletePipeline where
    toJSON = genericToJSON jsonOptions
