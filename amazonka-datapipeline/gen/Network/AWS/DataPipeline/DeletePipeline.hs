{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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
-- that are paused by SetStatus can be resumed. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.DeletePipeline
-- Content-Length: 50 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE"} x-amzn-RequestId:
-- b7a88c81-0754-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT Unexpected response: 200, OK, undefined.
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

import Network.AWS.DataPipeline.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The input for the DeletePipeline action.
newtype DeletePipeline = DeletePipeline
    { _dpPipelineId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeletePipeline' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PipelineId ::@ @Text@
--
deletePipeline :: Text -- ^ 'dpPipelineId'
               -> DeletePipeline
deletePipeline p1 = DeletePipeline
    { _dpPipelineId = p1
    }

-- | The identifier of the pipeline to be deleted.
dpPipelineId :: Lens' DeletePipeline Text
dpPipelineId = lens _dpPipelineId (\s a -> s { _dpPipelineId = a })

instance ToPath DeletePipeline

instance ToQuery DeletePipeline

instance ToHeaders DeletePipeline

instance ToJSON DeletePipeline

data DeletePipelineResponse = DeletePipelineResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeletePipelineResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deletePipelineResponse :: DeletePipelineResponse
deletePipelineResponse = DeletePipelineResponse

instance AWSRequest DeletePipeline where
    type Sv DeletePipeline = DataPipeline
    type Rs DeletePipeline = DeletePipelineResponse

    request = get
    response _ = nullaryResponse DeletePipelineResponse
