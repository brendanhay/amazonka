{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticTranscoder.DeletePipeline
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

-- | The DeletePipeline operation removes a pipeline.
--
-- You can only delete a pipeline that has never been used or that is not
-- currently in use (doesn\'t contain any active jobs). If the pipeline is
-- currently in use, @DeletePipeline@ returns an error.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/DeletePipeline.html>
module Network.AWS.ElasticTranscoder.DeletePipeline
    (
    -- * Request
      DeletePipeline
    -- ** Request constructor
    , deletePipeline
    -- ** Request lenses
    , delId

    -- * Response
    , DeletePipelineResponse
    -- ** Response constructor
    , deletePipelineResponse
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delId'
newtype DeletePipeline = DeletePipeline'{_delId :: Text} deriving (Eq, Read, Show)

-- | 'DeletePipeline' smart constructor.
deletePipeline :: Text -> DeletePipeline
deletePipeline pId = DeletePipeline'{_delId = pId};

-- | The identifier of the pipeline that you want to delete.
delId :: Lens' DeletePipeline Text
delId = lens _delId (\ s a -> s{_delId = a});

instance AWSRequest DeletePipeline where
        type Sv DeletePipeline = ElasticTranscoder
        type Rs DeletePipeline = DeletePipelineResponse
        request = delete
        response = receiveNull DeletePipelineResponse'

instance ToHeaders DeletePipeline where
        toHeaders = const mempty

instance ToPath DeletePipeline where
        toPath DeletePipeline'{..}
          = mconcat ["/2012-09-25/pipelines/", toText _delId]

instance ToQuery DeletePipeline where
        toQuery = const mempty

-- | /See:/ 'deletePipelineResponse' smart constructor.
data DeletePipelineResponse = DeletePipelineResponse' deriving (Eq, Read, Show)

-- | 'DeletePipelineResponse' smart constructor.
deletePipelineResponse :: DeletePipelineResponse
deletePipelineResponse = DeletePipelineResponse';
