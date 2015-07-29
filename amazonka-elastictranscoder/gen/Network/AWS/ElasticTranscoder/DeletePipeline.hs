{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.DeletePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The DeletePipeline operation removes a pipeline.
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
    , dId

    -- * Response
    , DeletePipelineResponse
    -- ** Response constructor
    , deletePipelineResponse
    -- ** Response lenses
    , drsStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @DeletePipelineRequest@ structure.
--
-- /See:/ 'deletePipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dId'
newtype DeletePipeline = DeletePipeline'
    { _dId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletePipeline' smart constructor.
deletePipeline :: Text -> DeletePipeline
deletePipeline pId_ =
    DeletePipeline'
    { _dId = pId_
    }

-- | The identifier of the pipeline that you want to delete.
dId :: Lens' DeletePipeline Text
dId = lens _dId (\ s a -> s{_dId = a});

instance AWSRequest DeletePipeline where
        type Sv DeletePipeline = ElasticTranscoder
        type Rs DeletePipeline = DeletePipelineResponse
        request = delete
        response
          = receiveJSON
              (\ s h x ->
                 DeletePipelineResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeletePipeline where
        toHeaders = const mempty

instance ToPath DeletePipeline where
        toPath DeletePipeline'{..}
          = ["2012-09-25", "pipelines", toBS _dId]

instance ToQuery DeletePipeline where
        toQuery = const mempty

-- | The @DeletePipelineResponse@ structure.
--
-- /See:/ 'deletePipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsStatus'
newtype DeletePipelineResponse = DeletePipelineResponse'
    { _drsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletePipelineResponse' smart constructor.
deletePipelineResponse :: Int -> DeletePipelineResponse
deletePipelineResponse pStatus_ =
    DeletePipelineResponse'
    { _drsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
drsStatus :: Lens' DeletePipelineResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
