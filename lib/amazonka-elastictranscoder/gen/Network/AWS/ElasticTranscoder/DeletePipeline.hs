{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.DeletePipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeletePipeline operation removes a pipeline.
--
--
-- You can only delete a pipeline that has never been used or that is not currently in use (doesn't contain any active jobs). If the pipeline is currently in use, @DeletePipeline@ returns an error.
--
module Network.AWS.ElasticTranscoder.DeletePipeline
    (
    -- * Creating a Request
      deletePipeline
    , DeletePipeline
    -- * Request Lenses
    , dId

    -- * Destructuring the Response
    , deletePipelineResponse
    , DeletePipelineResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @DeletePipelineRequest@ structure.
--
--
--
-- /See:/ 'deletePipeline' smart constructor.
newtype DeletePipeline = DeletePipeline'
  { _dId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dId' - The identifier of the pipeline that you want to delete.
deletePipeline
    :: Text -- ^ 'dId'
    -> DeletePipeline
deletePipeline pId_ = DeletePipeline' {_dId = pId_}


-- | The identifier of the pipeline that you want to delete.
dId :: Lens' DeletePipeline Text
dId = lens _dId (\ s a -> s{_dId = a})

instance AWSRequest DeletePipeline where
        type Rs DeletePipeline = DeletePipelineResponse
        request = delete elasticTranscoder
        response
          = receiveEmpty
              (\ s h x ->
                 DeletePipelineResponse' <$> (pure (fromEnum s)))

instance Hashable DeletePipeline where

instance NFData DeletePipeline where

instance ToHeaders DeletePipeline where
        toHeaders = const mempty

instance ToPath DeletePipeline where
        toPath DeletePipeline'{..}
          = mconcat ["/2012-09-25/pipelines/", toBS _dId]

instance ToQuery DeletePipeline where
        toQuery = const mempty

-- | The @DeletePipelineResponse@ structure.
--
--
--
-- /See:/ 'deletePipelineResponse' smart constructor.
newtype DeletePipelineResponse = DeletePipelineResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePipelineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deletePipelineResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeletePipelineResponse
deletePipelineResponse pResponseStatus_ =
  DeletePipelineResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeletePipelineResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeletePipelineResponse where
