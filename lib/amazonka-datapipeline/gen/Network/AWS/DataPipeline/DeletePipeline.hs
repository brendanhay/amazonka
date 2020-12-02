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
-- Module      : Network.AWS.DataPipeline.DeletePipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a pipeline, its pipeline definition, and its run history. AWS Data Pipeline attempts to cancel instances associated with the pipeline that are currently being processed by task runners.
--
--
-- Deleting a pipeline cannot be undone. You cannot query or restore a deleted pipeline. To temporarily pause a pipeline instead of deleting it, call 'SetStatus' with the status set to @PAUSE@ on individual components. Components that are paused by 'SetStatus' can be resumed.
--
module Network.AWS.DataPipeline.DeletePipeline
    (
    -- * Creating a Request
      deletePipeline
    , DeletePipeline
    -- * Request Lenses
    , dpPipelineId

    -- * Destructuring the Response
    , deletePipelineResponse
    , DeletePipelineResponse
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DeletePipeline.
--
--
--
-- /See:/ 'deletePipeline' smart constructor.
newtype DeletePipeline = DeletePipeline'
  { _dpPipelineId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpPipelineId' - The ID of the pipeline.
deletePipeline
    :: Text -- ^ 'dpPipelineId'
    -> DeletePipeline
deletePipeline pPipelineId_ = DeletePipeline' {_dpPipelineId = pPipelineId_}


-- | The ID of the pipeline.
dpPipelineId :: Lens' DeletePipeline Text
dpPipelineId = lens _dpPipelineId (\ s a -> s{_dpPipelineId = a})

instance AWSRequest DeletePipeline where
        type Rs DeletePipeline = DeletePipelineResponse
        request = postJSON dataPipeline
        response = receiveNull DeletePipelineResponse'

instance Hashable DeletePipeline where

instance NFData DeletePipeline where

instance ToHeaders DeletePipeline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.DeletePipeline" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePipeline where
        toJSON DeletePipeline'{..}
          = object
              (catMaybes [Just ("pipelineId" .= _dpPipelineId)])

instance ToPath DeletePipeline where
        toPath = const "/"

instance ToQuery DeletePipeline where
        toQuery = const mempty

-- | /See:/ 'deletePipelineResponse' smart constructor.
data DeletePipelineResponse =
  DeletePipelineResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePipelineResponse' with the minimum fields required to make a request.
--
deletePipelineResponse
    :: DeletePipelineResponse
deletePipelineResponse = DeletePipelineResponse'


instance NFData DeletePipelineResponse where
