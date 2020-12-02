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
-- Module      : Network.AWS.IoTAnalytics.DeletePipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified pipeline.
--
--
module Network.AWS.IoTAnalytics.DeletePipeline
    (
    -- * Creating a Request
      deletePipeline
    , DeletePipeline
    -- * Request Lenses
    , dPipelineName

    -- * Destructuring the Response
    , deletePipelineResponse
    , DeletePipelineResponse
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePipeline' smart constructor.
newtype DeletePipeline = DeletePipeline'
  { _dPipelineName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dPipelineName' - The name of the pipeline to delete.
deletePipeline
    :: Text -- ^ 'dPipelineName'
    -> DeletePipeline
deletePipeline pPipelineName_ =
  DeletePipeline' {_dPipelineName = pPipelineName_}


-- | The name of the pipeline to delete.
dPipelineName :: Lens' DeletePipeline Text
dPipelineName = lens _dPipelineName (\ s a -> s{_dPipelineName = a})

instance AWSRequest DeletePipeline where
        type Rs DeletePipeline = DeletePipelineResponse
        request = delete ioTAnalytics
        response = receiveNull DeletePipelineResponse'

instance Hashable DeletePipeline where

instance NFData DeletePipeline where

instance ToHeaders DeletePipeline where
        toHeaders = const mempty

instance ToPath DeletePipeline where
        toPath DeletePipeline'{..}
          = mconcat ["/pipelines/", toBS _dPipelineName]

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
