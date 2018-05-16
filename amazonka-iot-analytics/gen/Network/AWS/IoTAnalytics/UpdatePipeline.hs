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
-- Module      : Network.AWS.IoTAnalytics.UpdatePipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a pipeline.
--
--
module Network.AWS.IoTAnalytics.UpdatePipeline
    (
    -- * Creating a Request
      updatePipeline
    , UpdatePipeline
    -- * Request Lenses
    , upPipelineName
    , upPipelineActivities

    -- * Destructuring the Response
    , updatePipelineResponse
    , UpdatePipelineResponse
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updatePipeline' smart constructor.
data UpdatePipeline = UpdatePipeline'
  { _upPipelineName       :: !Text
  , _upPipelineActivities :: !(List1 PipelineActivity)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upPipelineName' - The name of the pipeline to update.
--
-- * 'upPipelineActivities' - A list of "PipelineActivity" objects. The list can be 1-25 __PipelineActivity__ objects. Activities perform transformations on your messages, such as removing, renaming or adding message attributes; filtering messages based on attribute values; invoking your Lambda functions on messages for advanced processing; or performing mathematical transformations to normalize device data.
updatePipeline
    :: Text -- ^ 'upPipelineName'
    -> NonEmpty PipelineActivity -- ^ 'upPipelineActivities'
    -> UpdatePipeline
updatePipeline pPipelineName_ pPipelineActivities_ =
  UpdatePipeline'
    { _upPipelineName = pPipelineName_
    , _upPipelineActivities = _List1 # pPipelineActivities_
    }


-- | The name of the pipeline to update.
upPipelineName :: Lens' UpdatePipeline Text
upPipelineName = lens _upPipelineName (\ s a -> s{_upPipelineName = a})

-- | A list of "PipelineActivity" objects. The list can be 1-25 __PipelineActivity__ objects. Activities perform transformations on your messages, such as removing, renaming or adding message attributes; filtering messages based on attribute values; invoking your Lambda functions on messages for advanced processing; or performing mathematical transformations to normalize device data.
upPipelineActivities :: Lens' UpdatePipeline (NonEmpty PipelineActivity)
upPipelineActivities = lens _upPipelineActivities (\ s a -> s{_upPipelineActivities = a}) . _List1

instance AWSRequest UpdatePipeline where
        type Rs UpdatePipeline = UpdatePipelineResponse
        request = putJSON ioTAnalytics
        response = receiveNull UpdatePipelineResponse'

instance Hashable UpdatePipeline where

instance NFData UpdatePipeline where

instance ToHeaders UpdatePipeline where
        toHeaders = const mempty

instance ToJSON UpdatePipeline where
        toJSON UpdatePipeline'{..}
          = object
              (catMaybes
                 [Just
                    ("pipelineActivities" .= _upPipelineActivities)])

instance ToPath UpdatePipeline where
        toPath UpdatePipeline'{..}
          = mconcat ["/pipelines/", toBS _upPipelineName]

instance ToQuery UpdatePipeline where
        toQuery = const mempty

-- | /See:/ 'updatePipelineResponse' smart constructor.
data UpdatePipelineResponse =
  UpdatePipelineResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePipelineResponse' with the minimum fields required to make a request.
--
updatePipelineResponse
    :: UpdatePipelineResponse
updatePipelineResponse = UpdatePipelineResponse'


instance NFData UpdatePipelineResponse where
