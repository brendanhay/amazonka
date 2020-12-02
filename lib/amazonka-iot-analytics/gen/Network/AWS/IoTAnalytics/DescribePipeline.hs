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
-- Module      : Network.AWS.IoTAnalytics.DescribePipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a pipeline.
--
--
module Network.AWS.IoTAnalytics.DescribePipeline
    (
    -- * Creating a Request
      describePipeline
    , DescribePipeline
    -- * Request Lenses
    , dpPipelineName

    -- * Destructuring the Response
    , describePipelineResponse
    , DescribePipelineResponse
    -- * Response Lenses
    , dprsPipeline
    , dprsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePipeline' smart constructor.
newtype DescribePipeline = DescribePipeline'
  { _dpPipelineName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpPipelineName' - The name of the pipeline whose information is retrieved.
describePipeline
    :: Text -- ^ 'dpPipelineName'
    -> DescribePipeline
describePipeline pPipelineName_ =
  DescribePipeline' {_dpPipelineName = pPipelineName_}


-- | The name of the pipeline whose information is retrieved.
dpPipelineName :: Lens' DescribePipeline Text
dpPipelineName = lens _dpPipelineName (\ s a -> s{_dpPipelineName = a})

instance AWSRequest DescribePipeline where
        type Rs DescribePipeline = DescribePipelineResponse
        request = get ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 DescribePipelineResponse' <$>
                   (x .?> "pipeline") <*> (pure (fromEnum s)))

instance Hashable DescribePipeline where

instance NFData DescribePipeline where

instance ToHeaders DescribePipeline where
        toHeaders = const mempty

instance ToPath DescribePipeline where
        toPath DescribePipeline'{..}
          = mconcat ["/pipelines/", toBS _dpPipelineName]

instance ToQuery DescribePipeline where
        toQuery = const mempty

-- | /See:/ 'describePipelineResponse' smart constructor.
data DescribePipelineResponse = DescribePipelineResponse'
  { _dprsPipeline       :: !(Maybe Pipeline)
  , _dprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePipelineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsPipeline' - A "Pipeline" object that contains information about the pipeline.
--
-- * 'dprsResponseStatus' - -- | The response status code.
describePipelineResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DescribePipelineResponse
describePipelineResponse pResponseStatus_ =
  DescribePipelineResponse'
    {_dprsPipeline = Nothing, _dprsResponseStatus = pResponseStatus_}


-- | A "Pipeline" object that contains information about the pipeline.
dprsPipeline :: Lens' DescribePipelineResponse (Maybe Pipeline)
dprsPipeline = lens _dprsPipeline (\ s a -> s{_dprsPipeline = a})

-- | -- | The response status code.
dprsResponseStatus :: Lens' DescribePipelineResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a})

instance NFData DescribePipelineResponse where
