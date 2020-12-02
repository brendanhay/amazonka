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
-- Module      : Network.AWS.IoTAnalytics.CancelPipelineReprocessing
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the reprocessing of data through the pipeline.
--
--
module Network.AWS.IoTAnalytics.CancelPipelineReprocessing
    (
    -- * Creating a Request
      cancelPipelineReprocessing
    , CancelPipelineReprocessing
    -- * Request Lenses
    , cprPipelineName
    , cprReprocessingId

    -- * Destructuring the Response
    , cancelPipelineReprocessingResponse
    , CancelPipelineReprocessingResponse
    -- * Response Lenses
    , cprrsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelPipelineReprocessing' smart constructor.
data CancelPipelineReprocessing = CancelPipelineReprocessing'
  { _cprPipelineName   :: !Text
  , _cprReprocessingId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelPipelineReprocessing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprPipelineName' - The name of pipeline for which data reprocessing is canceled.
--
-- * 'cprReprocessingId' - The ID of the reprocessing task (returned by "StartPipelineReprocessing").
cancelPipelineReprocessing
    :: Text -- ^ 'cprPipelineName'
    -> Text -- ^ 'cprReprocessingId'
    -> CancelPipelineReprocessing
cancelPipelineReprocessing pPipelineName_ pReprocessingId_ =
  CancelPipelineReprocessing'
    {_cprPipelineName = pPipelineName_, _cprReprocessingId = pReprocessingId_}


-- | The name of pipeline for which data reprocessing is canceled.
cprPipelineName :: Lens' CancelPipelineReprocessing Text
cprPipelineName = lens _cprPipelineName (\ s a -> s{_cprPipelineName = a})

-- | The ID of the reprocessing task (returned by "StartPipelineReprocessing").
cprReprocessingId :: Lens' CancelPipelineReprocessing Text
cprReprocessingId = lens _cprReprocessingId (\ s a -> s{_cprReprocessingId = a})

instance AWSRequest CancelPipelineReprocessing where
        type Rs CancelPipelineReprocessing =
             CancelPipelineReprocessingResponse
        request = delete ioTAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 CancelPipelineReprocessingResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CancelPipelineReprocessing where

instance NFData CancelPipelineReprocessing where

instance ToHeaders CancelPipelineReprocessing where
        toHeaders = const mempty

instance ToPath CancelPipelineReprocessing where
        toPath CancelPipelineReprocessing'{..}
          = mconcat
              ["/pipelines/", toBS _cprPipelineName,
               "/reprocessing/", toBS _cprReprocessingId]

instance ToQuery CancelPipelineReprocessing where
        toQuery = const mempty

-- | /See:/ 'cancelPipelineReprocessingResponse' smart constructor.
newtype CancelPipelineReprocessingResponse = CancelPipelineReprocessingResponse'
  { _cprrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelPipelineReprocessingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprrsResponseStatus' - -- | The response status code.
cancelPipelineReprocessingResponse
    :: Int -- ^ 'cprrsResponseStatus'
    -> CancelPipelineReprocessingResponse
cancelPipelineReprocessingResponse pResponseStatus_ =
  CancelPipelineReprocessingResponse' {_cprrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cprrsResponseStatus :: Lens' CancelPipelineReprocessingResponse Int
cprrsResponseStatus = lens _cprrsResponseStatus (\ s a -> s{_cprrsResponseStatus = a})

instance NFData CancelPipelineReprocessingResponse
         where
