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
-- Module      : Network.AWS.DataPipeline.SetStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests that the status of the specified physical or logical pipeline objects be updated in the specified pipeline. This update might not occur immediately, but is eventually consistent. The status that can be set depends on the type of object (for example, DataNode or Activity). You cannot perform this operation on @FINISHED@ pipelines and attempting to do so returns @InvalidRequestException@ .
--
--
module Network.AWS.DataPipeline.SetStatus
    (
    -- * Creating a Request
      setStatus
    , SetStatus
    -- * Request Lenses
    , ssPipelineId
    , ssObjectIds
    , ssStatus

    -- * Destructuring the Response
    , setStatusResponse
    , SetStatusResponse
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for SetStatus.
--
--
--
-- /See:/ 'setStatus' smart constructor.
data SetStatus = SetStatus'
  { _ssPipelineId :: !Text
  , _ssObjectIds  :: ![Text]
  , _ssStatus     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssPipelineId' - The ID of the pipeline that contains the objects.
--
-- * 'ssObjectIds' - The IDs of the objects. The corresponding objects can be either physical or components, but not a mix of both types.
--
-- * 'ssStatus' - The status to be set on all the objects specified in @objectIds@ . For components, use @PAUSE@ or @RESUME@ . For instances, use @TRY_CANCEL@ , @RERUN@ , or @MARK_FINISHED@ .
setStatus
    :: Text -- ^ 'ssPipelineId'
    -> Text -- ^ 'ssStatus'
    -> SetStatus
setStatus pPipelineId_ pStatus_ =
  SetStatus'
    {_ssPipelineId = pPipelineId_, _ssObjectIds = mempty, _ssStatus = pStatus_}


-- | The ID of the pipeline that contains the objects.
ssPipelineId :: Lens' SetStatus Text
ssPipelineId = lens _ssPipelineId (\ s a -> s{_ssPipelineId = a})

-- | The IDs of the objects. The corresponding objects can be either physical or components, but not a mix of both types.
ssObjectIds :: Lens' SetStatus [Text]
ssObjectIds = lens _ssObjectIds (\ s a -> s{_ssObjectIds = a}) . _Coerce

-- | The status to be set on all the objects specified in @objectIds@ . For components, use @PAUSE@ or @RESUME@ . For instances, use @TRY_CANCEL@ , @RERUN@ , or @MARK_FINISHED@ .
ssStatus :: Lens' SetStatus Text
ssStatus = lens _ssStatus (\ s a -> s{_ssStatus = a})

instance AWSRequest SetStatus where
        type Rs SetStatus = SetStatusResponse
        request = postJSON dataPipeline
        response = receiveNull SetStatusResponse'

instance Hashable SetStatus where

instance NFData SetStatus where

instance ToHeaders SetStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.SetStatus" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetStatus where
        toJSON SetStatus'{..}
          = object
              (catMaybes
                 [Just ("pipelineId" .= _ssPipelineId),
                  Just ("objectIds" .= _ssObjectIds),
                  Just ("status" .= _ssStatus)])

instance ToPath SetStatus where
        toPath = const "/"

instance ToQuery SetStatus where
        toQuery = const mempty

-- | /See:/ 'setStatusResponse' smart constructor.
data SetStatusResponse =
  SetStatusResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetStatusResponse' with the minimum fields required to make a request.
--
setStatusResponse
    :: SetStatusResponse
setStatusResponse = SetStatusResponse'


instance NFData SetStatusResponse where
