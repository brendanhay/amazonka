{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.SetStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Requests that the status of the specified physical or logical pipeline
-- objects be updated in the specified pipeline. This update might not
-- occur immediately, but is eventually consistent. The status that can be
-- set depends on the type of object (for example, DataNode or Activity).
-- You cannot perform this operation on @FINISHED@ pipelines and attempting
-- to do so returns @InvalidRequestException@.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_SetStatus.html>
module Network.AWS.DataPipeline.SetStatus
    (
    -- * Request
      SetStatus
    -- ** Request constructor
    , setStatus
    -- ** Request lenses
    , ssrqPipelineId
    , ssrqObjectIds
    , ssrqStatus

    -- * Response
    , SetStatusResponse
    -- ** Response constructor
    , setStatusResponse
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for SetStatus.
--
-- /See:/ 'setStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssrqPipelineId'
--
-- * 'ssrqObjectIds'
--
-- * 'ssrqStatus'
data SetStatus = SetStatus'
    { _ssrqPipelineId :: !Text
    , _ssrqObjectIds  :: ![Text]
    , _ssrqStatus     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetStatus' smart constructor.
setStatus :: Text -> Text -> SetStatus
setStatus pPipelineId_ pStatus_ =
    SetStatus'
    { _ssrqPipelineId = pPipelineId_
    , _ssrqObjectIds = mempty
    , _ssrqStatus = pStatus_
    }

-- | The ID of the pipeline that contains the objects.
ssrqPipelineId :: Lens' SetStatus Text
ssrqPipelineId = lens _ssrqPipelineId (\ s a -> s{_ssrqPipelineId = a});

-- | The IDs of the objects. The corresponding objects can be either physical
-- or components, but not a mix of both types.
ssrqObjectIds :: Lens' SetStatus [Text]
ssrqObjectIds = lens _ssrqObjectIds (\ s a -> s{_ssrqObjectIds = a});

-- | The status to be set on all the objects specified in @objectIds@. For
-- components, use @PAUSE@ or @RESUME@. For instances, use @TRY_CANCEL@,
-- @RERUN@, or @MARK_FINISHED@.
ssrqStatus :: Lens' SetStatus Text
ssrqStatus = lens _ssrqStatus (\ s a -> s{_ssrqStatus = a});

instance AWSRequest SetStatus where
        type Sv SetStatus = DataPipeline
        type Rs SetStatus = SetStatusResponse
        request = postJSON
        response = receiveNull SetStatusResponse'

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
              ["pipelineId" .= _ssrqPipelineId,
               "objectIds" .= _ssrqObjectIds,
               "status" .= _ssrqStatus]

instance ToPath SetStatus where
        toPath = const "/"

instance ToQuery SetStatus where
        toQuery = const mempty

-- | /See:/ 'setStatusResponse' smart constructor.
data SetStatusResponse =
    SetStatusResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetStatusResponse' smart constructor.
setStatusResponse :: SetStatusResponse
setStatusResponse = SetStatusResponse'
