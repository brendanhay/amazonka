{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DataPipeline.SetStatus
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

-- | Requests that the status of the specified physical or logical pipeline
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
    , ssPipelineId
    , ssObjectIds
    , ssStatus

    -- * Response
    , SetStatusResponse
    -- ** Response constructor
    , setStatusResponse
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for SetStatus.
--
-- /See:/ 'setStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssPipelineId'
--
-- * 'ssObjectIds'
--
-- * 'ssStatus'
data SetStatus = SetStatus'{_ssPipelineId :: Text, _ssObjectIds :: [Text], _ssStatus :: Text} deriving (Eq, Read, Show)

-- | 'SetStatus' smart constructor.
setStatus :: Text -> Text -> SetStatus
setStatus pPipelineId pStatus = SetStatus'{_ssPipelineId = pPipelineId, _ssObjectIds = mempty, _ssStatus = pStatus};

-- | The ID of the pipeline that contains the objects.
ssPipelineId :: Lens' SetStatus Text
ssPipelineId = lens _ssPipelineId (\ s a -> s{_ssPipelineId = a});

-- | The IDs of the objects. The corresponding objects can be either physical
-- or components, but not a mix of both types.
ssObjectIds :: Lens' SetStatus [Text]
ssObjectIds = lens _ssObjectIds (\ s a -> s{_ssObjectIds = a});

-- | The status to be set on all the objects specified in @objectIds@. For
-- components, use @PAUSE@ or @RESUME@. For instances, use @TRY_CANCEL@,
-- @RERUN@, or @MARK_FINISHED@.
ssStatus :: Lens' SetStatus Text
ssStatus = lens _ssStatus (\ s a -> s{_ssStatus = a});

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
              ["pipelineId" .= _ssPipelineId,
               "objectIds" .= _ssObjectIds, "status" .= _ssStatus]

instance ToPath SetStatus where
        toPath = const "/"

instance ToQuery SetStatus where
        toQuery = const mempty

-- | /See:/ 'setStatusResponse' smart constructor.
data SetStatusResponse = SetStatusResponse' deriving (Eq, Read, Show)

-- | 'SetStatusResponse' smart constructor.
setStatusResponse :: SetStatusResponse
setStatusResponse = SetStatusResponse';
