{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.SetStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Requests that the status of an array of physical or logical pipeline objects
-- be updated in the pipeline. This update may not occur immediately, but is
-- eventually consistent. The status that can be set depends on the type of
-- object, e.g. DataNode or Activity. You cannot perform this operation on
-- FINISHED pipelines and attempting to do so will return an
-- InvalidRequestException.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_SetStatus.html>
module Network.AWS.DataPipeline.SetStatus
    (
    -- * Request
      SetStatus
    -- ** Request constructor
    , setStatus
    -- ** Request lenses
    , ssObjectIds
    , ssPipelineId
    , ssStatus

    -- * Response
    , SetStatusResponse
    -- ** Response constructor
    , setStatusResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data SetStatus = SetStatus
    { _ssObjectIds  :: List "objectIds" Text
    , _ssPipelineId :: Text
    , _ssStatus     :: Text
    } deriving (Eq, Ord, Show)

-- | 'SetStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssObjectIds' @::@ ['Text']
--
-- * 'ssPipelineId' @::@ 'Text'
--
-- * 'ssStatus' @::@ 'Text'
--
setStatus :: Text -- ^ 'ssPipelineId'
          -> Text -- ^ 'ssStatus'
          -> SetStatus
setStatus p1 p2 = SetStatus
    { _ssPipelineId = p1
    , _ssStatus     = p2
    , _ssObjectIds  = mempty
    }

-- | Identifies an array of objects. The corresponding objects can be either
-- physical or components, but not a mix of both types.
ssObjectIds :: Lens' SetStatus [Text]
ssObjectIds = lens _ssObjectIds (\s a -> s { _ssObjectIds = a }) . _List

-- | Identifies the pipeline that contains the objects.
ssPipelineId :: Lens' SetStatus Text
ssPipelineId = lens _ssPipelineId (\s a -> s { _ssPipelineId = a })

-- | Specifies the status to be set on all the objects in 'objectIds'. For
-- components, this can be either 'PAUSE' or 'RESUME'. For instances, this can be
-- either 'CANCEL', 'RERUN', or 'MARK_FINISHED'.
ssStatus :: Lens' SetStatus Text
ssStatus = lens _ssStatus (\s a -> s { _ssStatus = a })

data SetStatusResponse = SetStatusResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetStatusResponse' constructor.
setStatusResponse :: SetStatusResponse
setStatusResponse = SetStatusResponse

instance ToPath SetStatus where
    toPath = const "/"

instance ToQuery SetStatus where
    toQuery = const mempty

instance ToHeaders SetStatus

instance ToJSON SetStatus where
    toJSON SetStatus{..} = object
        [ "pipelineId" .= _ssPipelineId
        , "objectIds"  .= _ssObjectIds
        , "status"     .= _ssStatus
        ]

instance AWSRequest SetStatus where
    type Sv SetStatus = DataPipeline
    type Rs SetStatus = SetStatusResponse

    request  = post "SetStatus"
    response = nullResponse SetStatusResponse
