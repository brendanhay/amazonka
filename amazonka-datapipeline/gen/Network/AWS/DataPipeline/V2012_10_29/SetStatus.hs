{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.V2012_10_29.SetStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests that the status of an array of physical or logical pipeline
-- objects be updated in the pipeline. This update may not occur immediately,
-- but is eventually consistent. The status that can be set depends on the
-- type of object. POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: DataPipeline.SetStatus Content-Length: 100 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-0634701J7KEXAMPLE",
-- "objectIds": ["o-08600941GHJWMBR9E2"], "status": "pause"} x-amzn-RequestId:
-- e83b8ab7-076a-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 0 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT Unexpected response: 200, OK, undefined.
module Network.AWS.DataPipeline.V2012_10_29.SetStatus
    (
    -- * Request
      SetStatus
    -- ** Request constructor
    , setStatus
    -- ** Request lenses
    , ssiPipelineId
    , ssiObjectIds
    , ssiStatus

    -- * Response
    , SetStatusResponse
    ) where

import           Network.AWS.DataPipeline.V2012_10_29.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'SetStatus' request.
setStatus :: Text -- ^ 'ssiPipelineId'
          -> [Text] -- ^ 'ssiObjectIds'
          -> Text -- ^ 'ssiStatus'
          -> SetStatus
setStatus p1 p2 p3 = SetStatus
    { _ssiPipelineId = p1
    , _ssiObjectIds = p2
    , _ssiStatus = p3
    }
{-# INLINE setStatus #-}

data SetStatus = SetStatus
    { _ssiPipelineId :: Text
      -- ^ Identifies the pipeline that contains the objects.
    , _ssiObjectIds :: [Text]
      -- ^ Identifies an array of objects. The corresponding objects can be
      -- either physical or components, but not a mix of both types.
    , _ssiStatus :: Text
      -- ^ Specifies the status to be set on all the objects in objectIds.
      -- For components, this can be either PAUSE or RESUME. For
      -- instances, this can be either CANCEL, RERUN, or MARK_FINISHED.
    } deriving (Show, Generic)

-- | Identifies the pipeline that contains the objects.
ssiPipelineId :: Lens' SetStatus (Text)
ssiPipelineId f x =
    f (_ssiPipelineId x)
        <&> \y -> x { _ssiPipelineId = y }
{-# INLINE ssiPipelineId #-}

-- | Identifies an array of objects. The corresponding objects can be either
-- physical or components, but not a mix of both types.
ssiObjectIds :: Lens' SetStatus ([Text])
ssiObjectIds f x =
    f (_ssiObjectIds x)
        <&> \y -> x { _ssiObjectIds = y }
{-# INLINE ssiObjectIds #-}

-- | Specifies the status to be set on all the objects in objectIds. For
-- components, this can be either PAUSE or RESUME. For instances, this can be
-- either CANCEL, RERUN, or MARK_FINISHED.
ssiStatus :: Lens' SetStatus (Text)
ssiStatus f x =
    f (_ssiStatus x)
        <&> \y -> x { _ssiStatus = y }
{-# INLINE ssiStatus #-}

instance ToPath SetStatus

instance ToQuery SetStatus

instance ToHeaders SetStatus

instance ToJSON SetStatus

data SetStatusResponse = SetStatusResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetStatus where
    type Sv SetStatus = DataPipeline
    type Rs SetStatus = SetStatusResponse

    request = get
    response _ = nullaryResponse SetStatusResponse
