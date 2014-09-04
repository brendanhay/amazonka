{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CancelExportTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Cancels an active export task. The request removes all artifacts of the
-- export, including any partially-created Amazon S3 objects. If the export
-- task is complete or is in the process of transferring the final disk image,
-- the command fails and returns an error. Example This example request
-- cancels the export task with the ID export-i-1234wxyz.
-- https://ec2.amazonaws.com/?Action=CancelExportTask
-- &amp;exportTaskId=export-i-1234wxyz &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE true.
module Network.AWS.EC2.V2014_06_15.CancelExportTask
    (
    -- * Request
      CancelExportTask
    -- ** Request constructor
    , cancelExportTask
    -- ** Request lenses
    , cetrExportTaskId

    -- * Response
    , CancelExportTaskResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CancelExportTask' request.
cancelExportTask :: Text -- ^ 'cetrExportTaskId'
                 -> CancelExportTask
cancelExportTask p1 = CancelExportTask
    { _cetrExportTaskId = p1
    }
{-# INLINE cancelExportTask #-}

data CancelExportTask = CancelExportTask
    { _cetrExportTaskId :: Text
      -- ^ The ID of the export task. This is the ID returned by
      -- CreateInstanceExportTask.
    } deriving (Show, Generic)

-- | The ID of the export task. This is the ID returned by
-- CreateInstanceExportTask.
cetrExportTaskId :: Lens' CancelExportTask Text
cetrExportTaskId f x =
    f (_cetrExportTaskId x) <&> \y -> x { _cetrExportTaskId = y }
{-# INLINE cetrExportTaskId #-}

instance ToQuery CancelExportTask where
    toQuery = genericQuery def

data CancelExportTaskResponse = CancelExportTaskResponse
    deriving (Eq, Show, Generic)

instance AWSRequest CancelExportTask where
    type Sv CancelExportTask = EC2
    type Rs CancelExportTask = CancelExportTaskResponse

    request = post "CancelExportTask"
    response _ = nullaryResponse CancelExportTaskResponse
