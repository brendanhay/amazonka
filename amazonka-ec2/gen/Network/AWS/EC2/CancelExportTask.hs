{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CancelExportTask
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
-- the command fails and returns an error.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelExportTask.html>
module Network.AWS.EC2.CancelExportTask
    (
    -- * Request
      CancelExportTask
    -- ** Request constructor
    , cancelExportTask
    -- ** Request lenses
    , cetExportTaskId

    -- * Response
    , CancelExportTaskResponse
    -- ** Response constructor
    , cancelExportTaskResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

newtype CancelExportTask = CancelExportTask
    { _cetExportTaskId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'CancelExportTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cetExportTaskId' @::@ 'Text'
--
cancelExportTask :: Text -- ^ 'cetExportTaskId'
                 -> CancelExportTask
cancelExportTask p1 = CancelExportTask
    { _cetExportTaskId = p1
    }

-- | The ID of the export task. This is the ID returned by
-- CreateInstanceExportTask.
cetExportTaskId :: Lens' CancelExportTask Text
cetExportTaskId = lens _cetExportTaskId (\s a -> s { _cetExportTaskId = a })

data CancelExportTaskResponse = CancelExportTaskResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CancelExportTaskResponse' constructor.
cancelExportTaskResponse :: CancelExportTaskResponse
cancelExportTaskResponse = CancelExportTaskResponse

instance ToPath CancelExportTask where
    toPath = const "/"

instance ToQuery CancelExportTask

instance ToHeaders CancelExportTask

instance AWSRequest CancelExportTask where
    type Sv CancelExportTask = EC2
    type Rs CancelExportTask = CancelExportTaskResponse

    request  = post "CancelExportTask"
    response = nullResponse CancelExportTaskResponse
