{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.CancelExportTask
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
module Network.AWS.EC2.V2014_05_01.CancelExportTask where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CancelExportTask = CancelExportTask
    { _cetrExportTaskId :: Text
      -- ^ The ID of the export task. This is the ID returned by
      -- CreateInstanceExportTask.
    } deriving (Generic)

instance ToQuery CancelExportTask where
    toQuery = genericToQuery def

instance AWSRequest CancelExportTask where
    type Sv CancelExportTask = EC2
    type Rs CancelExportTask = CancelExportTaskResponse

    request = post "CancelExportTask"
    response _ _ = return (Right CancelExportTaskResponse)

data CancelExportTaskResponse = CancelExportTaskResponse
    deriving (Eq, Show, Generic)
