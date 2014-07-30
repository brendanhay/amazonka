{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.V2010_06_01.CancelJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation cancels a specified job. Only the job owner can cancel it.
-- The operation fails if the job has already started or is complete.
module Network.AWS.ImportExport.V2010_06_01.CancelJob where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ImportExport.V2010_06_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CancelJob = CancelJob
    { _cjjJobId :: Text
      -- ^ A unique identifier which refers to a particular job.
    } deriving (Generic)

instance ToQuery CancelJob where
    toQuery = genericToQuery def

instance AWSRequest CancelJob where
    type Sv CancelJob = ImportExport
    type Rs CancelJob = CancelJobResponse

    request = post "CancelJob"
    response _ = xmlResponse

data CancelJobResponse = CancelJobResponse
    { _cjpSuccess :: Maybe Bool
      -- ^ Specifies whether (true) or not (false) AWS Import/Export updated
      -- your job.
    } deriving (Generic)

instance FromXML CancelJobResponse where
    fromXMLOptions = xmlOptions
