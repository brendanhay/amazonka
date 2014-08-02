{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ImportExport.V2010_06_01.UpdateJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | You use this operation to change the parameters specified in the original
-- manifest file by supplying a new manifest file. The manifest file attached
-- to this request replaces the original manifest file. You can only use the
-- operation after a CreateJob request but before the data transfer starts and
-- you can only use it on jobs you own.
module Network.AWS.ImportExport.V2010_06_01.UpdateJob where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

data UpdateJob = UpdateJob
    { _ujiJobId :: Text
      -- ^ A unique identifier which refers to a particular job.
    , _ujiJobType :: JobType
      -- ^ Specifies whether the job to initiate is an import or export job.
    , _ujiManifest :: Text
      -- ^ The UTF-8 encoded text of the manifest file.
    , _ujiValidateOnly :: Bool
      -- ^ Validate the manifest and parameter values in the request but do
      -- not actually create a job.
    } deriving (Generic)

makeLenses ''UpdateJob

instance ToQuery UpdateJob where
    toQuery = genericToQuery def

data UpdateJobResponse = UpdateJobResponse
    { _ujoSuccess :: Maybe Bool
      -- ^ Specifies whether (true) or not (false) AWS Import/Export updated
      -- your job.
    , _ujoWarningMessage :: Maybe Text
      -- ^ An optional message notifying you of non-fatal issues with the
      -- job, such as use of an incompatible Amazon S3 bucket name.
    } deriving (Generic)

makeLenses ''UpdateJobResponse

instance FromXML UpdateJobResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateJob where
    type Sv UpdateJob = ImportExport
    type Rs UpdateJob = UpdateJobResponse

    request = post "UpdateJob"
    response _ = xmlResponse
