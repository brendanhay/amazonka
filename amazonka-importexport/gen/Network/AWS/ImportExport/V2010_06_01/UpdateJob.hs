{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.ImportExport.V2010_06_01.UpdateJob
    (
    -- * Request
      UpdateJob
    -- ** Request constructor
    , mkUpdateJobInput
    -- ** Request lenses
    , ujiJobId
    , ujiManifest
    , ujiJobType
    , ujiValidateOnly

    -- * Response
    , UpdateJobResponse
    -- ** Response lenses
    , ujoSuccess
    , ujoWarningMessage
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateJob' request.
mkUpdateJobInput :: Text -- ^ 'ujiJobId'
                 -> Text -- ^ 'ujiManifest'
                 -> JobType -- ^ 'ujiJobType'
                 -> Bool -- ^ 'ujiValidateOnly'
                 -> UpdateJob
mkUpdateJobInput p1 p2 p3 p4 = UpdateJob
    { _ujiJobId = p1
    , _ujiManifest = p2
    , _ujiJobType = p3
    , _ujiValidateOnly = p4
    }
{-# INLINE mkUpdateJobInput #-}

data UpdateJob = UpdateJob
    { _ujiJobId :: Text
      -- ^ A unique identifier which refers to a particular job.
    , _ujiManifest :: Text
      -- ^ The UTF-8 encoded text of the manifest file.
    , _ujiJobType :: JobType
      -- ^ Specifies whether the job to initiate is an import or export job.
    , _ujiValidateOnly :: Bool
      -- ^ Validate the manifest and parameter values in the request but do
      -- not actually create a job.
    } deriving (Show, Generic)

-- | A unique identifier which refers to a particular job.
ujiJobId :: Lens' UpdateJob (Text)
ujiJobId = lens _ujiJobId (\s a -> s { _ujiJobId = a })
{-# INLINE ujiJobId #-}

-- | The UTF-8 encoded text of the manifest file.
ujiManifest :: Lens' UpdateJob (Text)
ujiManifest = lens _ujiManifest (\s a -> s { _ujiManifest = a })
{-# INLINE ujiManifest #-}

-- | Specifies whether the job to initiate is an import or export job.
ujiJobType :: Lens' UpdateJob (JobType)
ujiJobType = lens _ujiJobType (\s a -> s { _ujiJobType = a })
{-# INLINE ujiJobType #-}

-- | Validate the manifest and parameter values in the request but do not
-- actually create a job.
ujiValidateOnly :: Lens' UpdateJob (Bool)
ujiValidateOnly = lens _ujiValidateOnly (\s a -> s { _ujiValidateOnly = a })
{-# INLINE ujiValidateOnly #-}

instance ToQuery UpdateJob where
    toQuery = genericQuery def

data UpdateJobResponse = UpdateJobResponse
    { _ujoSuccess :: Maybe Bool
      -- ^ Specifies whether (true) or not (false) AWS Import/Export updated
      -- your job.
    , _ujoWarningMessage :: Maybe Text
      -- ^ An optional message notifying you of non-fatal issues with the
      -- job, such as use of an incompatible Amazon S3 bucket name.
    } deriving (Show, Generic)

-- | Specifies whether (true) or not (false) AWS Import/Export updated your job.
ujoSuccess :: Lens' UpdateJobResponse (Maybe Bool)
ujoSuccess = lens _ujoSuccess (\s a -> s { _ujoSuccess = a })
{-# INLINE ujoSuccess #-}

-- | An optional message notifying you of non-fatal issues with the job, such as
-- use of an incompatible Amazon S3 bucket name.
ujoWarningMessage :: Lens' UpdateJobResponse (Maybe Text)
ujoWarningMessage = lens _ujoWarningMessage (\s a -> s { _ujoWarningMessage = a })
{-# INLINE ujoWarningMessage #-}

instance FromXML UpdateJobResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateJob where
    type Sv UpdateJob = ImportExport
    type Rs UpdateJob = UpdateJobResponse

    request = post "UpdateJob"
    response _ = xmlResponse
