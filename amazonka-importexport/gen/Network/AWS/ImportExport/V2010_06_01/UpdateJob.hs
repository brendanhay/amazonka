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
    , updateJob
    -- ** Request lenses
    , ujiJobId
    , ujiJobType
    , ujiManifest
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

-- | Minimum specification for a 'UpdateJob' request.
updateJob :: Text -- ^ 'ujiJobId'
          -> JobType -- ^ 'ujiJobType'
          -> Text -- ^ 'ujiManifest'
          -> Bool -- ^ 'ujiValidateOnly'
          -> UpdateJob
updateJob p1 p2 p3 p4 = UpdateJob
    { _ujiJobId = p1
    , _ujiJobType = p2
    , _ujiManifest = p3
    , _ujiValidateOnly = p4
    }
{-# INLINE updateJob #-}

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
    } deriving (Show, Generic)

-- | A unique identifier which refers to a particular job.
ujiJobId :: Lens' UpdateJob (Text)
ujiJobId f x =
    f (_ujiJobId x)
        <&> \y -> x { _ujiJobId = y }
{-# INLINE ujiJobId #-}

-- | Specifies whether the job to initiate is an import or export job.
ujiJobType :: Lens' UpdateJob (JobType)
ujiJobType f x =
    f (_ujiJobType x)
        <&> \y -> x { _ujiJobType = y }
{-# INLINE ujiJobType #-}

-- | The UTF-8 encoded text of the manifest file.
ujiManifest :: Lens' UpdateJob (Text)
ujiManifest f x =
    f (_ujiManifest x)
        <&> \y -> x { _ujiManifest = y }
{-# INLINE ujiManifest #-}

-- | Validate the manifest and parameter values in the request but do not
-- actually create a job.
ujiValidateOnly :: Lens' UpdateJob (Bool)
ujiValidateOnly f x =
    f (_ujiValidateOnly x)
        <&> \y -> x { _ujiValidateOnly = y }
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
ujoSuccess f x =
    f (_ujoSuccess x)
        <&> \y -> x { _ujoSuccess = y }
{-# INLINE ujoSuccess #-}

-- | An optional message notifying you of non-fatal issues with the job, such as
-- use of an incompatible Amazon S3 bucket name.
ujoWarningMessage :: Lens' UpdateJobResponse (Maybe Text)
ujoWarningMessage f x =
    f (_ujoWarningMessage x)
        <&> \y -> x { _ujoWarningMessage = y }
{-# INLINE ujoWarningMessage #-}

instance FromXML UpdateJobResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateJob where
    type Sv UpdateJob = ImportExport
    type Rs UpdateJob = UpdateJobResponse

    request = post "UpdateJob"
    response _ = xmlResponse
