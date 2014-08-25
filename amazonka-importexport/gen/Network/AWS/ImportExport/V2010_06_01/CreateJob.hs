{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.V2010_06_01.CreateJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation initiates the process of scheduling an upload or download of
-- your data. You include in the request a manifest that describes the data
-- transfer specifics. The response to the request includes a job ID, which
-- you can use in other operations, a signature that you use to identify your
-- storage device, and the address where you should ship your storage device.
module Network.AWS.ImportExport.V2010_06_01.CreateJob where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateJob' request.
createJob :: JobType -- ^ '_cjiJobType'
          -> Text -- ^ '_cjiManifest'
          -> Bool -- ^ '_cjiValidateOnly'
          -> CreateJob
createJob p1 p2 p3 = CreateJob
    { _cjiJobType = p1
    , _cjiManifest = p2
    , _cjiValidateOnly = p3
    , _cjiManifestAddendum = Nothing
    }

data CreateJob = CreateJob
    { _cjiJobType :: JobType
      -- ^ Specifies whether the job to initiate is an import or export job.
    , _cjiManifest :: Text
      -- ^ The UTF-8 encoded text of the manifest file.
    , _cjiValidateOnly :: Bool
      -- ^ Validate the manifest and parameter values in the request but do
      -- not actually create a job.
    , _cjiManifestAddendum :: Maybe Text
      -- ^ For internal use only.
    } deriving (Show, Generic)

makeLenses ''CreateJob

instance ToQuery CreateJob where
    toQuery = genericQuery def

data CreateJobResponse = CreateJobResponse
    { _cjoAwsShippingAddress :: Maybe Text
      -- ^ Address you ship your storage device to.
    , _cjoJobId :: Maybe Text
      -- ^ A unique identifier which refers to a particular job.
    , _cjoJobType :: Maybe JobType
      -- ^ Specifies whether the job to initiate is an import or export job.
    , _cjoSignature :: Maybe Text
      -- ^ An encrypted code used to authenticate the request and response,
      -- for example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value
      -- is you want to create the signature file yourself. Generally you
      -- should use the SignatureFileContents value.
    , _cjoSignatureFileContents :: Maybe Text
      -- ^ The actual text of the SIGNATURE file to be written to disk.
    , _cjoWarningMessage :: Maybe Text
      -- ^ An optional message notifying you of non-fatal issues with the
      -- job, such as use of an incompatible Amazon S3 bucket name.
    } deriving (Show, Generic)

makeLenses ''CreateJobResponse

instance FromXML CreateJobResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateJob where
    type Sv CreateJob = ImportExport
    type Rs CreateJob = CreateJobResponse

    request = post "CreateJob"
    response _ = xmlResponse
