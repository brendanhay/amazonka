{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.V2010_06_01.GetStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns information about a job, including where the job is
-- in the processing pipeline, the status of the results, and the signature
-- value associated with the job. You can only return information about jobs
-- you own.
module Network.AWS.ImportExport.V2010_06_01.GetStatus where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

data GetStatus = GetStatus
    { _gsiJobId :: Text
      -- ^ A unique identifier which refers to a particular job.
    } deriving (Show, Generic)

makeLenses ''GetStatus

instance ToQuery GetStatus where
    toQuery = genericQuery def

data GetStatusResponse = GetStatusResponse
    { _gsoAwsShippingAddress :: Maybe Text
      -- ^ Address you ship your storage device to.
    , _gsoCarrier :: Maybe Text
      -- ^ Name of the shipping company. This value is included when the
      -- LocationCode is "Returned".
    , _gsoCreationDate :: Maybe ISO8601
      -- ^ Timestamp of the CreateJob request in ISO8601 date format. For
      -- example "2010-03-28T20:27:35Z".
    , _gsoCurrentManifest :: Maybe Text
      -- ^ The last manifest submitted, which will be used to process the
      -- job.
    , _gsoErrorCount :: Maybe Integer
      -- ^ Number of errors. We return this value when the ProgressCode is
      -- Success or SuccessWithErrors.
    , _gsoJobId :: Maybe Text
      -- ^ A unique identifier which refers to a particular job.
    , _gsoJobType :: Maybe JobType
      -- ^ Specifies whether the job to initiate is an import or export job.
    , _gsoLocationCode :: Maybe Text
      -- ^ A token representing the location of the storage device, such as
      -- "AtAWS".
    , _gsoLocationMessage :: Maybe Text
      -- ^ A more human readable form of the physical location of the
      -- storage device.
    , _gsoLogBucket :: Maybe Text
      -- ^ Amazon S3 bucket for user logs.
    , _gsoLogKey :: Maybe Text
      -- ^ The key where the user logs were stored.
    , _gsoProgressCode :: Maybe Text
      -- ^ A token representing the state of the job, such as "Started".
    , _gsoProgressMessage :: Maybe Text
      -- ^ A more human readable form of the job status.
    , _gsoSignature :: Maybe Text
      -- ^ An encrypted code used to authenticate the request and response,
      -- for example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value
      -- is you want to create the signature file yourself. Generally you
      -- should use the SignatureFileContents value.
    , _gsoSignatureFileContents :: Maybe Text
      -- ^ An encrypted code used to authenticate the request and response,
      -- for example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value
      -- is you want to create the signature file yourself. Generally you
      -- should use the SignatureFileContents value.
    , _gsoTrackingNumber :: Maybe Text
      -- ^ The shipping tracking number assigned by AWS Import/Export to the
      -- storage device when it's returned to you. We return this value
      -- when the LocationCode is "Returned".
    } deriving (Show, Generic)

makeLenses ''GetStatusResponse

instance FromXML GetStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetStatus where
    type Sv GetStatus = ImportExport
    type Rs GetStatus = GetStatusResponse

    request = post "GetStatus"
    response _ = xmlResponse
