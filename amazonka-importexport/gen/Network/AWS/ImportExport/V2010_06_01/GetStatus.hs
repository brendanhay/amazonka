{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.ImportExport.V2010_06_01.GetStatus
    (
    -- * Request
      GetStatus
    -- ** Request constructor
    , mkGetStatusInput
    -- ** Request lenses
    , gsiJobId

    -- * Response
    , GetStatusResponse
    -- ** Response lenses
    , gsoJobId
    , gsoJobType
    , gsoAwsShippingAddress
    , gsoLocationCode
    , gsoLocationMessage
    , gsoProgressCode
    , gsoProgressMessage
    , gsoCarrier
    , gsoTrackingNumber
    , gsoLogBucket
    , gsoLogKey
    , gsoErrorCount
    , gsoSignature
    , gsoSignatureFileContents
    , gsoCurrentManifest
    , gsoCreationDate
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetStatus' request.
mkGetStatusInput :: Text -- ^ 'gsiJobId'
                 -> GetStatus
mkGetStatusInput p1 = GetStatus
    { _gsiJobId = p1
    }
{-# INLINE mkGetStatusInput #-}

newtype GetStatus = GetStatus
    { _gsiJobId :: Text
      -- ^ A unique identifier which refers to a particular job.
    } deriving (Show, Generic)

-- | A unique identifier which refers to a particular job.
gsiJobId :: Lens' GetStatus (Text)
gsiJobId = lens _gsiJobId (\s a -> s { _gsiJobId = a })
{-# INLINE gsiJobId #-}

instance ToQuery GetStatus where
    toQuery = genericQuery def

data GetStatusResponse = GetStatusResponse
    { _gsoJobId :: Maybe Text
      -- ^ A unique identifier which refers to a particular job.
    , _gsoJobType :: Maybe JobType
      -- ^ Specifies whether the job to initiate is an import or export job.
    , _gsoAwsShippingAddress :: Maybe Text
      -- ^ Address you ship your storage device to.
    , _gsoLocationCode :: Maybe Text
      -- ^ A token representing the location of the storage device, such as
      -- "AtAWS".
    , _gsoLocationMessage :: Maybe Text
      -- ^ A more human readable form of the physical location of the
      -- storage device.
    , _gsoProgressCode :: Maybe Text
      -- ^ A token representing the state of the job, such as "Started".
    , _gsoProgressMessage :: Maybe Text
      -- ^ A more human readable form of the job status.
    , _gsoCarrier :: Maybe Text
      -- ^ Name of the shipping company. This value is included when the
      -- LocationCode is "Returned".
    , _gsoTrackingNumber :: Maybe Text
      -- ^ The shipping tracking number assigned by AWS Import/Export to the
      -- storage device when it's returned to you. We return this value
      -- when the LocationCode is "Returned".
    , _gsoLogBucket :: Maybe Text
      -- ^ Amazon S3 bucket for user logs.
    , _gsoLogKey :: Maybe Text
      -- ^ The key where the user logs were stored.
    , _gsoErrorCount :: Maybe Integer
      -- ^ Number of errors. We return this value when the ProgressCode is
      -- Success or SuccessWithErrors.
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
    , _gsoCurrentManifest :: Maybe Text
      -- ^ The last manifest submitted, which will be used to process the
      -- job.
    , _gsoCreationDate :: Maybe ISO8601
      -- ^ Timestamp of the CreateJob request in ISO8601 date format. For
      -- example "2010-03-28T20:27:35Z".
    } deriving (Show, Generic)

-- | A unique identifier which refers to a particular job.
gsoJobId :: Lens' GetStatusResponse (Maybe Text)
gsoJobId = lens _gsoJobId (\s a -> s { _gsoJobId = a })
{-# INLINE gsoJobId #-}

-- | Specifies whether the job to initiate is an import or export job.
gsoJobType :: Lens' GetStatusResponse (Maybe JobType)
gsoJobType = lens _gsoJobType (\s a -> s { _gsoJobType = a })
{-# INLINE gsoJobType #-}

-- | Address you ship your storage device to.
gsoAwsShippingAddress :: Lens' GetStatusResponse (Maybe Text)
gsoAwsShippingAddress = lens _gsoAwsShippingAddress (\s a -> s { _gsoAwsShippingAddress = a })
{-# INLINE gsoAwsShippingAddress #-}

-- | A token representing the location of the storage device, such as "AtAWS".
gsoLocationCode :: Lens' GetStatusResponse (Maybe Text)
gsoLocationCode = lens _gsoLocationCode (\s a -> s { _gsoLocationCode = a })
{-# INLINE gsoLocationCode #-}

-- | A more human readable form of the physical location of the storage device.
gsoLocationMessage :: Lens' GetStatusResponse (Maybe Text)
gsoLocationMessage = lens _gsoLocationMessage (\s a -> s { _gsoLocationMessage = a })
{-# INLINE gsoLocationMessage #-}

-- | A token representing the state of the job, such as "Started".
gsoProgressCode :: Lens' GetStatusResponse (Maybe Text)
gsoProgressCode = lens _gsoProgressCode (\s a -> s { _gsoProgressCode = a })
{-# INLINE gsoProgressCode #-}

-- | A more human readable form of the job status.
gsoProgressMessage :: Lens' GetStatusResponse (Maybe Text)
gsoProgressMessage = lens _gsoProgressMessage (\s a -> s { _gsoProgressMessage = a })
{-# INLINE gsoProgressMessage #-}

-- | Name of the shipping company. This value is included when the LocationCode
-- is "Returned".
gsoCarrier :: Lens' GetStatusResponse (Maybe Text)
gsoCarrier = lens _gsoCarrier (\s a -> s { _gsoCarrier = a })
{-# INLINE gsoCarrier #-}

-- | The shipping tracking number assigned by AWS Import/Export to the storage
-- device when it's returned to you. We return this value when the
-- LocationCode is "Returned".
gsoTrackingNumber :: Lens' GetStatusResponse (Maybe Text)
gsoTrackingNumber = lens _gsoTrackingNumber (\s a -> s { _gsoTrackingNumber = a })
{-# INLINE gsoTrackingNumber #-}

-- | Amazon S3 bucket for user logs.
gsoLogBucket :: Lens' GetStatusResponse (Maybe Text)
gsoLogBucket = lens _gsoLogBucket (\s a -> s { _gsoLogBucket = a })
{-# INLINE gsoLogBucket #-}

-- | The key where the user logs were stored.
gsoLogKey :: Lens' GetStatusResponse (Maybe Text)
gsoLogKey = lens _gsoLogKey (\s a -> s { _gsoLogKey = a })
{-# INLINE gsoLogKey #-}

-- | Number of errors. We return this value when the ProgressCode is Success or
-- SuccessWithErrors.
gsoErrorCount :: Lens' GetStatusResponse (Maybe Integer)
gsoErrorCount = lens _gsoErrorCount (\s a -> s { _gsoErrorCount = a })
{-# INLINE gsoErrorCount #-}

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
gsoSignature :: Lens' GetStatusResponse (Maybe Text)
gsoSignature = lens _gsoSignature (\s a -> s { _gsoSignature = a })
{-# INLINE gsoSignature #-}

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
gsoSignatureFileContents :: Lens' GetStatusResponse (Maybe Text)
gsoSignatureFileContents = lens _gsoSignatureFileContents (\s a -> s { _gsoSignatureFileContents = a })
{-# INLINE gsoSignatureFileContents #-}

-- | The last manifest submitted, which will be used to process the job.
gsoCurrentManifest :: Lens' GetStatusResponse (Maybe Text)
gsoCurrentManifest = lens _gsoCurrentManifest (\s a -> s { _gsoCurrentManifest = a })
{-# INLINE gsoCurrentManifest #-}

-- | Timestamp of the CreateJob request in ISO8601 date format. For example
-- "2010-03-28T20:27:35Z".
gsoCreationDate :: Lens' GetStatusResponse (Maybe ISO8601)
gsoCreationDate = lens _gsoCreationDate (\s a -> s { _gsoCreationDate = a })
{-# INLINE gsoCreationDate #-}

instance FromXML GetStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetStatus where
    type Sv GetStatus = ImportExport
    type Rs GetStatus = GetStatusResponse

    request = post "GetStatus"
    response _ = xmlResponse
