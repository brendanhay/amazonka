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
    , getStatus
    -- ** Request lenses
    , gsiJobId

    -- * Response
    , GetStatusResponse
    -- ** Response lenses
    , gsoAwsShippingAddress
    , gsoCarrier
    , gsoCreationDate
    , gsoCurrentManifest
    , gsoErrorCount
    , gsoJobId
    , gsoJobType
    , gsoLocationCode
    , gsoLocationMessage
    , gsoLogBucket
    , gsoLogKey
    , gsoProgressCode
    , gsoProgressMessage
    , gsoSignature
    , gsoSignatureFileContents
    , gsoTrackingNumber
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetStatus' request.
getStatus :: Text -- ^ 'gsiJobId'
          -> GetStatus
getStatus p1 = GetStatus
    { _gsiJobId = p1
    }
{-# INLINE getStatus #-}

data GetStatus = GetStatus
    { _gsiJobId :: Text
      -- ^ A unique identifier which refers to a particular job.
    } deriving (Show, Generic)

-- | A unique identifier which refers to a particular job.
gsiJobId :: Lens' GetStatus (Text)
gsiJobId f x =
    f (_gsiJobId x)
        <&> \y -> x { _gsiJobId = y }
{-# INLINE gsiJobId #-}

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

-- | Address you ship your storage device to.
gsoAwsShippingAddress :: Lens' GetStatusResponse (Maybe Text)
gsoAwsShippingAddress f x =
    f (_gsoAwsShippingAddress x)
        <&> \y -> x { _gsoAwsShippingAddress = y }
{-# INLINE gsoAwsShippingAddress #-}

-- | Name of the shipping company. This value is included when the LocationCode
-- is "Returned".
gsoCarrier :: Lens' GetStatusResponse (Maybe Text)
gsoCarrier f x =
    f (_gsoCarrier x)
        <&> \y -> x { _gsoCarrier = y }
{-# INLINE gsoCarrier #-}

-- | Timestamp of the CreateJob request in ISO8601 date format. For example
-- "2010-03-28T20:27:35Z".
gsoCreationDate :: Lens' GetStatusResponse (Maybe ISO8601)
gsoCreationDate f x =
    f (_gsoCreationDate x)
        <&> \y -> x { _gsoCreationDate = y }
{-# INLINE gsoCreationDate #-}

-- | The last manifest submitted, which will be used to process the job.
gsoCurrentManifest :: Lens' GetStatusResponse (Maybe Text)
gsoCurrentManifest f x =
    f (_gsoCurrentManifest x)
        <&> \y -> x { _gsoCurrentManifest = y }
{-# INLINE gsoCurrentManifest #-}

-- | Number of errors. We return this value when the ProgressCode is Success or
-- SuccessWithErrors.
gsoErrorCount :: Lens' GetStatusResponse (Maybe Integer)
gsoErrorCount f x =
    f (_gsoErrorCount x)
        <&> \y -> x { _gsoErrorCount = y }
{-# INLINE gsoErrorCount #-}

-- | A unique identifier which refers to a particular job.
gsoJobId :: Lens' GetStatusResponse (Maybe Text)
gsoJobId f x =
    f (_gsoJobId x)
        <&> \y -> x { _gsoJobId = y }
{-# INLINE gsoJobId #-}

-- | Specifies whether the job to initiate is an import or export job.
gsoJobType :: Lens' GetStatusResponse (Maybe JobType)
gsoJobType f x =
    f (_gsoJobType x)
        <&> \y -> x { _gsoJobType = y }
{-# INLINE gsoJobType #-}

-- | A token representing the location of the storage device, such as "AtAWS".
gsoLocationCode :: Lens' GetStatusResponse (Maybe Text)
gsoLocationCode f x =
    f (_gsoLocationCode x)
        <&> \y -> x { _gsoLocationCode = y }
{-# INLINE gsoLocationCode #-}

-- | A more human readable form of the physical location of the storage device.
gsoLocationMessage :: Lens' GetStatusResponse (Maybe Text)
gsoLocationMessage f x =
    f (_gsoLocationMessage x)
        <&> \y -> x { _gsoLocationMessage = y }
{-# INLINE gsoLocationMessage #-}

-- | Amazon S3 bucket for user logs.
gsoLogBucket :: Lens' GetStatusResponse (Maybe Text)
gsoLogBucket f x =
    f (_gsoLogBucket x)
        <&> \y -> x { _gsoLogBucket = y }
{-# INLINE gsoLogBucket #-}

-- | The key where the user logs were stored.
gsoLogKey :: Lens' GetStatusResponse (Maybe Text)
gsoLogKey f x =
    f (_gsoLogKey x)
        <&> \y -> x { _gsoLogKey = y }
{-# INLINE gsoLogKey #-}

-- | A token representing the state of the job, such as "Started".
gsoProgressCode :: Lens' GetStatusResponse (Maybe Text)
gsoProgressCode f x =
    f (_gsoProgressCode x)
        <&> \y -> x { _gsoProgressCode = y }
{-# INLINE gsoProgressCode #-}

-- | A more human readable form of the job status.
gsoProgressMessage :: Lens' GetStatusResponse (Maybe Text)
gsoProgressMessage f x =
    f (_gsoProgressMessage x)
        <&> \y -> x { _gsoProgressMessage = y }
{-# INLINE gsoProgressMessage #-}

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
gsoSignature :: Lens' GetStatusResponse (Maybe Text)
gsoSignature f x =
    f (_gsoSignature x)
        <&> \y -> x { _gsoSignature = y }
{-# INLINE gsoSignature #-}

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
gsoSignatureFileContents :: Lens' GetStatusResponse (Maybe Text)
gsoSignatureFileContents f x =
    f (_gsoSignatureFileContents x)
        <&> \y -> x { _gsoSignatureFileContents = y }
{-# INLINE gsoSignatureFileContents #-}

-- | The shipping tracking number assigned by AWS Import/Export to the storage
-- device when it's returned to you. We return this value when the
-- LocationCode is "Returned".
gsoTrackingNumber :: Lens' GetStatusResponse (Maybe Text)
gsoTrackingNumber f x =
    f (_gsoTrackingNumber x)
        <&> \y -> x { _gsoTrackingNumber = y }
{-# INLINE gsoTrackingNumber #-}

instance FromXML GetStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetStatus where
    type Sv GetStatus = ImportExport
    type Rs GetStatus = GetStatusResponse

    request = post "GetStatus"
    response _ = xmlResponse
