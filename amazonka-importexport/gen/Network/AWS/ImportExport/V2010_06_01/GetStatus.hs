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

data GetStatus = GetStatus
    { _gsiJobId :: Text
      -- ^ A unique identifier which refers to a particular job.
    } deriving (Show, Generic)

-- | A unique identifier which refers to a particular job.
gsiJobId
    :: Functor f
    => (Text
    -> f (Text))
    -> GetStatus
    -> f GetStatus
gsiJobId f x =
    (\y -> x { _gsiJobId = y })
       <$> f (_gsiJobId x)
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
gsoAwsShippingAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoAwsShippingAddress f x =
    (\y -> x { _gsoAwsShippingAddress = y })
       <$> f (_gsoAwsShippingAddress x)
{-# INLINE gsoAwsShippingAddress #-}

-- | Name of the shipping company. This value is included when the LocationCode
-- is "Returned".
gsoCarrier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoCarrier f x =
    (\y -> x { _gsoCarrier = y })
       <$> f (_gsoCarrier x)
{-# INLINE gsoCarrier #-}

-- | Timestamp of the CreateJob request in ISO8601 date format. For example
-- "2010-03-28T20:27:35Z".
gsoCreationDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoCreationDate f x =
    (\y -> x { _gsoCreationDate = y })
       <$> f (_gsoCreationDate x)
{-# INLINE gsoCreationDate #-}

-- | The last manifest submitted, which will be used to process the job.
gsoCurrentManifest
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoCurrentManifest f x =
    (\y -> x { _gsoCurrentManifest = y })
       <$> f (_gsoCurrentManifest x)
{-# INLINE gsoCurrentManifest #-}

-- | Number of errors. We return this value when the ProgressCode is Success or
-- SuccessWithErrors.
gsoErrorCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoErrorCount f x =
    (\y -> x { _gsoErrorCount = y })
       <$> f (_gsoErrorCount x)
{-# INLINE gsoErrorCount #-}

-- | A unique identifier which refers to a particular job.
gsoJobId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoJobId f x =
    (\y -> x { _gsoJobId = y })
       <$> f (_gsoJobId x)
{-# INLINE gsoJobId #-}

-- | Specifies whether the job to initiate is an import or export job.
gsoJobType
    :: Functor f
    => (Maybe JobType
    -> f (Maybe JobType))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoJobType f x =
    (\y -> x { _gsoJobType = y })
       <$> f (_gsoJobType x)
{-# INLINE gsoJobType #-}

-- | A token representing the location of the storage device, such as "AtAWS".
gsoLocationCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoLocationCode f x =
    (\y -> x { _gsoLocationCode = y })
       <$> f (_gsoLocationCode x)
{-# INLINE gsoLocationCode #-}

-- | A more human readable form of the physical location of the storage device.
gsoLocationMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoLocationMessage f x =
    (\y -> x { _gsoLocationMessage = y })
       <$> f (_gsoLocationMessage x)
{-# INLINE gsoLocationMessage #-}

-- | Amazon S3 bucket for user logs.
gsoLogBucket
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoLogBucket f x =
    (\y -> x { _gsoLogBucket = y })
       <$> f (_gsoLogBucket x)
{-# INLINE gsoLogBucket #-}

-- | The key where the user logs were stored.
gsoLogKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoLogKey f x =
    (\y -> x { _gsoLogKey = y })
       <$> f (_gsoLogKey x)
{-# INLINE gsoLogKey #-}

-- | A token representing the state of the job, such as "Started".
gsoProgressCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoProgressCode f x =
    (\y -> x { _gsoProgressCode = y })
       <$> f (_gsoProgressCode x)
{-# INLINE gsoProgressCode #-}

-- | A more human readable form of the job status.
gsoProgressMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoProgressMessage f x =
    (\y -> x { _gsoProgressMessage = y })
       <$> f (_gsoProgressMessage x)
{-# INLINE gsoProgressMessage #-}

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
gsoSignature
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoSignature f x =
    (\y -> x { _gsoSignature = y })
       <$> f (_gsoSignature x)
{-# INLINE gsoSignature #-}

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
gsoSignatureFileContents
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoSignatureFileContents f x =
    (\y -> x { _gsoSignatureFileContents = y })
       <$> f (_gsoSignatureFileContents x)
{-# INLINE gsoSignatureFileContents #-}

-- | The shipping tracking number assigned by AWS Import/Export to the storage
-- device when it's returned to you. We return this value when the
-- LocationCode is "Returned".
gsoTrackingNumber
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetStatusResponse
    -> f GetStatusResponse
gsoTrackingNumber f x =
    (\y -> x { _gsoTrackingNumber = y })
       <$> f (_gsoTrackingNumber x)
{-# INLINE gsoTrackingNumber #-}

instance FromXML GetStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetStatus where
    type Sv GetStatus = ImportExport
    type Rs GetStatus = GetStatusResponse

    request = post "GetStatus"
    response _ = xmlResponse
