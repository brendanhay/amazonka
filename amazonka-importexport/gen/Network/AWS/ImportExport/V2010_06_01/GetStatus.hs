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
    , mkGetStatus
    -- ** Request lenses
    , gsJobId

    -- * Response
    , GetStatusResponse
    -- ** Response lenses
    , gsrsJobId
    , gsrsJobType
    , gsrsAwsShippingAddress
    , gsrsLocationCode
    , gsrsLocationMessage
    , gsrsProgressCode
    , gsrsProgressMessage
    , gsrsCarrier
    , gsrsTrackingNumber
    , gsrsLogBucket
    , gsrsLogKey
    , gsrsErrorCount
    , gsrsSignature
    , gsrsSignatureFileContents
    , gsrsCurrentManifest
    , gsrsCreationDate
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

-- | Input structure for the GetStatus operation.
newtype GetStatus = GetStatus
    { _gsJobId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetStatus' request.
mkGetStatus :: Text -- ^ 'gsJobId'
            -> GetStatus
mkGetStatus p1 = GetStatus
    { _gsJobId = p1
    }
{-# INLINE mkGetStatus #-}

-- | A unique identifier which refers to a particular job.
gsJobId :: Lens' GetStatus Text
gsJobId = lens _gsJobId (\s a -> s { _gsJobId = a })
{-# INLINE gsJobId #-}

instance ToQuery GetStatus where
    toQuery = genericQuery def

-- | Output structure for the GetStatus operation.
data GetStatusResponse = GetStatusResponse
    { _gsrsJobId :: Maybe Text
    , _gsrsJobType :: Maybe JobType
    , _gsrsAwsShippingAddress :: Maybe Text
    , _gsrsLocationCode :: Maybe Text
    , _gsrsLocationMessage :: Maybe Text
    , _gsrsProgressCode :: Maybe Text
    , _gsrsProgressMessage :: Maybe Text
    , _gsrsCarrier :: Maybe Text
    , _gsrsTrackingNumber :: Maybe Text
    , _gsrsLogBucket :: Maybe Text
    , _gsrsLogKey :: Maybe Text
    , _gsrsErrorCount :: Maybe Integer
    , _gsrsSignature :: Maybe Text
    , _gsrsSignatureFileContents :: Maybe Text
    , _gsrsCurrentManifest :: Maybe Text
    , _gsrsCreationDate :: Maybe ISO8601
    } deriving (Show, Generic)

-- | A unique identifier which refers to a particular job.
gsrsJobId :: Lens' GetStatusResponse (Maybe Text)
gsrsJobId = lens _gsrsJobId (\s a -> s { _gsrsJobId = a })
{-# INLINE gsrsJobId #-}

-- | Specifies whether the job to initiate is an import or export job.
gsrsJobType :: Lens' GetStatusResponse (Maybe JobType)
gsrsJobType = lens _gsrsJobType (\s a -> s { _gsrsJobType = a })
{-# INLINE gsrsJobType #-}

-- | Address you ship your storage device to.
gsrsAwsShippingAddress :: Lens' GetStatusResponse (Maybe Text)
gsrsAwsShippingAddress =
    lens _gsrsAwsShippingAddress (\s a -> s { _gsrsAwsShippingAddress = a })
{-# INLINE gsrsAwsShippingAddress #-}

-- | A token representing the location of the storage device, such as "AtAWS".
gsrsLocationCode :: Lens' GetStatusResponse (Maybe Text)
gsrsLocationCode =
    lens _gsrsLocationCode (\s a -> s { _gsrsLocationCode = a })
{-# INLINE gsrsLocationCode #-}

-- | A more human readable form of the physical location of the storage device.
gsrsLocationMessage :: Lens' GetStatusResponse (Maybe Text)
gsrsLocationMessage =
    lens _gsrsLocationMessage (\s a -> s { _gsrsLocationMessage = a })
{-# INLINE gsrsLocationMessage #-}

-- | A token representing the state of the job, such as "Started".
gsrsProgressCode :: Lens' GetStatusResponse (Maybe Text)
gsrsProgressCode =
    lens _gsrsProgressCode (\s a -> s { _gsrsProgressCode = a })
{-# INLINE gsrsProgressCode #-}

-- | A more human readable form of the job status.
gsrsProgressMessage :: Lens' GetStatusResponse (Maybe Text)
gsrsProgressMessage =
    lens _gsrsProgressMessage (\s a -> s { _gsrsProgressMessage = a })
{-# INLINE gsrsProgressMessage #-}

-- | Name of the shipping company. This value is included when the LocationCode
-- is "Returned".
gsrsCarrier :: Lens' GetStatusResponse (Maybe Text)
gsrsCarrier = lens _gsrsCarrier (\s a -> s { _gsrsCarrier = a })
{-# INLINE gsrsCarrier #-}

-- | The shipping tracking number assigned by AWS Import/Export to the storage
-- device when it's returned to you. We return this value when the
-- LocationCode is "Returned".
gsrsTrackingNumber :: Lens' GetStatusResponse (Maybe Text)
gsrsTrackingNumber =
    lens _gsrsTrackingNumber (\s a -> s { _gsrsTrackingNumber = a })
{-# INLINE gsrsTrackingNumber #-}

-- | Amazon S3 bucket for user logs.
gsrsLogBucket :: Lens' GetStatusResponse (Maybe Text)
gsrsLogBucket = lens _gsrsLogBucket (\s a -> s { _gsrsLogBucket = a })
{-# INLINE gsrsLogBucket #-}

-- | The key where the user logs were stored.
gsrsLogKey :: Lens' GetStatusResponse (Maybe Text)
gsrsLogKey = lens _gsrsLogKey (\s a -> s { _gsrsLogKey = a })
{-# INLINE gsrsLogKey #-}

-- | Number of errors. We return this value when the ProgressCode is Success or
-- SuccessWithErrors.
gsrsErrorCount :: Lens' GetStatusResponse (Maybe Integer)
gsrsErrorCount = lens _gsrsErrorCount (\s a -> s { _gsrsErrorCount = a })
{-# INLINE gsrsErrorCount #-}

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
gsrsSignature :: Lens' GetStatusResponse (Maybe Text)
gsrsSignature = lens _gsrsSignature (\s a -> s { _gsrsSignature = a })
{-# INLINE gsrsSignature #-}

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
gsrsSignatureFileContents :: Lens' GetStatusResponse (Maybe Text)
gsrsSignatureFileContents =
    lens _gsrsSignatureFileContents
         (\s a -> s { _gsrsSignatureFileContents = a })
{-# INLINE gsrsSignatureFileContents #-}

-- | The last manifest submitted, which will be used to process the job.
gsrsCurrentManifest :: Lens' GetStatusResponse (Maybe Text)
gsrsCurrentManifest =
    lens _gsrsCurrentManifest (\s a -> s { _gsrsCurrentManifest = a })
{-# INLINE gsrsCurrentManifest #-}

-- | Timestamp of the CreateJob request in ISO8601 date format. For example
-- "2010-03-28T20:27:35Z".
gsrsCreationDate :: Lens' GetStatusResponse (Maybe ISO8601)
gsrsCreationDate =
    lens _gsrsCreationDate (\s a -> s { _gsrsCreationDate = a })
{-# INLINE gsrsCreationDate #-}

instance FromXML GetStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetStatus where
    type Sv GetStatus = ImportExport
    type Rs GetStatus = GetStatusResponse

    request = post "GetStatus"
    response _ = xmlResponse
