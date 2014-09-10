{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.GetStatus
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
module Network.AWS.ImportExport.GetStatus
    (
    -- * Request
      GetStatus
    -- ** Request constructor
    , mkGetStatus
    -- ** Request lenses
    , gsJobId

    -- * Response
    , GetStatusResponse
    -- ** Response constructor
    , mkGetStatusResponse
    -- ** Response lenses
    , gsrJobId
    , gsrJobType
    , gsrAwsShippingAddress
    , gsrLocationCode
    , gsrLocationMessage
    , gsrProgressCode
    , gsrProgressMessage
    , gsrCarrier
    , gsrTrackingNumber
    , gsrLogBucket
    , gsrLogKey
    , gsrErrorCount
    , gsrSignature
    , gsrSignatureFileContents
    , gsrCurrentManifest
    , gsrCreationDate
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types
import Network.AWS.Prelude

-- | Input structure for the GetStatus operation.
newtype GetStatus = GetStatus
    { _gsJobId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetStatus' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @JobId ::@ @Text@
--
mkGetStatus :: Text -- ^ 'gsJobId'
            -> GetStatus
mkGetStatus p1 = GetStatus
    { _gsJobId = p1
    }

-- | A unique identifier which refers to a particular job.
gsJobId :: Lens' GetStatus Text
gsJobId = lens _gsJobId (\s a -> s { _gsJobId = a })

instance ToQuery GetStatus where
    toQuery = genericQuery def

-- | Output structure for the GetStatus operation.
data GetStatusResponse = GetStatusResponse
    { _gsrJobId :: !(Maybe Text)
    , _gsrJobType :: Maybe JobType
    , _gsrAwsShippingAddress :: !(Maybe Text)
    , _gsrLocationCode :: !(Maybe Text)
    , _gsrLocationMessage :: !(Maybe Text)
    , _gsrProgressCode :: !(Maybe Text)
    , _gsrProgressMessage :: !(Maybe Text)
    , _gsrCarrier :: !(Maybe Text)
    , _gsrTrackingNumber :: !(Maybe Text)
    , _gsrLogBucket :: !(Maybe Text)
    , _gsrLogKey :: !(Maybe Text)
    , _gsrErrorCount :: !(Maybe Integer)
    , _gsrSignature :: !(Maybe Text)
    , _gsrSignatureFileContents :: !(Maybe Text)
    , _gsrCurrentManifest :: !(Maybe Text)
    , _gsrCreationDate :: !(Maybe ISO8601)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetStatusResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @JobId ::@ @Maybe Text@
--
-- * @JobType ::@ @Maybe JobType@
--
-- * @AwsShippingAddress ::@ @Maybe Text@
--
-- * @LocationCode ::@ @Maybe Text@
--
-- * @LocationMessage ::@ @Maybe Text@
--
-- * @ProgressCode ::@ @Maybe Text@
--
-- * @ProgressMessage ::@ @Maybe Text@
--
-- * @Carrier ::@ @Maybe Text@
--
-- * @TrackingNumber ::@ @Maybe Text@
--
-- * @LogBucket ::@ @Maybe Text@
--
-- * @LogKey ::@ @Maybe Text@
--
-- * @ErrorCount ::@ @Maybe Integer@
--
-- * @Signature ::@ @Maybe Text@
--
-- * @SignatureFileContents ::@ @Maybe Text@
--
-- * @CurrentManifest ::@ @Maybe Text@
--
-- * @CreationDate ::@ @Maybe ISO8601@
--
mkGetStatusResponse :: GetStatusResponse
mkGetStatusResponse = GetStatusResponse
    { _gsrJobId = Nothing
    , _gsrJobType = Nothing
    , _gsrAwsShippingAddress = Nothing
    , _gsrLocationCode = Nothing
    , _gsrLocationMessage = Nothing
    , _gsrProgressCode = Nothing
    , _gsrProgressMessage = Nothing
    , _gsrCarrier = Nothing
    , _gsrTrackingNumber = Nothing
    , _gsrLogBucket = Nothing
    , _gsrLogKey = Nothing
    , _gsrErrorCount = Nothing
    , _gsrSignature = Nothing
    , _gsrSignatureFileContents = Nothing
    , _gsrCurrentManifest = Nothing
    , _gsrCreationDate = Nothing
    }

-- | A unique identifier which refers to a particular job.
gsrJobId :: Lens' GetStatusResponse (Maybe Text)
gsrJobId = lens _gsrJobId (\s a -> s { _gsrJobId = a })

-- | Specifies whether the job to initiate is an import or export job.
gsrJobType :: Lens' GetStatusResponse (Maybe JobType)
gsrJobType = lens _gsrJobType (\s a -> s { _gsrJobType = a })

-- | Address you ship your storage device to.
gsrAwsShippingAddress :: Lens' GetStatusResponse (Maybe Text)
gsrAwsShippingAddress =
    lens _gsrAwsShippingAddress (\s a -> s { _gsrAwsShippingAddress = a })

-- | A token representing the location of the storage device, such as "AtAWS".
gsrLocationCode :: Lens' GetStatusResponse (Maybe Text)
gsrLocationCode = lens _gsrLocationCode (\s a -> s { _gsrLocationCode = a })

-- | A more human readable form of the physical location of the storage device.
gsrLocationMessage :: Lens' GetStatusResponse (Maybe Text)
gsrLocationMessage =
    lens _gsrLocationMessage (\s a -> s { _gsrLocationMessage = a })

-- | A token representing the state of the job, such as "Started".
gsrProgressCode :: Lens' GetStatusResponse (Maybe Text)
gsrProgressCode = lens _gsrProgressCode (\s a -> s { _gsrProgressCode = a })

-- | A more human readable form of the job status.
gsrProgressMessage :: Lens' GetStatusResponse (Maybe Text)
gsrProgressMessage =
    lens _gsrProgressMessage (\s a -> s { _gsrProgressMessage = a })

-- | Name of the shipping company. This value is included when the LocationCode
-- is "Returned".
gsrCarrier :: Lens' GetStatusResponse (Maybe Text)
gsrCarrier = lens _gsrCarrier (\s a -> s { _gsrCarrier = a })

-- | The shipping tracking number assigned by AWS Import/Export to the storage
-- device when it's returned to you. We return this value when the
-- LocationCode is "Returned".
gsrTrackingNumber :: Lens' GetStatusResponse (Maybe Text)
gsrTrackingNumber =
    lens _gsrTrackingNumber (\s a -> s { _gsrTrackingNumber = a })

-- | Amazon S3 bucket for user logs.
gsrLogBucket :: Lens' GetStatusResponse (Maybe Text)
gsrLogBucket = lens _gsrLogBucket (\s a -> s { _gsrLogBucket = a })

-- | The key where the user logs were stored.
gsrLogKey :: Lens' GetStatusResponse (Maybe Text)
gsrLogKey = lens _gsrLogKey (\s a -> s { _gsrLogKey = a })

-- | Number of errors. We return this value when the ProgressCode is Success or
-- SuccessWithErrors.
gsrErrorCount :: Lens' GetStatusResponse (Maybe Integer)
gsrErrorCount = lens _gsrErrorCount (\s a -> s { _gsrErrorCount = a })

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
gsrSignature :: Lens' GetStatusResponse (Maybe Text)
gsrSignature = lens _gsrSignature (\s a -> s { _gsrSignature = a })

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
gsrSignatureFileContents :: Lens' GetStatusResponse (Maybe Text)
gsrSignatureFileContents =
    lens _gsrSignatureFileContents
         (\s a -> s { _gsrSignatureFileContents = a })

-- | The last manifest submitted, which will be used to process the job.
gsrCurrentManifest :: Lens' GetStatusResponse (Maybe Text)
gsrCurrentManifest =
    lens _gsrCurrentManifest (\s a -> s { _gsrCurrentManifest = a })

-- | Timestamp of the CreateJob request in ISO8601 date format. For example
-- "2010-03-28T20:27:35Z".
gsrCreationDate :: Lens' GetStatusResponse (Maybe ISO8601)
gsrCreationDate = lens _gsrCreationDate (\s a -> s { _gsrCreationDate = a })

instance FromXML GetStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetStatus where
    type Sv GetStatus = ImportExport
    type Rs GetStatus = GetStatusResponse

    request = post "GetStatus"
    response _ = xmlResponse
