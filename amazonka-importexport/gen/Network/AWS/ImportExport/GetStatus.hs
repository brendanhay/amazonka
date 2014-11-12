{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      GetStatusInput
    -- ** Request constructor
    , getStatusInput
    -- ** Request lenses
    , gsiJobId

    -- * Response
    , GetStatusOutput
    -- ** Response constructor
    , getStatusOutput
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types

newtype GetStatusInput = GetStatusInput
    { _gsiJobId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetStatusInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsiJobId' @::@ 'Text'
--
getStatusInput :: Text -- ^ 'gsiJobId'
               -> GetStatusInput
getStatusInput p1 = GetStatusInput
    { _gsiJobId = p1
    }

gsiJobId :: Lens' GetStatusInput Text
gsiJobId = lens _gsiJobId (\s a -> s { _gsiJobId = a })

instance ToQuery GetStatusInput

instance ToPath GetStatusInput where
    toPath = const "/"

data GetStatusOutput = GetStatusOutput
    { _gsoAwsShippingAddress    :: Maybe Text
    , _gsoCarrier               :: Maybe Text
    , _gsoCreationDate          :: Maybe RFC822
    , _gsoCurrentManifest       :: Maybe Text
    , _gsoErrorCount            :: Maybe Int
    , _gsoJobId                 :: Maybe Text
    , _gsoJobType               :: Maybe Text
    , _gsoLocationCode          :: Maybe Text
    , _gsoLocationMessage       :: Maybe Text
    , _gsoLogBucket             :: Maybe Text
    , _gsoLogKey                :: Maybe Text
    , _gsoProgressCode          :: Maybe Text
    , _gsoProgressMessage       :: Maybe Text
    , _gsoSignature             :: Maybe Text
    , _gsoSignatureFileContents :: Maybe Text
    , _gsoTrackingNumber        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetStatusOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsoAwsShippingAddress' @::@ 'Maybe' 'Text'
--
-- * 'gsoCarrier' @::@ 'Maybe' 'Text'
--
-- * 'gsoCreationDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'gsoCurrentManifest' @::@ 'Maybe' 'Text'
--
-- * 'gsoErrorCount' @::@ 'Maybe' 'Int'
--
-- * 'gsoJobId' @::@ 'Maybe' 'Text'
--
-- * 'gsoJobType' @::@ 'Maybe' 'Text'
--
-- * 'gsoLocationCode' @::@ 'Maybe' 'Text'
--
-- * 'gsoLocationMessage' @::@ 'Maybe' 'Text'
--
-- * 'gsoLogBucket' @::@ 'Maybe' 'Text'
--
-- * 'gsoLogKey' @::@ 'Maybe' 'Text'
--
-- * 'gsoProgressCode' @::@ 'Maybe' 'Text'
--
-- * 'gsoProgressMessage' @::@ 'Maybe' 'Text'
--
-- * 'gsoSignature' @::@ 'Maybe' 'Text'
--
-- * 'gsoSignatureFileContents' @::@ 'Maybe' 'Text'
--
-- * 'gsoTrackingNumber' @::@ 'Maybe' 'Text'
--
getStatusOutput :: GetStatusOutput
getStatusOutput = GetStatusOutput
    { _gsoJobId                 = Nothing
    , _gsoJobType               = Nothing
    , _gsoAwsShippingAddress    = Nothing
    , _gsoLocationCode          = Nothing
    , _gsoLocationMessage       = Nothing
    , _gsoProgressCode          = Nothing
    , _gsoProgressMessage       = Nothing
    , _gsoCarrier               = Nothing
    , _gsoTrackingNumber        = Nothing
    , _gsoLogBucket             = Nothing
    , _gsoLogKey                = Nothing
    , _gsoErrorCount            = Nothing
    , _gsoSignature             = Nothing
    , _gsoSignatureFileContents = Nothing
    , _gsoCurrentManifest       = Nothing
    , _gsoCreationDate          = Nothing
    }

gsoAwsShippingAddress :: Lens' GetStatusOutput (Maybe Text)
gsoAwsShippingAddress =
    lens _gsoAwsShippingAddress (\s a -> s { _gsoAwsShippingAddress = a })

gsoCarrier :: Lens' GetStatusOutput (Maybe Text)
gsoCarrier = lens _gsoCarrier (\s a -> s { _gsoCarrier = a })

gsoCreationDate :: Lens' GetStatusOutput (Maybe UTCTime)
gsoCreationDate = lens _gsoCreationDate (\s a -> s { _gsoCreationDate = a })
    . mapping _Time

gsoCurrentManifest :: Lens' GetStatusOutput (Maybe Text)
gsoCurrentManifest =
    lens _gsoCurrentManifest (\s a -> s { _gsoCurrentManifest = a })

gsoErrorCount :: Lens' GetStatusOutput (Maybe Int)
gsoErrorCount = lens _gsoErrorCount (\s a -> s { _gsoErrorCount = a })

gsoJobId :: Lens' GetStatusOutput (Maybe Text)
gsoJobId = lens _gsoJobId (\s a -> s { _gsoJobId = a })

gsoJobType :: Lens' GetStatusOutput (Maybe Text)
gsoJobType = lens _gsoJobType (\s a -> s { _gsoJobType = a })

gsoLocationCode :: Lens' GetStatusOutput (Maybe Text)
gsoLocationCode = lens _gsoLocationCode (\s a -> s { _gsoLocationCode = a })

gsoLocationMessage :: Lens' GetStatusOutput (Maybe Text)
gsoLocationMessage =
    lens _gsoLocationMessage (\s a -> s { _gsoLocationMessage = a })

gsoLogBucket :: Lens' GetStatusOutput (Maybe Text)
gsoLogBucket = lens _gsoLogBucket (\s a -> s { _gsoLogBucket = a })

gsoLogKey :: Lens' GetStatusOutput (Maybe Text)
gsoLogKey = lens _gsoLogKey (\s a -> s { _gsoLogKey = a })

gsoProgressCode :: Lens' GetStatusOutput (Maybe Text)
gsoProgressCode = lens _gsoProgressCode (\s a -> s { _gsoProgressCode = a })

gsoProgressMessage :: Lens' GetStatusOutput (Maybe Text)
gsoProgressMessage =
    lens _gsoProgressMessage (\s a -> s { _gsoProgressMessage = a })

gsoSignature :: Lens' GetStatusOutput (Maybe Text)
gsoSignature = lens _gsoSignature (\s a -> s { _gsoSignature = a })

gsoSignatureFileContents :: Lens' GetStatusOutput (Maybe Text)
gsoSignatureFileContents =
    lens _gsoSignatureFileContents
        (\s a -> s { _gsoSignatureFileContents = a })

gsoTrackingNumber :: Lens' GetStatusOutput (Maybe Text)
gsoTrackingNumber =
    lens _gsoTrackingNumber (\s a -> s { _gsoTrackingNumber = a })

instance FromXML GetStatusOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetStatusOutput"

instance AWSRequest GetStatusInput where
    type Sv GetStatusInput = ImportExport
    type Rs GetStatusInput = GetStatusOutput

    request  = post "GetStatus"
    response = xmlResponse $ \h x -> GetStatusOutput
        <$> x %| "AwsShippingAddress"
        <*> x %| "Carrier"
        <*> x %| "CreationDate"
        <*> x %| "CurrentManifest"
        <*> x %| "ErrorCount"
        <*> x %| "JobId"
        <*> x %| "JobType"
        <*> x %| "LocationCode"
        <*> x %| "LocationMessage"
        <*> x %| "LogBucket"
        <*> x %| "LogKey"
        <*> x %| "ProgressCode"
        <*> x %| "ProgressMessage"
        <*> x %| "Signature"
        <*> x %| "SignatureFileContents"
        <*> x %| "TrackingNumber"
