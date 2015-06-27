{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ImportExport.GetStatus
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns information about a job, including where the job
-- is in the processing pipeline, the status of the results, and the
-- signature value associated with the job. You can only return information
-- about jobs you own.
--
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebGetStatus.html>
module Network.AWS.ImportExport.GetStatus
    (
    -- * Request
      GetStatus
    -- ** Request constructor
    , getStatus
    -- ** Request lenses
    , gsAPIVersion
    , gsJobId

    -- * Response
    , GetStatusResponse
    -- ** Response constructor
    , getStatusResponse
    -- ** Response lenses
    , gsrCarrier
    , gsrSignature
    , gsrTrackingNumber
    , gsrJobType
    , gsrJobId
    , gsrSignatureFileContents
    , gsrErrorCount
    , gsrCurrentManifest
    , gsrArtifactList
    , gsrLogBucket
    , gsrCreationDate
    , gsrProgressCode
    , gsrLocationCode
    , gsrLogKey
    , gsrProgressMessage
    , gsrLocationMessage
    , gsrStatus
    ) where

import           Network.AWS.ImportExport.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input structure for the GetStatus operation.
--
-- /See:/ 'getStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsAPIVersion'
--
-- * 'gsJobId'
data GetStatus = GetStatus'
    { _gsAPIVersion :: !(Maybe Text)
    , _gsJobId      :: !Text
    } deriving (Eq,Read,Show)

-- | 'GetStatus' smart constructor.
getStatus :: Text -> GetStatus
getStatus pJobId =
    GetStatus'
    { _gsAPIVersion = Nothing
    , _gsJobId = pJobId
    }

-- | FIXME: Undocumented member.
gsAPIVersion :: Lens' GetStatus (Maybe Text)
gsAPIVersion = lens _gsAPIVersion (\ s a -> s{_gsAPIVersion = a});

-- | FIXME: Undocumented member.
gsJobId :: Lens' GetStatus Text
gsJobId = lens _gsJobId (\ s a -> s{_gsJobId = a});

instance AWSRequest GetStatus where
        type Sv GetStatus = ImportExport
        type Rs GetStatus = GetStatusResponse
        request = post
        response
          = receiveXMLWrapper "GetStatusResult"
              (\ s h x ->
                 GetStatusResponse' <$>
                   (x .@? "Carrier") <*> (x .@? "Signature") <*>
                     (x .@? "TrackingNumber")
                     <*> (x .@? "JobType")
                     <*> (x .@? "JobId")
                     <*> (x .@? "SignatureFileContents")
                     <*> (x .@? "ErrorCount")
                     <*> (x .@? "CurrentManifest")
                     <*>
                     (x .@? "ArtifactList" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "LogBucket")
                     <*> (x .@? "CreationDate")
                     <*> (x .@? "ProgressCode")
                     <*> (x .@? "LocationCode")
                     <*> (x .@? "LogKey")
                     <*> (x .@? "ProgressMessage")
                     <*> (x .@? "LocationMessage")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetStatus where
        toHeaders = const mempty

instance ToPath GetStatus where
        toPath = const "/"

instance ToQuery GetStatus where
        toQuery GetStatus'{..}
          = mconcat
              ["Operation=GetStatus",
               "Action" =: ("GetStatus" :: ByteString),
               "Version" =: ("2010-06-01" :: ByteString),
               "APIVersion" =: _gsAPIVersion, "JobId" =: _gsJobId]

-- | Output structure for the GetStatus operation.
--
-- /See:/ 'getStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsrCarrier'
--
-- * 'gsrSignature'
--
-- * 'gsrTrackingNumber'
--
-- * 'gsrJobType'
--
-- * 'gsrJobId'
--
-- * 'gsrSignatureFileContents'
--
-- * 'gsrErrorCount'
--
-- * 'gsrCurrentManifest'
--
-- * 'gsrArtifactList'
--
-- * 'gsrLogBucket'
--
-- * 'gsrCreationDate'
--
-- * 'gsrProgressCode'
--
-- * 'gsrLocationCode'
--
-- * 'gsrLogKey'
--
-- * 'gsrProgressMessage'
--
-- * 'gsrLocationMessage'
--
-- * 'gsrStatus'
data GetStatusResponse = GetStatusResponse'
    { _gsrCarrier               :: !(Maybe Text)
    , _gsrSignature             :: !(Maybe Text)
    , _gsrTrackingNumber        :: !(Maybe Text)
    , _gsrJobType               :: !(Maybe JobType)
    , _gsrJobId                 :: !(Maybe Text)
    , _gsrSignatureFileContents :: !(Maybe Text)
    , _gsrErrorCount            :: !(Maybe Int)
    , _gsrCurrentManifest       :: !(Maybe Text)
    , _gsrArtifactList          :: !(Maybe [Artifact])
    , _gsrLogBucket             :: !(Maybe Text)
    , _gsrCreationDate          :: !(Maybe ISO8601)
    , _gsrProgressCode          :: !(Maybe Text)
    , _gsrLocationCode          :: !(Maybe Text)
    , _gsrLogKey                :: !(Maybe Text)
    , _gsrProgressMessage       :: !(Maybe Text)
    , _gsrLocationMessage       :: !(Maybe Text)
    , _gsrStatus                :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetStatusResponse' smart constructor.
getStatusResponse :: Int -> GetStatusResponse
getStatusResponse pStatus =
    GetStatusResponse'
    { _gsrCarrier = Nothing
    , _gsrSignature = Nothing
    , _gsrTrackingNumber = Nothing
    , _gsrJobType = Nothing
    , _gsrJobId = Nothing
    , _gsrSignatureFileContents = Nothing
    , _gsrErrorCount = Nothing
    , _gsrCurrentManifest = Nothing
    , _gsrArtifactList = Nothing
    , _gsrLogBucket = Nothing
    , _gsrCreationDate = Nothing
    , _gsrProgressCode = Nothing
    , _gsrLocationCode = Nothing
    , _gsrLogKey = Nothing
    , _gsrProgressMessage = Nothing
    , _gsrLocationMessage = Nothing
    , _gsrStatus = pStatus
    }

-- | FIXME: Undocumented member.
gsrCarrier :: Lens' GetStatusResponse (Maybe Text)
gsrCarrier = lens _gsrCarrier (\ s a -> s{_gsrCarrier = a});

-- | FIXME: Undocumented member.
gsrSignature :: Lens' GetStatusResponse (Maybe Text)
gsrSignature = lens _gsrSignature (\ s a -> s{_gsrSignature = a});

-- | FIXME: Undocumented member.
gsrTrackingNumber :: Lens' GetStatusResponse (Maybe Text)
gsrTrackingNumber = lens _gsrTrackingNumber (\ s a -> s{_gsrTrackingNumber = a});

-- | FIXME: Undocumented member.
gsrJobType :: Lens' GetStatusResponse (Maybe JobType)
gsrJobType = lens _gsrJobType (\ s a -> s{_gsrJobType = a});

-- | FIXME: Undocumented member.
gsrJobId :: Lens' GetStatusResponse (Maybe Text)
gsrJobId = lens _gsrJobId (\ s a -> s{_gsrJobId = a});

-- | FIXME: Undocumented member.
gsrSignatureFileContents :: Lens' GetStatusResponse (Maybe Text)
gsrSignatureFileContents = lens _gsrSignatureFileContents (\ s a -> s{_gsrSignatureFileContents = a});

-- | FIXME: Undocumented member.
gsrErrorCount :: Lens' GetStatusResponse (Maybe Int)
gsrErrorCount = lens _gsrErrorCount (\ s a -> s{_gsrErrorCount = a});

-- | FIXME: Undocumented member.
gsrCurrentManifest :: Lens' GetStatusResponse (Maybe Text)
gsrCurrentManifest = lens _gsrCurrentManifest (\ s a -> s{_gsrCurrentManifest = a});

-- | FIXME: Undocumented member.
gsrArtifactList :: Lens' GetStatusResponse [Artifact]
gsrArtifactList = lens _gsrArtifactList (\ s a -> s{_gsrArtifactList = a}) . _Default;

-- | FIXME: Undocumented member.
gsrLogBucket :: Lens' GetStatusResponse (Maybe Text)
gsrLogBucket = lens _gsrLogBucket (\ s a -> s{_gsrLogBucket = a});

-- | FIXME: Undocumented member.
gsrCreationDate :: Lens' GetStatusResponse (Maybe UTCTime)
gsrCreationDate = lens _gsrCreationDate (\ s a -> s{_gsrCreationDate = a}) . mapping _Time;

-- | FIXME: Undocumented member.
gsrProgressCode :: Lens' GetStatusResponse (Maybe Text)
gsrProgressCode = lens _gsrProgressCode (\ s a -> s{_gsrProgressCode = a});

-- | FIXME: Undocumented member.
gsrLocationCode :: Lens' GetStatusResponse (Maybe Text)
gsrLocationCode = lens _gsrLocationCode (\ s a -> s{_gsrLocationCode = a});

-- | FIXME: Undocumented member.
gsrLogKey :: Lens' GetStatusResponse (Maybe Text)
gsrLogKey = lens _gsrLogKey (\ s a -> s{_gsrLogKey = a});

-- | FIXME: Undocumented member.
gsrProgressMessage :: Lens' GetStatusResponse (Maybe Text)
gsrProgressMessage = lens _gsrProgressMessage (\ s a -> s{_gsrProgressMessage = a});

-- | FIXME: Undocumented member.
gsrLocationMessage :: Lens' GetStatusResponse (Maybe Text)
gsrLocationMessage = lens _gsrLocationMessage (\ s a -> s{_gsrLocationMessage = a});

-- | FIXME: Undocumented member.
gsrStatus :: Lens' GetStatusResponse Int
gsrStatus = lens _gsrStatus (\ s a -> s{_gsrStatus = a});
