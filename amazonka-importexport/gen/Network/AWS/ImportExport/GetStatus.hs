{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.GetStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a job, including where the job
-- is in the processing pipeline, the status of the results, and the
-- signature value associated with the job. You can only return information
-- about jobs you own.
--
-- /See:/ <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebGetStatus.html AWS API Reference> for GetStatus.
module Network.AWS.ImportExport.GetStatus
    (
    -- * Creating a Request
      GetStatus
    , getStatus
    -- * Request Lenses
    , gsAPIVersion
    , gsJobId

    -- * Destructuring the Response
    , GetStatusResponse
    , getStatusResponse
    -- * Response Lenses
    , gsrsCarrier
    , gsrsSignature
    , gsrsTrackingNumber
    , gsrsJobType
    , gsrsJobId
    , gsrsSignatureFileContents
    , gsrsErrorCount
    , gsrsCurrentManifest
    , gsrsArtifactList
    , gsrsLogBucket
    , gsrsCreationDate
    , gsrsProgressCode
    , gsrsLocationCode
    , gsrsLogKey
    , gsrsProgressMessage
    , gsrsLocationMessage
    , gsrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetStatus' smart constructor.
getStatus :: Text -> GetStatus
getStatus pJobId_ =
    GetStatus'
    { _gsAPIVersion = Nothing
    , _gsJobId = pJobId_
    }

-- | Undocumented member.
gsAPIVersion :: Lens' GetStatus (Maybe Text)
gsAPIVersion = lens _gsAPIVersion (\ s a -> s{_gsAPIVersion = a});

-- | Undocumented member.
gsJobId :: Lens' GetStatus Text
gsJobId = lens _gsJobId (\ s a -> s{_gsJobId = a});

instance AWSRequest GetStatus where
        type Sv GetStatus = ImportExport
        type Rs GetStatus = GetStatusResponse
        request = postQuery
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
-- * 'gsrsCarrier'
--
-- * 'gsrsSignature'
--
-- * 'gsrsTrackingNumber'
--
-- * 'gsrsJobType'
--
-- * 'gsrsJobId'
--
-- * 'gsrsSignatureFileContents'
--
-- * 'gsrsErrorCount'
--
-- * 'gsrsCurrentManifest'
--
-- * 'gsrsArtifactList'
--
-- * 'gsrsLogBucket'
--
-- * 'gsrsCreationDate'
--
-- * 'gsrsProgressCode'
--
-- * 'gsrsLocationCode'
--
-- * 'gsrsLogKey'
--
-- * 'gsrsProgressMessage'
--
-- * 'gsrsLocationMessage'
--
-- * 'gsrsStatus'
data GetStatusResponse = GetStatusResponse'
    { _gsrsCarrier               :: !(Maybe Text)
    , _gsrsSignature             :: !(Maybe Text)
    , _gsrsTrackingNumber        :: !(Maybe Text)
    , _gsrsJobType               :: !(Maybe JobType)
    , _gsrsJobId                 :: !(Maybe Text)
    , _gsrsSignatureFileContents :: !(Maybe Text)
    , _gsrsErrorCount            :: !(Maybe Int)
    , _gsrsCurrentManifest       :: !(Maybe Text)
    , _gsrsArtifactList          :: !(Maybe [Artifact])
    , _gsrsLogBucket             :: !(Maybe Text)
    , _gsrsCreationDate          :: !(Maybe ISO8601)
    , _gsrsProgressCode          :: !(Maybe Text)
    , _gsrsLocationCode          :: !(Maybe Text)
    , _gsrsLogKey                :: !(Maybe Text)
    , _gsrsProgressMessage       :: !(Maybe Text)
    , _gsrsLocationMessage       :: !(Maybe Text)
    , _gsrsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetStatusResponse' smart constructor.
getStatusResponse :: Int -> GetStatusResponse
getStatusResponse pStatus_ =
    GetStatusResponse'
    { _gsrsCarrier = Nothing
    , _gsrsSignature = Nothing
    , _gsrsTrackingNumber = Nothing
    , _gsrsJobType = Nothing
    , _gsrsJobId = Nothing
    , _gsrsSignatureFileContents = Nothing
    , _gsrsErrorCount = Nothing
    , _gsrsCurrentManifest = Nothing
    , _gsrsArtifactList = Nothing
    , _gsrsLogBucket = Nothing
    , _gsrsCreationDate = Nothing
    , _gsrsProgressCode = Nothing
    , _gsrsLocationCode = Nothing
    , _gsrsLogKey = Nothing
    , _gsrsProgressMessage = Nothing
    , _gsrsLocationMessage = Nothing
    , _gsrsStatus = pStatus_
    }

-- | Undocumented member.
gsrsCarrier :: Lens' GetStatusResponse (Maybe Text)
gsrsCarrier = lens _gsrsCarrier (\ s a -> s{_gsrsCarrier = a});

-- | Undocumented member.
gsrsSignature :: Lens' GetStatusResponse (Maybe Text)
gsrsSignature = lens _gsrsSignature (\ s a -> s{_gsrsSignature = a});

-- | Undocumented member.
gsrsTrackingNumber :: Lens' GetStatusResponse (Maybe Text)
gsrsTrackingNumber = lens _gsrsTrackingNumber (\ s a -> s{_gsrsTrackingNumber = a});

-- | Undocumented member.
gsrsJobType :: Lens' GetStatusResponse (Maybe JobType)
gsrsJobType = lens _gsrsJobType (\ s a -> s{_gsrsJobType = a});

-- | Undocumented member.
gsrsJobId :: Lens' GetStatusResponse (Maybe Text)
gsrsJobId = lens _gsrsJobId (\ s a -> s{_gsrsJobId = a});

-- | Undocumented member.
gsrsSignatureFileContents :: Lens' GetStatusResponse (Maybe Text)
gsrsSignatureFileContents = lens _gsrsSignatureFileContents (\ s a -> s{_gsrsSignatureFileContents = a});

-- | Undocumented member.
gsrsErrorCount :: Lens' GetStatusResponse (Maybe Int)
gsrsErrorCount = lens _gsrsErrorCount (\ s a -> s{_gsrsErrorCount = a});

-- | Undocumented member.
gsrsCurrentManifest :: Lens' GetStatusResponse (Maybe Text)
gsrsCurrentManifest = lens _gsrsCurrentManifest (\ s a -> s{_gsrsCurrentManifest = a});

-- | Undocumented member.
gsrsArtifactList :: Lens' GetStatusResponse [Artifact]
gsrsArtifactList = lens _gsrsArtifactList (\ s a -> s{_gsrsArtifactList = a}) . _Default . _Coerce;

-- | Undocumented member.
gsrsLogBucket :: Lens' GetStatusResponse (Maybe Text)
gsrsLogBucket = lens _gsrsLogBucket (\ s a -> s{_gsrsLogBucket = a});

-- | Undocumented member.
gsrsCreationDate :: Lens' GetStatusResponse (Maybe UTCTime)
gsrsCreationDate = lens _gsrsCreationDate (\ s a -> s{_gsrsCreationDate = a}) . mapping _Time;

-- | Undocumented member.
gsrsProgressCode :: Lens' GetStatusResponse (Maybe Text)
gsrsProgressCode = lens _gsrsProgressCode (\ s a -> s{_gsrsProgressCode = a});

-- | Undocumented member.
gsrsLocationCode :: Lens' GetStatusResponse (Maybe Text)
gsrsLocationCode = lens _gsrsLocationCode (\ s a -> s{_gsrsLocationCode = a});

-- | Undocumented member.
gsrsLogKey :: Lens' GetStatusResponse (Maybe Text)
gsrsLogKey = lens _gsrsLogKey (\ s a -> s{_gsrsLogKey = a});

-- | Undocumented member.
gsrsProgressMessage :: Lens' GetStatusResponse (Maybe Text)
gsrsProgressMessage = lens _gsrsProgressMessage (\ s a -> s{_gsrsProgressMessage = a});

-- | Undocumented member.
gsrsLocationMessage :: Lens' GetStatusResponse (Maybe Text)
gsrsLocationMessage = lens _gsrsLocationMessage (\ s a -> s{_gsrsLocationMessage = a});

-- | Undocumented member.
gsrsStatus :: Lens' GetStatusResponse Int
gsrsStatus = lens _gsrsStatus (\ s a -> s{_gsrsStatus = a});
