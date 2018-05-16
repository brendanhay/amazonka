{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.GetStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a job, including where the job is in the processing pipeline, the status of the results, and the signature value associated with the job. You can only return information about jobs you own.
module Network.AWS.ImportExport.GetStatus
    (
    -- * Creating a Request
      getStatus
    , GetStatus
    -- * Request Lenses
    , gsAPIVersion
    , gsJobId

    -- * Destructuring the Response
    , getStatusResponse
    , GetStatusResponse
    -- * Response Lenses
    , gsrsCarrier
    , gsrsTrackingNumber
    , gsrsSignature
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
    , gsrsLocationMessage
    , gsrsProgressMessage
    , gsrsResponseStatus
    ) where

import Network.AWS.ImportExport.Types
import Network.AWS.ImportExport.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input structure for the GetStatus operation.
--
-- /See:/ 'getStatus' smart constructor.
data GetStatus = GetStatus'
  { _gsAPIVersion :: !(Maybe Text)
  , _gsJobId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsAPIVersion' - Undocumented member.
--
-- * 'gsJobId' - Undocumented member.
getStatus
    :: Text -- ^ 'gsJobId'
    -> GetStatus
getStatus pJobId_ = GetStatus' {_gsAPIVersion = Nothing, _gsJobId = pJobId_}


-- | Undocumented member.
gsAPIVersion :: Lens' GetStatus (Maybe Text)
gsAPIVersion = lens _gsAPIVersion (\ s a -> s{_gsAPIVersion = a})

-- | Undocumented member.
gsJobId :: Lens' GetStatus Text
gsJobId = lens _gsJobId (\ s a -> s{_gsJobId = a})

instance AWSRequest GetStatus where
        type Rs GetStatus = GetStatusResponse
        request = postQuery importExport
        response
          = receiveXMLWrapper "GetStatusResult"
              (\ s h x ->
                 GetStatusResponse' <$>
                   (x .@? "Carrier") <*> (x .@? "TrackingNumber") <*>
                     (x .@? "Signature")
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
                     <*> (x .@? "LocationMessage")
                     <*> (x .@? "ProgressMessage")
                     <*> (pure (fromEnum s)))

instance Hashable GetStatus where

instance NFData GetStatus where

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
data GetStatusResponse = GetStatusResponse'
  { _gsrsCarrier               :: !(Maybe Text)
  , _gsrsTrackingNumber        :: !(Maybe Text)
  , _gsrsSignature             :: !(Maybe Text)
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
  , _gsrsLocationMessage       :: !(Maybe Text)
  , _gsrsProgressMessage       :: !(Maybe Text)
  , _gsrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrsCarrier' - Undocumented member.
--
-- * 'gsrsTrackingNumber' - Undocumented member.
--
-- * 'gsrsSignature' - Undocumented member.
--
-- * 'gsrsJobType' - Undocumented member.
--
-- * 'gsrsJobId' - Undocumented member.
--
-- * 'gsrsSignatureFileContents' - Undocumented member.
--
-- * 'gsrsErrorCount' - Undocumented member.
--
-- * 'gsrsCurrentManifest' - Undocumented member.
--
-- * 'gsrsArtifactList' - Undocumented member.
--
-- * 'gsrsLogBucket' - Undocumented member.
--
-- * 'gsrsCreationDate' - Undocumented member.
--
-- * 'gsrsProgressCode' - Undocumented member.
--
-- * 'gsrsLocationCode' - Undocumented member.
--
-- * 'gsrsLogKey' - Undocumented member.
--
-- * 'gsrsLocationMessage' - Undocumented member.
--
-- * 'gsrsProgressMessage' - Undocumented member.
--
-- * 'gsrsResponseStatus' - -- | The response status code.
getStatusResponse
    :: Int -- ^ 'gsrsResponseStatus'
    -> GetStatusResponse
getStatusResponse pResponseStatus_ =
  GetStatusResponse'
    { _gsrsCarrier = Nothing
    , _gsrsTrackingNumber = Nothing
    , _gsrsSignature = Nothing
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
    , _gsrsLocationMessage = Nothing
    , _gsrsProgressMessage = Nothing
    , _gsrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
gsrsCarrier :: Lens' GetStatusResponse (Maybe Text)
gsrsCarrier = lens _gsrsCarrier (\ s a -> s{_gsrsCarrier = a})

-- | Undocumented member.
gsrsTrackingNumber :: Lens' GetStatusResponse (Maybe Text)
gsrsTrackingNumber = lens _gsrsTrackingNumber (\ s a -> s{_gsrsTrackingNumber = a})

-- | Undocumented member.
gsrsSignature :: Lens' GetStatusResponse (Maybe Text)
gsrsSignature = lens _gsrsSignature (\ s a -> s{_gsrsSignature = a})

-- | Undocumented member.
gsrsJobType :: Lens' GetStatusResponse (Maybe JobType)
gsrsJobType = lens _gsrsJobType (\ s a -> s{_gsrsJobType = a})

-- | Undocumented member.
gsrsJobId :: Lens' GetStatusResponse (Maybe Text)
gsrsJobId = lens _gsrsJobId (\ s a -> s{_gsrsJobId = a})

-- | Undocumented member.
gsrsSignatureFileContents :: Lens' GetStatusResponse (Maybe Text)
gsrsSignatureFileContents = lens _gsrsSignatureFileContents (\ s a -> s{_gsrsSignatureFileContents = a})

-- | Undocumented member.
gsrsErrorCount :: Lens' GetStatusResponse (Maybe Int)
gsrsErrorCount = lens _gsrsErrorCount (\ s a -> s{_gsrsErrorCount = a})

-- | Undocumented member.
gsrsCurrentManifest :: Lens' GetStatusResponse (Maybe Text)
gsrsCurrentManifest = lens _gsrsCurrentManifest (\ s a -> s{_gsrsCurrentManifest = a})

-- | Undocumented member.
gsrsArtifactList :: Lens' GetStatusResponse [Artifact]
gsrsArtifactList = lens _gsrsArtifactList (\ s a -> s{_gsrsArtifactList = a}) . _Default . _Coerce

-- | Undocumented member.
gsrsLogBucket :: Lens' GetStatusResponse (Maybe Text)
gsrsLogBucket = lens _gsrsLogBucket (\ s a -> s{_gsrsLogBucket = a})

-- | Undocumented member.
gsrsCreationDate :: Lens' GetStatusResponse (Maybe UTCTime)
gsrsCreationDate = lens _gsrsCreationDate (\ s a -> s{_gsrsCreationDate = a}) . mapping _Time

-- | Undocumented member.
gsrsProgressCode :: Lens' GetStatusResponse (Maybe Text)
gsrsProgressCode = lens _gsrsProgressCode (\ s a -> s{_gsrsProgressCode = a})

-- | Undocumented member.
gsrsLocationCode :: Lens' GetStatusResponse (Maybe Text)
gsrsLocationCode = lens _gsrsLocationCode (\ s a -> s{_gsrsLocationCode = a})

-- | Undocumented member.
gsrsLogKey :: Lens' GetStatusResponse (Maybe Text)
gsrsLogKey = lens _gsrsLogKey (\ s a -> s{_gsrsLogKey = a})

-- | Undocumented member.
gsrsLocationMessage :: Lens' GetStatusResponse (Maybe Text)
gsrsLocationMessage = lens _gsrsLocationMessage (\ s a -> s{_gsrsLocationMessage = a})

-- | Undocumented member.
gsrsProgressMessage :: Lens' GetStatusResponse (Maybe Text)
gsrsProgressMessage = lens _gsrsProgressMessage (\ s a -> s{_gsrsProgressMessage = a})

-- | -- | The response status code.
gsrsResponseStatus :: Lens' GetStatusResponse Int
gsrsResponseStatus = lens _gsrsResponseStatus (\ s a -> s{_gsrsResponseStatus = a})

instance NFData GetStatusResponse where
