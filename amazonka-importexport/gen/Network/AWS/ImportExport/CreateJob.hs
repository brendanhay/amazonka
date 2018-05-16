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
-- Module      : Network.AWS.ImportExport.CreateJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates the process of scheduling an upload or download of your data. You include in the request a manifest that describes the data transfer specifics. The response to the request includes a job ID, which you can use in other operations, a signature that you use to identify your storage device, and the address where you should ship your storage device.
module Network.AWS.ImportExport.CreateJob
    (
    -- * Creating a Request
      createJob
    , CreateJob
    -- * Request Lenses
    , cjAPIVersion
    , cjManifestAddendum
    , cjJobType
    , cjManifest
    , cjValidateOnly

    -- * Destructuring the Response
    , createJobResponse
    , CreateJobResponse
    -- * Response Lenses
    , cjrsSignature
    , cjrsJobType
    , cjrsJobId
    , cjrsSignatureFileContents
    , cjrsWarningMessage
    , cjrsArtifactList
    , cjrsResponseStatus
    ) where

import Network.AWS.ImportExport.Types
import Network.AWS.ImportExport.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input structure for the CreateJob operation.
--
-- /See:/ 'createJob' smart constructor.
data CreateJob = CreateJob'
  { _cjAPIVersion       :: !(Maybe Text)
  , _cjManifestAddendum :: !(Maybe Text)
  , _cjJobType          :: !JobType
  , _cjManifest         :: !Text
  , _cjValidateOnly     :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjAPIVersion' - Undocumented member.
--
-- * 'cjManifestAddendum' - Undocumented member.
--
-- * 'cjJobType' - Undocumented member.
--
-- * 'cjManifest' - Undocumented member.
--
-- * 'cjValidateOnly' - Undocumented member.
createJob
    :: JobType -- ^ 'cjJobType'
    -> Text -- ^ 'cjManifest'
    -> Bool -- ^ 'cjValidateOnly'
    -> CreateJob
createJob pJobType_ pManifest_ pValidateOnly_ =
  CreateJob'
    { _cjAPIVersion = Nothing
    , _cjManifestAddendum = Nothing
    , _cjJobType = pJobType_
    , _cjManifest = pManifest_
    , _cjValidateOnly = pValidateOnly_
    }


-- | Undocumented member.
cjAPIVersion :: Lens' CreateJob (Maybe Text)
cjAPIVersion = lens _cjAPIVersion (\ s a -> s{_cjAPIVersion = a})

-- | Undocumented member.
cjManifestAddendum :: Lens' CreateJob (Maybe Text)
cjManifestAddendum = lens _cjManifestAddendum (\ s a -> s{_cjManifestAddendum = a})

-- | Undocumented member.
cjJobType :: Lens' CreateJob JobType
cjJobType = lens _cjJobType (\ s a -> s{_cjJobType = a})

-- | Undocumented member.
cjManifest :: Lens' CreateJob Text
cjManifest = lens _cjManifest (\ s a -> s{_cjManifest = a})

-- | Undocumented member.
cjValidateOnly :: Lens' CreateJob Bool
cjValidateOnly = lens _cjValidateOnly (\ s a -> s{_cjValidateOnly = a})

instance AWSRequest CreateJob where
        type Rs CreateJob = CreateJobResponse
        request = postQuery importExport
        response
          = receiveXMLWrapper "CreateJobResult"
              (\ s h x ->
                 CreateJobResponse' <$>
                   (x .@? "Signature") <*> (x .@? "JobType") <*>
                     (x .@? "JobId")
                     <*> (x .@? "SignatureFileContents")
                     <*> (x .@? "WarningMessage")
                     <*>
                     (x .@? "ArtifactList" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable CreateJob where

instance NFData CreateJob where

instance ToHeaders CreateJob where
        toHeaders = const mempty

instance ToPath CreateJob where
        toPath = const "/"

instance ToQuery CreateJob where
        toQuery CreateJob'{..}
          = mconcat
              ["Operation=CreateJob",
               "Action" =: ("CreateJob" :: ByteString),
               "Version" =: ("2010-06-01" :: ByteString),
               "APIVersion" =: _cjAPIVersion,
               "ManifestAddendum" =: _cjManifestAddendum,
               "JobType" =: _cjJobType, "Manifest" =: _cjManifest,
               "ValidateOnly" =: _cjValidateOnly]

-- | Output structure for the CreateJob operation.
--
-- /See:/ 'createJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { _cjrsSignature             :: !(Maybe Text)
  , _cjrsJobType               :: !(Maybe JobType)
  , _cjrsJobId                 :: !(Maybe Text)
  , _cjrsSignatureFileContents :: !(Maybe Text)
  , _cjrsWarningMessage        :: !(Maybe Text)
  , _cjrsArtifactList          :: !(Maybe [Artifact])
  , _cjrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjrsSignature' - Undocumented member.
--
-- * 'cjrsJobType' - Undocumented member.
--
-- * 'cjrsJobId' - Undocumented member.
--
-- * 'cjrsSignatureFileContents' - Undocumented member.
--
-- * 'cjrsWarningMessage' - Undocumented member.
--
-- * 'cjrsArtifactList' - Undocumented member.
--
-- * 'cjrsResponseStatus' - -- | The response status code.
createJobResponse
    :: Int -- ^ 'cjrsResponseStatus'
    -> CreateJobResponse
createJobResponse pResponseStatus_ =
  CreateJobResponse'
    { _cjrsSignature = Nothing
    , _cjrsJobType = Nothing
    , _cjrsJobId = Nothing
    , _cjrsSignatureFileContents = Nothing
    , _cjrsWarningMessage = Nothing
    , _cjrsArtifactList = Nothing
    , _cjrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
cjrsSignature :: Lens' CreateJobResponse (Maybe Text)
cjrsSignature = lens _cjrsSignature (\ s a -> s{_cjrsSignature = a})

-- | Undocumented member.
cjrsJobType :: Lens' CreateJobResponse (Maybe JobType)
cjrsJobType = lens _cjrsJobType (\ s a -> s{_cjrsJobType = a})

-- | Undocumented member.
cjrsJobId :: Lens' CreateJobResponse (Maybe Text)
cjrsJobId = lens _cjrsJobId (\ s a -> s{_cjrsJobId = a})

-- | Undocumented member.
cjrsSignatureFileContents :: Lens' CreateJobResponse (Maybe Text)
cjrsSignatureFileContents = lens _cjrsSignatureFileContents (\ s a -> s{_cjrsSignatureFileContents = a})

-- | Undocumented member.
cjrsWarningMessage :: Lens' CreateJobResponse (Maybe Text)
cjrsWarningMessage = lens _cjrsWarningMessage (\ s a -> s{_cjrsWarningMessage = a})

-- | Undocumented member.
cjrsArtifactList :: Lens' CreateJobResponse [Artifact]
cjrsArtifactList = lens _cjrsArtifactList (\ s a -> s{_cjrsArtifactList = a}) . _Default . _Coerce

-- | -- | The response status code.
cjrsResponseStatus :: Lens' CreateJobResponse Int
cjrsResponseStatus = lens _cjrsResponseStatus (\ s a -> s{_cjrsResponseStatus = a})

instance NFData CreateJobResponse where
