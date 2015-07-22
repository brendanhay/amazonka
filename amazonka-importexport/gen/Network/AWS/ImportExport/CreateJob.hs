{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.CreateJob
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates the process of scheduling an upload or download
-- of your data. You include in the request a manifest that describes the
-- data transfer specifics. The response to the request includes a job ID,
-- which you can use in other operations, a signature that you use to
-- identify your storage device, and the address where you should ship your
-- storage device.
--
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebCreateJob.html>
module Network.AWS.ImportExport.CreateJob
    (
    -- * Request
      CreateJob
    -- ** Request constructor
    , createJob
    -- ** Request lenses
    , cjrqAPIVersion
    , cjrqManifestAddendum
    , cjrqJobType
    , cjrqManifest
    , cjrqValidateOnly

    -- * Response
    , CreateJobResponse
    -- ** Response constructor
    , createJobResponse
    -- ** Response lenses
    , cjrsSignature
    , cjrsJobType
    , cjrsJobId
    , cjrsSignatureFileContents
    , cjrsWarningMessage
    , cjrsArtifactList
    , cjrsStatus
    ) where

import           Network.AWS.ImportExport.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input structure for the CreateJob operation.
--
-- /See:/ 'createJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjrqAPIVersion'
--
-- * 'cjrqManifestAddendum'
--
-- * 'cjrqJobType'
--
-- * 'cjrqManifest'
--
-- * 'cjrqValidateOnly'
data CreateJob = CreateJob'
    { _cjrqAPIVersion       :: !(Maybe Text)
    , _cjrqManifestAddendum :: !(Maybe Text)
    , _cjrqJobType          :: !JobType
    , _cjrqManifest         :: !Text
    , _cjrqValidateOnly     :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateJob' smart constructor.
createJob :: JobType -> Text -> Bool -> CreateJob
createJob pJobType pManifest pValidateOnly =
    CreateJob'
    { _cjrqAPIVersion = Nothing
    , _cjrqManifestAddendum = Nothing
    , _cjrqJobType = pJobType
    , _cjrqManifest = pManifest
    , _cjrqValidateOnly = pValidateOnly
    }

-- | FIXME: Undocumented member.
cjrqAPIVersion :: Lens' CreateJob (Maybe Text)
cjrqAPIVersion = lens _cjrqAPIVersion (\ s a -> s{_cjrqAPIVersion = a});

-- | FIXME: Undocumented member.
cjrqManifestAddendum :: Lens' CreateJob (Maybe Text)
cjrqManifestAddendum = lens _cjrqManifestAddendum (\ s a -> s{_cjrqManifestAddendum = a});

-- | FIXME: Undocumented member.
cjrqJobType :: Lens' CreateJob JobType
cjrqJobType = lens _cjrqJobType (\ s a -> s{_cjrqJobType = a});

-- | FIXME: Undocumented member.
cjrqManifest :: Lens' CreateJob Text
cjrqManifest = lens _cjrqManifest (\ s a -> s{_cjrqManifest = a});

-- | FIXME: Undocumented member.
cjrqValidateOnly :: Lens' CreateJob Bool
cjrqValidateOnly = lens _cjrqValidateOnly (\ s a -> s{_cjrqValidateOnly = a});

instance AWSRequest CreateJob where
        type Sv CreateJob = ImportExport
        type Rs CreateJob = CreateJobResponse
        request = post
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
               "APIVersion" =: _cjrqAPIVersion,
               "ManifestAddendum" =: _cjrqManifestAddendum,
               "JobType" =: _cjrqJobType,
               "Manifest" =: _cjrqManifest,
               "ValidateOnly" =: _cjrqValidateOnly]

-- | Output structure for the CreateJob operation.
--
-- /See:/ 'createJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjrsSignature'
--
-- * 'cjrsJobType'
--
-- * 'cjrsJobId'
--
-- * 'cjrsSignatureFileContents'
--
-- * 'cjrsWarningMessage'
--
-- * 'cjrsArtifactList'
--
-- * 'cjrsStatus'
data CreateJobResponse = CreateJobResponse'
    { _cjrsSignature             :: !(Maybe Text)
    , _cjrsJobType               :: !(Maybe JobType)
    , _cjrsJobId                 :: !(Maybe Text)
    , _cjrsSignatureFileContents :: !(Maybe Text)
    , _cjrsWarningMessage        :: !(Maybe Text)
    , _cjrsArtifactList          :: !(Maybe [Artifact])
    , _cjrsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateJobResponse' smart constructor.
createJobResponse :: Int -> CreateJobResponse
createJobResponse pStatus =
    CreateJobResponse'
    { _cjrsSignature = Nothing
    , _cjrsJobType = Nothing
    , _cjrsJobId = Nothing
    , _cjrsSignatureFileContents = Nothing
    , _cjrsWarningMessage = Nothing
    , _cjrsArtifactList = Nothing
    , _cjrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
cjrsSignature :: Lens' CreateJobResponse (Maybe Text)
cjrsSignature = lens _cjrsSignature (\ s a -> s{_cjrsSignature = a});

-- | FIXME: Undocumented member.
cjrsJobType :: Lens' CreateJobResponse (Maybe JobType)
cjrsJobType = lens _cjrsJobType (\ s a -> s{_cjrsJobType = a});

-- | FIXME: Undocumented member.
cjrsJobId :: Lens' CreateJobResponse (Maybe Text)
cjrsJobId = lens _cjrsJobId (\ s a -> s{_cjrsJobId = a});

-- | FIXME: Undocumented member.
cjrsSignatureFileContents :: Lens' CreateJobResponse (Maybe Text)
cjrsSignatureFileContents = lens _cjrsSignatureFileContents (\ s a -> s{_cjrsSignatureFileContents = a});

-- | FIXME: Undocumented member.
cjrsWarningMessage :: Lens' CreateJobResponse (Maybe Text)
cjrsWarningMessage = lens _cjrsWarningMessage (\ s a -> s{_cjrsWarningMessage = a});

-- | FIXME: Undocumented member.
cjrsArtifactList :: Lens' CreateJobResponse [Artifact]
cjrsArtifactList = lens _cjrsArtifactList (\ s a -> s{_cjrsArtifactList = a}) . _Default;

-- | FIXME: Undocumented member.
cjrsStatus :: Lens' CreateJobResponse Int
cjrsStatus = lens _cjrsStatus (\ s a -> s{_cjrsStatus = a});
