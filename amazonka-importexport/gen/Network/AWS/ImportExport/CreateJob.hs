{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ImportExport.CreateJob
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

-- | This operation initiates the process of scheduling an upload or download
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
    , cjAPIVersion
    , cjManifestAddendum
    , cjJobType
    , cjManifest
    , cjValidateOnly

    -- * Response
    , CreateJobResponse
    -- ** Response constructor
    , createJobResponse
    -- ** Response lenses
    , cjrSignature
    , cjrJobType
    , cjrJobId
    , cjrSignatureFileContents
    , cjrWarningMessage
    , cjrArtifactList
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ImportExport.Types

-- | /See:/ 'createJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjAPIVersion'
--
-- * 'cjManifestAddendum'
--
-- * 'cjJobType'
--
-- * 'cjManifest'
--
-- * 'cjValidateOnly'
data CreateJob = CreateJob'{_cjAPIVersion :: Maybe Text, _cjManifestAddendum :: Maybe Text, _cjJobType :: JobType, _cjManifest :: Text, _cjValidateOnly :: Bool} deriving (Eq, Read, Show)

-- | 'CreateJob' smart constructor.
createJob :: JobType -> Text -> Bool -> CreateJob
createJob pJobType pManifest pValidateOnly = CreateJob'{_cjAPIVersion = Nothing, _cjManifestAddendum = Nothing, _cjJobType = pJobType, _cjManifest = pManifest, _cjValidateOnly = pValidateOnly};

-- | FIXME: Undocumented member.
cjAPIVersion :: Lens' CreateJob (Maybe Text)
cjAPIVersion = lens _cjAPIVersion (\ s a -> s{_cjAPIVersion = a});

-- | FIXME: Undocumented member.
cjManifestAddendum :: Lens' CreateJob (Maybe Text)
cjManifestAddendum = lens _cjManifestAddendum (\ s a -> s{_cjManifestAddendum = a});

-- | FIXME: Undocumented member.
cjJobType :: Lens' CreateJob JobType
cjJobType = lens _cjJobType (\ s a -> s{_cjJobType = a});

-- | FIXME: Undocumented member.
cjManifest :: Lens' CreateJob Text
cjManifest = lens _cjManifest (\ s a -> s{_cjManifest = a});

-- | FIXME: Undocumented member.
cjValidateOnly :: Lens' CreateJob Bool
cjValidateOnly = lens _cjValidateOnly (\ s a -> s{_cjValidateOnly = a});

instance AWSRequest CreateJob where
        type Sv CreateJob = ImportExport
        type Rs CreateJob = CreateJobResponse
        request = post
        response
          = receiveXMLWrapper "CreateJobResult"
              (\ s h x ->
                 CreateJobResponse' <$>
                   x .@? "Signature" <*> x .@? "JobType" <*>
                     x .@? "JobId"
                     <*> x .@? "SignatureFileContents"
                     <*> x .@? "WarningMessage"
                     <*>
                     (x .@? "ArtifactList" .!@ mempty >>=
                        parseXMLList "member"))

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

-- | /See:/ 'createJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjrSignature'
--
-- * 'cjrJobType'
--
-- * 'cjrJobId'
--
-- * 'cjrSignatureFileContents'
--
-- * 'cjrWarningMessage'
--
-- * 'cjrArtifactList'
data CreateJobResponse = CreateJobResponse'{_cjrSignature :: Maybe Text, _cjrJobType :: Maybe JobType, _cjrJobId :: Maybe Text, _cjrSignatureFileContents :: Maybe Text, _cjrWarningMessage :: Maybe Text, _cjrArtifactList :: [Artifact]} deriving (Eq, Read, Show)

-- | 'CreateJobResponse' smart constructor.
createJobResponse :: CreateJobResponse
createJobResponse = CreateJobResponse'{_cjrSignature = Nothing, _cjrJobType = Nothing, _cjrJobId = Nothing, _cjrSignatureFileContents = Nothing, _cjrWarningMessage = Nothing, _cjrArtifactList = mempty};

-- | FIXME: Undocumented member.
cjrSignature :: Lens' CreateJobResponse (Maybe Text)
cjrSignature = lens _cjrSignature (\ s a -> s{_cjrSignature = a});

-- | FIXME: Undocumented member.
cjrJobType :: Lens' CreateJobResponse (Maybe JobType)
cjrJobType = lens _cjrJobType (\ s a -> s{_cjrJobType = a});

-- | FIXME: Undocumented member.
cjrJobId :: Lens' CreateJobResponse (Maybe Text)
cjrJobId = lens _cjrJobId (\ s a -> s{_cjrJobId = a});

-- | FIXME: Undocumented member.
cjrSignatureFileContents :: Lens' CreateJobResponse (Maybe Text)
cjrSignatureFileContents = lens _cjrSignatureFileContents (\ s a -> s{_cjrSignatureFileContents = a});

-- | FIXME: Undocumented member.
cjrWarningMessage :: Lens' CreateJobResponse (Maybe Text)
cjrWarningMessage = lens _cjrWarningMessage (\ s a -> s{_cjrWarningMessage = a});

-- | FIXME: Undocumented member.
cjrArtifactList :: Lens' CreateJobResponse [Artifact]
cjrArtifactList = lens _cjrArtifactList (\ s a -> s{_cjrArtifactList = a});
