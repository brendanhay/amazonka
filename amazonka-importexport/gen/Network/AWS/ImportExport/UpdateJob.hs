{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ImportExport.UpdateJob
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

-- | You use this operation to change the parameters specified in the
-- original manifest file by supplying a new manifest file. The manifest
-- file attached to this request replaces the original manifest file. You
-- can only use the operation after a CreateJob request but before the data
-- transfer starts and you can only use it on jobs you own.
--
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebUpdateJob.html>
module Network.AWS.ImportExport.UpdateJob
    (
    -- * Request
      UpdateJob
    -- ** Request constructor
    , updateJob
    -- ** Request lenses
    , ujAPIVersion
    , ujJobId
    , ujManifest
    , ujJobType
    , ujValidateOnly

    -- * Response
    , UpdateJobResponse
    -- ** Response constructor
    , updateJobResponse
    -- ** Response lenses
    , ujrSuccess
    , ujrWarningMessage
    , ujrArtifactList
    , ujrStatus
    ) where

import           Network.AWS.ImportExport.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input structure for the UpateJob operation.
--
-- /See:/ 'updateJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ujAPIVersion'
--
-- * 'ujJobId'
--
-- * 'ujManifest'
--
-- * 'ujJobType'
--
-- * 'ujValidateOnly'
data UpdateJob = UpdateJob'
    { _ujAPIVersion   :: Maybe Text
    , _ujJobId        :: Text
    , _ujManifest     :: Text
    , _ujJobType      :: JobType
    , _ujValidateOnly :: !Bool
    } deriving (Eq,Read,Show)

-- | 'UpdateJob' smart constructor.
updateJob :: Text -> Text -> JobType -> Bool -> UpdateJob
updateJob pJobId pManifest pJobType pValidateOnly =
    UpdateJob'
    { _ujAPIVersion = Nothing
    , _ujJobId = pJobId
    , _ujManifest = pManifest
    , _ujJobType = pJobType
    , _ujValidateOnly = pValidateOnly
    }

-- | FIXME: Undocumented member.
ujAPIVersion :: Lens' UpdateJob (Maybe Text)
ujAPIVersion = lens _ujAPIVersion (\ s a -> s{_ujAPIVersion = a});

-- | FIXME: Undocumented member.
ujJobId :: Lens' UpdateJob Text
ujJobId = lens _ujJobId (\ s a -> s{_ujJobId = a});

-- | FIXME: Undocumented member.
ujManifest :: Lens' UpdateJob Text
ujManifest = lens _ujManifest (\ s a -> s{_ujManifest = a});

-- | FIXME: Undocumented member.
ujJobType :: Lens' UpdateJob JobType
ujJobType = lens _ujJobType (\ s a -> s{_ujJobType = a});

-- | FIXME: Undocumented member.
ujValidateOnly :: Lens' UpdateJob Bool
ujValidateOnly = lens _ujValidateOnly (\ s a -> s{_ujValidateOnly = a});

instance AWSRequest UpdateJob where
        type Sv UpdateJob = ImportExport
        type Rs UpdateJob = UpdateJobResponse
        request = post
        response
          = receiveXMLWrapper "UpdateJobResult"
              (\ s h x ->
                 UpdateJobResponse' <$>
                   (x .@? "Success") <*> (x .@? "WarningMessage") <*>
                     (x .@? "ArtifactList" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders UpdateJob where
        toHeaders = const mempty

instance ToPath UpdateJob where
        toPath = const "/"

instance ToQuery UpdateJob where
        toQuery UpdateJob'{..}
          = mconcat
              ["Operation=UpdateJob",
               "Action" =: ("UpdateJob" :: ByteString),
               "Version" =: ("2010-06-01" :: ByteString),
               "APIVersion" =: _ujAPIVersion, "JobId" =: _ujJobId,
               "Manifest" =: _ujManifest, "JobType" =: _ujJobType,
               "ValidateOnly" =: _ujValidateOnly]

-- | Output structure for the UpateJob operation.
--
-- /See:/ 'updateJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ujrSuccess'
--
-- * 'ujrWarningMessage'
--
-- * 'ujrArtifactList'
--
-- * 'ujrStatus'
data UpdateJobResponse = UpdateJobResponse'
    { _ujrSuccess        :: Maybe Bool
    , _ujrWarningMessage :: Maybe Text
    , _ujrArtifactList   :: Maybe [Artifact]
    , _ujrStatus         :: !Int
    } deriving (Eq,Read,Show)

-- | 'UpdateJobResponse' smart constructor.
updateJobResponse :: Int -> UpdateJobResponse
updateJobResponse pStatus =
    UpdateJobResponse'
    { _ujrSuccess = Nothing
    , _ujrWarningMessage = Nothing
    , _ujrArtifactList = Nothing
    , _ujrStatus = pStatus
    }

-- | FIXME: Undocumented member.
ujrSuccess :: Lens' UpdateJobResponse (Maybe Bool)
ujrSuccess = lens _ujrSuccess (\ s a -> s{_ujrSuccess = a});

-- | FIXME: Undocumented member.
ujrWarningMessage :: Lens' UpdateJobResponse (Maybe Text)
ujrWarningMessage = lens _ujrWarningMessage (\ s a -> s{_ujrWarningMessage = a});

-- | FIXME: Undocumented member.
ujrArtifactList :: Lens' UpdateJobResponse [Artifact]
ujrArtifactList = lens _ujrArtifactList (\ s a -> s{_ujrArtifactList = a}) . _Default;

-- | FIXME: Undocumented member.
ujrStatus :: Lens' UpdateJobResponse Int
ujrStatus = lens _ujrStatus (\ s a -> s{_ujrStatus = a});
