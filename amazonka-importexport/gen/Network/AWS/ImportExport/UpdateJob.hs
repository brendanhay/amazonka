{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.UpdateJob
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- You use this operation to change the parameters specified in the
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
    , ujrqAPIVersion
    , ujrqJobId
    , ujrqManifest
    , ujrqJobType
    , ujrqValidateOnly

    -- * Response
    , UpdateJobResponse
    -- ** Response constructor
    , updateJobResponse
    -- ** Response lenses
    , ujrsSuccess
    , ujrsWarningMessage
    , ujrsArtifactList
    , ujrsStatus
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
-- * 'ujrqAPIVersion'
--
-- * 'ujrqJobId'
--
-- * 'ujrqManifest'
--
-- * 'ujrqJobType'
--
-- * 'ujrqValidateOnly'
data UpdateJob = UpdateJob'
    { _ujrqAPIVersion   :: !(Maybe Text)
    , _ujrqJobId        :: !Text
    , _ujrqManifest     :: !Text
    , _ujrqJobType      :: !JobType
    , _ujrqValidateOnly :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateJob' smart constructor.
updateJob :: Text -> Text -> JobType -> Bool -> UpdateJob
updateJob pJobId pManifest pJobType pValidateOnly =
    UpdateJob'
    { _ujrqAPIVersion = Nothing
    , _ujrqJobId = pJobId
    , _ujrqManifest = pManifest
    , _ujrqJobType = pJobType
    , _ujrqValidateOnly = pValidateOnly
    }

-- | FIXME: Undocumented member.
ujrqAPIVersion :: Lens' UpdateJob (Maybe Text)
ujrqAPIVersion = lens _ujrqAPIVersion (\ s a -> s{_ujrqAPIVersion = a});

-- | FIXME: Undocumented member.
ujrqJobId :: Lens' UpdateJob Text
ujrqJobId = lens _ujrqJobId (\ s a -> s{_ujrqJobId = a});

-- | FIXME: Undocumented member.
ujrqManifest :: Lens' UpdateJob Text
ujrqManifest = lens _ujrqManifest (\ s a -> s{_ujrqManifest = a});

-- | FIXME: Undocumented member.
ujrqJobType :: Lens' UpdateJob JobType
ujrqJobType = lens _ujrqJobType (\ s a -> s{_ujrqJobType = a});

-- | FIXME: Undocumented member.
ujrqValidateOnly :: Lens' UpdateJob Bool
ujrqValidateOnly = lens _ujrqValidateOnly (\ s a -> s{_ujrqValidateOnly = a});

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
               "APIVersion" =: _ujrqAPIVersion,
               "JobId" =: _ujrqJobId, "Manifest" =: _ujrqManifest,
               "JobType" =: _ujrqJobType,
               "ValidateOnly" =: _ujrqValidateOnly]

-- | Output structure for the UpateJob operation.
--
-- /See:/ 'updateJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ujrsSuccess'
--
-- * 'ujrsWarningMessage'
--
-- * 'ujrsArtifactList'
--
-- * 'ujrsStatus'
data UpdateJobResponse = UpdateJobResponse'
    { _ujrsSuccess        :: !(Maybe Bool)
    , _ujrsWarningMessage :: !(Maybe Text)
    , _ujrsArtifactList   :: !(Maybe [Artifact])
    , _ujrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateJobResponse' smart constructor.
updateJobResponse :: Int -> UpdateJobResponse
updateJobResponse pStatus =
    UpdateJobResponse'
    { _ujrsSuccess = Nothing
    , _ujrsWarningMessage = Nothing
    , _ujrsArtifactList = Nothing
    , _ujrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
ujrsSuccess :: Lens' UpdateJobResponse (Maybe Bool)
ujrsSuccess = lens _ujrsSuccess (\ s a -> s{_ujrsSuccess = a});

-- | FIXME: Undocumented member.
ujrsWarningMessage :: Lens' UpdateJobResponse (Maybe Text)
ujrsWarningMessage = lens _ujrsWarningMessage (\ s a -> s{_ujrsWarningMessage = a});

-- | FIXME: Undocumented member.
ujrsArtifactList :: Lens' UpdateJobResponse [Artifact]
ujrsArtifactList = lens _ujrsArtifactList (\ s a -> s{_ujrsArtifactList = a}) . _Default;

-- | FIXME: Undocumented member.
ujrsStatus :: Lens' UpdateJobResponse Int
ujrsStatus = lens _ujrsStatus (\ s a -> s{_ujrsStatus = a});
