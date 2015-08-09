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
-- Module      : Network.AWS.ImportExport.UpdateJob
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You use this operation to change the parameters specified in the
-- original manifest file by supplying a new manifest file. The manifest
-- file attached to this request replaces the original manifest file. You
-- can only use the operation after a CreateJob request but before the data
-- transfer starts and you can only use it on jobs you own.
--
-- /See:/ <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebUpdateJob.html AWS API Reference> for UpdateJob.
module Network.AWS.ImportExport.UpdateJob
    (
    -- * Creating a Request
      UpdateJob
    , updateJob
    -- * Request Lenses
    , ujAPIVersion
    , ujJobId
    , ujManifest
    , ujJobType
    , ujValidateOnly

    -- * Destructuring the Response
    , UpdateJobResponse
    , updateJobResponse
    -- * Response Lenses
    , ujrsSuccess
    , ujrsWarningMessage
    , ujrsArtifactList
    , ujrsStatus
    ) where

import           Network.AWS.ImportExport.Types
import           Network.AWS.ImportExport.Types.Product
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
    { _ujAPIVersion   :: !(Maybe Text)
    , _ujJobId        :: !Text
    , _ujManifest     :: !Text
    , _ujJobType      :: !JobType
    , _ujValidateOnly :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateJob' smart constructor.
updateJob :: Text -> Text -> JobType -> Bool -> UpdateJob
updateJob pJobId_ pManifest_ pJobType_ pValidateOnly_ =
    UpdateJob'
    { _ujAPIVersion = Nothing
    , _ujJobId = pJobId_
    , _ujManifest = pManifest_
    , _ujJobType = pJobType_
    , _ujValidateOnly = pValidateOnly_
    }

-- | Undocumented member.
ujAPIVersion :: Lens' UpdateJob (Maybe Text)
ujAPIVersion = lens _ujAPIVersion (\ s a -> s{_ujAPIVersion = a});

-- | Undocumented member.
ujJobId :: Lens' UpdateJob Text
ujJobId = lens _ujJobId (\ s a -> s{_ujJobId = a});

-- | Undocumented member.
ujManifest :: Lens' UpdateJob Text
ujManifest = lens _ujManifest (\ s a -> s{_ujManifest = a});

-- | Undocumented member.
ujJobType :: Lens' UpdateJob JobType
ujJobType = lens _ujJobType (\ s a -> s{_ujJobType = a});

-- | Undocumented member.
ujValidateOnly :: Lens' UpdateJob Bool
ujValidateOnly = lens _ujValidateOnly (\ s a -> s{_ujValidateOnly = a});

instance AWSRequest UpdateJob where
        type Sv UpdateJob = ImportExport
        type Rs UpdateJob = UpdateJobResponse
        request = postQuery
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
updateJobResponse pStatus_ =
    UpdateJobResponse'
    { _ujrsSuccess = Nothing
    , _ujrsWarningMessage = Nothing
    , _ujrsArtifactList = Nothing
    , _ujrsStatus = pStatus_
    }

-- | Undocumented member.
ujrsSuccess :: Lens' UpdateJobResponse (Maybe Bool)
ujrsSuccess = lens _ujrsSuccess (\ s a -> s{_ujrsSuccess = a});

-- | Undocumented member.
ujrsWarningMessage :: Lens' UpdateJobResponse (Maybe Text)
ujrsWarningMessage = lens _ujrsWarningMessage (\ s a -> s{_ujrsWarningMessage = a});

-- | Undocumented member.
ujrsArtifactList :: Lens' UpdateJobResponse [Artifact]
ujrsArtifactList = lens _ujrsArtifactList (\ s a -> s{_ujrsArtifactList = a}) . _Default . _Coerce;

-- | Undocumented member.
ujrsStatus :: Lens' UpdateJobResponse Int
ujrsStatus = lens _ujrsStatus (\ s a -> s{_ujrsStatus = a});
