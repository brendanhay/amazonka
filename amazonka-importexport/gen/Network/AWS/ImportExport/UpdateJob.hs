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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You use this operation to change the parameters specified in the original manifest file by supplying a new manifest file. The manifest file attached to this request replaces the original manifest file. You can only use the operation after a CreateJob request but before the data transfer starts and you can only use it on jobs you own.
module Network.AWS.ImportExport.UpdateJob
    (
    -- * Creating a Request
      updateJob
    , UpdateJob
    -- * Request Lenses
    , ujAPIVersion
    , ujJobId
    , ujManifest
    , ujJobType
    , ujValidateOnly

    -- * Destructuring the Response
    , updateJobResponse
    , UpdateJobResponse
    -- * Response Lenses
    , ujrsSuccess
    , ujrsWarningMessage
    , ujrsArtifactList
    , ujrsResponseStatus
    ) where

import Network.AWS.ImportExport.Types
import Network.AWS.ImportExport.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input structure for the UpateJob operation.
--
-- /See:/ 'updateJob' smart constructor.
data UpdateJob = UpdateJob'
  { _ujAPIVersion   :: !(Maybe Text)
  , _ujJobId        :: !Text
  , _ujManifest     :: !Text
  , _ujJobType      :: !JobType
  , _ujValidateOnly :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujAPIVersion' - Undocumented member.
--
-- * 'ujJobId' - Undocumented member.
--
-- * 'ujManifest' - Undocumented member.
--
-- * 'ujJobType' - Undocumented member.
--
-- * 'ujValidateOnly' - Undocumented member.
updateJob
    :: Text -- ^ 'ujJobId'
    -> Text -- ^ 'ujManifest'
    -> JobType -- ^ 'ujJobType'
    -> Bool -- ^ 'ujValidateOnly'
    -> UpdateJob
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
ujAPIVersion = lens _ujAPIVersion (\ s a -> s{_ujAPIVersion = a})

-- | Undocumented member.
ujJobId :: Lens' UpdateJob Text
ujJobId = lens _ujJobId (\ s a -> s{_ujJobId = a})

-- | Undocumented member.
ujManifest :: Lens' UpdateJob Text
ujManifest = lens _ujManifest (\ s a -> s{_ujManifest = a})

-- | Undocumented member.
ujJobType :: Lens' UpdateJob JobType
ujJobType = lens _ujJobType (\ s a -> s{_ujJobType = a})

-- | Undocumented member.
ujValidateOnly :: Lens' UpdateJob Bool
ujValidateOnly = lens _ujValidateOnly (\ s a -> s{_ujValidateOnly = a})

instance AWSRequest UpdateJob where
        type Rs UpdateJob = UpdateJobResponse
        request = postQuery importExport
        response
          = receiveXMLWrapper "UpdateJobResult"
              (\ s h x ->
                 UpdateJobResponse' <$>
                   (x .@? "Success") <*> (x .@? "WarningMessage") <*>
                     (x .@? "ArtifactList" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable UpdateJob where

instance NFData UpdateJob where

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
data UpdateJobResponse = UpdateJobResponse'
  { _ujrsSuccess        :: !(Maybe Bool)
  , _ujrsWarningMessage :: !(Maybe Text)
  , _ujrsArtifactList   :: !(Maybe [Artifact])
  , _ujrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujrsSuccess' - Undocumented member.
--
-- * 'ujrsWarningMessage' - Undocumented member.
--
-- * 'ujrsArtifactList' - Undocumented member.
--
-- * 'ujrsResponseStatus' - -- | The response status code.
updateJobResponse
    :: Int -- ^ 'ujrsResponseStatus'
    -> UpdateJobResponse
updateJobResponse pResponseStatus_ =
  UpdateJobResponse'
    { _ujrsSuccess = Nothing
    , _ujrsWarningMessage = Nothing
    , _ujrsArtifactList = Nothing
    , _ujrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ujrsSuccess :: Lens' UpdateJobResponse (Maybe Bool)
ujrsSuccess = lens _ujrsSuccess (\ s a -> s{_ujrsSuccess = a})

-- | Undocumented member.
ujrsWarningMessage :: Lens' UpdateJobResponse (Maybe Text)
ujrsWarningMessage = lens _ujrsWarningMessage (\ s a -> s{_ujrsWarningMessage = a})

-- | Undocumented member.
ujrsArtifactList :: Lens' UpdateJobResponse [Artifact]
ujrsArtifactList = lens _ujrsArtifactList (\ s a -> s{_ujrsArtifactList = a}) . _Default . _Coerce

-- | -- | The response status code.
ujrsResponseStatus :: Lens' UpdateJobResponse Int
ujrsResponseStatus = lens _ujrsResponseStatus (\ s a -> s{_ujrsResponseStatus = a})

instance NFData UpdateJobResponse where
