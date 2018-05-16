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
-- Module      : Network.AWS.Mobile.ExportProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports project configuration to a snapshot which can be downloaded and shared. Note that mobile app push credentials are encrypted in exported projects, so they can only be shared successfully within the same AWS account.
--
--
module Network.AWS.Mobile.ExportProject
    (
    -- * Creating a Request
      exportProject
    , ExportProject
    -- * Request Lenses
    , epProjectId

    -- * Destructuring the Response
    , exportProjectResponse
    , ExportProjectResponse
    -- * Response Lenses
    , eprsShareURL
    , eprsDownloadURL
    , eprsSnapshotId
    , eprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Mobile.Types
import Network.AWS.Mobile.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure used in requests to export project configuration details.
--
--
--
-- /See:/ 'exportProject' smart constructor.
newtype ExportProject = ExportProject'
  { _epProjectId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epProjectId' - Unique project identifier.
exportProject
    :: Text -- ^ 'epProjectId'
    -> ExportProject
exportProject pProjectId_ = ExportProject' {_epProjectId = pProjectId_}


-- | Unique project identifier.
epProjectId :: Lens' ExportProject Text
epProjectId = lens _epProjectId (\ s a -> s{_epProjectId = a})

instance AWSRequest ExportProject where
        type Rs ExportProject = ExportProjectResponse
        request = postJSON mobile
        response
          = receiveJSON
              (\ s h x ->
                 ExportProjectResponse' <$>
                   (x .?> "shareUrl") <*> (x .?> "downloadUrl") <*>
                     (x .?> "snapshotId")
                     <*> (pure (fromEnum s)))

instance Hashable ExportProject where

instance NFData ExportProject where

instance ToHeaders ExportProject where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ExportProject where
        toJSON = const (Object mempty)

instance ToPath ExportProject where
        toPath ExportProject'{..}
          = mconcat ["/exports/", toBS _epProjectId]

instance ToQuery ExportProject where
        toQuery = const mempty

-- | Result structure used for requests to export project configuration details.
--
--
--
-- /See:/ 'exportProjectResponse' smart constructor.
data ExportProjectResponse = ExportProjectResponse'
  { _eprsShareURL       :: !(Maybe Text)
  , _eprsDownloadURL    :: !(Maybe Text)
  , _eprsSnapshotId     :: !(Maybe Text)
  , _eprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eprsShareURL' - URL which can be shared to allow other AWS users to create their own project in AWS Mobile Hub with the same configuration as the specified project. This URL pertains to a snapshot in time of the project configuration that is created when this API is called. If you want to share additional changes to your project configuration, then you will need to create and share a new snapshot by calling this method again.
--
-- * 'eprsDownloadURL' - URL which can be used to download the exported project configuation file(s).
--
-- * 'eprsSnapshotId' - Unique identifier for the exported snapshot of the project configuration. This snapshot identifier is included in the share URL.
--
-- * 'eprsResponseStatus' - -- | The response status code.
exportProjectResponse
    :: Int -- ^ 'eprsResponseStatus'
    -> ExportProjectResponse
exportProjectResponse pResponseStatus_ =
  ExportProjectResponse'
    { _eprsShareURL = Nothing
    , _eprsDownloadURL = Nothing
    , _eprsSnapshotId = Nothing
    , _eprsResponseStatus = pResponseStatus_
    }


-- | URL which can be shared to allow other AWS users to create their own project in AWS Mobile Hub with the same configuration as the specified project. This URL pertains to a snapshot in time of the project configuration that is created when this API is called. If you want to share additional changes to your project configuration, then you will need to create and share a new snapshot by calling this method again.
eprsShareURL :: Lens' ExportProjectResponse (Maybe Text)
eprsShareURL = lens _eprsShareURL (\ s a -> s{_eprsShareURL = a})

-- | URL which can be used to download the exported project configuation file(s).
eprsDownloadURL :: Lens' ExportProjectResponse (Maybe Text)
eprsDownloadURL = lens _eprsDownloadURL (\ s a -> s{_eprsDownloadURL = a})

-- | Unique identifier for the exported snapshot of the project configuration. This snapshot identifier is included in the share URL.
eprsSnapshotId :: Lens' ExportProjectResponse (Maybe Text)
eprsSnapshotId = lens _eprsSnapshotId (\ s a -> s{_eprsSnapshotId = a})

-- | -- | The response status code.
eprsResponseStatus :: Lens' ExportProjectResponse Int
eprsResponseStatus = lens _eprsResponseStatus (\ s a -> s{_eprsResponseStatus = a})

instance NFData ExportProjectResponse where
