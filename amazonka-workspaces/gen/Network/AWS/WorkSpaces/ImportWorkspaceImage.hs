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
-- Module      : Network.AWS.WorkSpaces.ImportWorkspaceImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the specified Windows 7 or Windows 10 bring your own license (BYOL) image into Amazon WorkSpaces. The image must be an already licensed EC2 image that is in your AWS account, and you must own the image.
--
--
module Network.AWS.WorkSpaces.ImportWorkspaceImage
    (
    -- * Creating a Request
      importWorkspaceImage
    , ImportWorkspaceImage
    -- * Request Lenses
    , iwiEC2ImageId
    , iwiIngestionProcess
    , iwiImageName
    , iwiImageDescription

    -- * Destructuring the Response
    , importWorkspaceImageResponse
    , ImportWorkspaceImageResponse
    -- * Response Lenses
    , iwirsImageId
    , iwirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'importWorkspaceImage' smart constructor.
data ImportWorkspaceImage = ImportWorkspaceImage'
  { _iwiEC2ImageId       :: !Text
  , _iwiIngestionProcess :: !WorkspaceImageIngestionProcess
  , _iwiImageName        :: !Text
  , _iwiImageDescription :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportWorkspaceImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iwiEC2ImageId' - The identifier of the EC2 image.
--
-- * 'iwiIngestionProcess' - The ingestion process to be used when importing the image.
--
-- * 'iwiImageName' - The name of the WorkSpace image.
--
-- * 'iwiImageDescription' - The description of the WorkSpace image.
importWorkspaceImage
    :: Text -- ^ 'iwiEC2ImageId'
    -> WorkspaceImageIngestionProcess -- ^ 'iwiIngestionProcess'
    -> Text -- ^ 'iwiImageName'
    -> Text -- ^ 'iwiImageDescription'
    -> ImportWorkspaceImage
importWorkspaceImage pEC2ImageId_ pIngestionProcess_ pImageName_ pImageDescription_ =
  ImportWorkspaceImage'
    { _iwiEC2ImageId = pEC2ImageId_
    , _iwiIngestionProcess = pIngestionProcess_
    , _iwiImageName = pImageName_
    , _iwiImageDescription = pImageDescription_
    }


-- | The identifier of the EC2 image.
iwiEC2ImageId :: Lens' ImportWorkspaceImage Text
iwiEC2ImageId = lens _iwiEC2ImageId (\ s a -> s{_iwiEC2ImageId = a})

-- | The ingestion process to be used when importing the image.
iwiIngestionProcess :: Lens' ImportWorkspaceImage WorkspaceImageIngestionProcess
iwiIngestionProcess = lens _iwiIngestionProcess (\ s a -> s{_iwiIngestionProcess = a})

-- | The name of the WorkSpace image.
iwiImageName :: Lens' ImportWorkspaceImage Text
iwiImageName = lens _iwiImageName (\ s a -> s{_iwiImageName = a})

-- | The description of the WorkSpace image.
iwiImageDescription :: Lens' ImportWorkspaceImage Text
iwiImageDescription = lens _iwiImageDescription (\ s a -> s{_iwiImageDescription = a})

instance AWSRequest ImportWorkspaceImage where
        type Rs ImportWorkspaceImage =
             ImportWorkspaceImageResponse
        request = postJSON workSpaces
        response
          = receiveJSON
              (\ s h x ->
                 ImportWorkspaceImageResponse' <$>
                   (x .?> "ImageId") <*> (pure (fromEnum s)))

instance Hashable ImportWorkspaceImage where

instance NFData ImportWorkspaceImage where

instance ToHeaders ImportWorkspaceImage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.ImportWorkspaceImage" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ImportWorkspaceImage where
        toJSON ImportWorkspaceImage'{..}
          = object
              (catMaybes
                 [Just ("Ec2ImageId" .= _iwiEC2ImageId),
                  Just ("IngestionProcess" .= _iwiIngestionProcess),
                  Just ("ImageName" .= _iwiImageName),
                  Just ("ImageDescription" .= _iwiImageDescription)])

instance ToPath ImportWorkspaceImage where
        toPath = const "/"

instance ToQuery ImportWorkspaceImage where
        toQuery = const mempty

-- | /See:/ 'importWorkspaceImageResponse' smart constructor.
data ImportWorkspaceImageResponse = ImportWorkspaceImageResponse'
  { _iwirsImageId        :: !(Maybe Text)
  , _iwirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportWorkspaceImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iwirsImageId' - The identifier of the WorkSpace image.
--
-- * 'iwirsResponseStatus' - -- | The response status code.
importWorkspaceImageResponse
    :: Int -- ^ 'iwirsResponseStatus'
    -> ImportWorkspaceImageResponse
importWorkspaceImageResponse pResponseStatus_ =
  ImportWorkspaceImageResponse'
    {_iwirsImageId = Nothing, _iwirsResponseStatus = pResponseStatus_}


-- | The identifier of the WorkSpace image.
iwirsImageId :: Lens' ImportWorkspaceImageResponse (Maybe Text)
iwirsImageId = lens _iwirsImageId (\ s a -> s{_iwirsImageId = a})

-- | -- | The response status code.
iwirsResponseStatus :: Lens' ImportWorkspaceImageResponse Int
iwirsResponseStatus = lens _iwirsResponseStatus (\ s a -> s{_iwirsResponseStatus = a})

instance NFData ImportWorkspaceImageResponse where
