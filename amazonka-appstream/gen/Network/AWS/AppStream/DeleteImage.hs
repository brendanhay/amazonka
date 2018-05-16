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
-- Module      : Network.AWS.AppStream.DeleteImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image. You cannot delete an image that is currently in use. After you delete an image, you cannot provision new capacity using the image.
--
--
module Network.AWS.AppStream.DeleteImage
    (
    -- * Creating a Request
      deleteImage
    , DeleteImage
    -- * Request Lenses
    , diName

    -- * Destructuring the Response
    , deleteImageResponse
    , DeleteImageResponse
    -- * Response Lenses
    , dirsImage
    , dirsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteImage' smart constructor.
newtype DeleteImage = DeleteImage'
  { _diName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diName' - The name of the image.
deleteImage
    :: Text -- ^ 'diName'
    -> DeleteImage
deleteImage pName_ = DeleteImage' {_diName = pName_}


-- | The name of the image.
diName :: Lens' DeleteImage Text
diName = lens _diName (\ s a -> s{_diName = a})

instance AWSRequest DeleteImage where
        type Rs DeleteImage = DeleteImageResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 DeleteImageResponse' <$>
                   (x .?> "Image") <*> (pure (fromEnum s)))

instance Hashable DeleteImage where

instance NFData DeleteImage where

instance ToHeaders DeleteImage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DeleteImage" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteImage where
        toJSON DeleteImage'{..}
          = object (catMaybes [Just ("Name" .= _diName)])

instance ToPath DeleteImage where
        toPath = const "/"

instance ToQuery DeleteImage where
        toQuery = const mempty

-- | /See:/ 'deleteImageResponse' smart constructor.
data DeleteImageResponse = DeleteImageResponse'
  { _dirsImage          :: !(Maybe Image)
  , _dirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsImage' - Information about the image.
--
-- * 'dirsResponseStatus' - -- | The response status code.
deleteImageResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DeleteImageResponse
deleteImageResponse pResponseStatus_ =
  DeleteImageResponse'
    {_dirsImage = Nothing, _dirsResponseStatus = pResponseStatus_}


-- | Information about the image.
dirsImage :: Lens' DeleteImageResponse (Maybe Image)
dirsImage = lens _dirsImage (\ s a -> s{_dirsImage = a})

-- | -- | The response status code.
dirsResponseStatus :: Lens' DeleteImageResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a})

instance NFData DeleteImageResponse where
