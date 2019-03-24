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
-- Module      : Network.AWS.AppStream.UpdateImagePermissions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates permissions for the specified private image.
--
--
module Network.AWS.AppStream.UpdateImagePermissions
    (
    -- * Creating a Request
      updateImagePermissions
    , UpdateImagePermissions
    -- * Request Lenses
    , uipName
    , uipSharedAccountId
    , uipImagePermissions

    -- * Destructuring the Response
    , updateImagePermissionsResponse
    , UpdateImagePermissionsResponse
    -- * Response Lenses
    , uiprsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateImagePermissions' smart constructor.
data UpdateImagePermissions = UpdateImagePermissions'
  { _uipName             :: !Text
  , _uipSharedAccountId  :: !Text
  , _uipImagePermissions :: !ImagePermissions
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateImagePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uipName' - The name of the private image.
--
-- * 'uipSharedAccountId' - The 12-digit identifier of the AWS account for which you want add or update image permissions.
--
-- * 'uipImagePermissions' - The permissions for the image.
updateImagePermissions
    :: Text -- ^ 'uipName'
    -> Text -- ^ 'uipSharedAccountId'
    -> ImagePermissions -- ^ 'uipImagePermissions'
    -> UpdateImagePermissions
updateImagePermissions pName_ pSharedAccountId_ pImagePermissions_ =
  UpdateImagePermissions'
    { _uipName = pName_
    , _uipSharedAccountId = pSharedAccountId_
    , _uipImagePermissions = pImagePermissions_
    }


-- | The name of the private image.
uipName :: Lens' UpdateImagePermissions Text
uipName = lens _uipName (\ s a -> s{_uipName = a})

-- | The 12-digit identifier of the AWS account for which you want add or update image permissions.
uipSharedAccountId :: Lens' UpdateImagePermissions Text
uipSharedAccountId = lens _uipSharedAccountId (\ s a -> s{_uipSharedAccountId = a})

-- | The permissions for the image.
uipImagePermissions :: Lens' UpdateImagePermissions ImagePermissions
uipImagePermissions = lens _uipImagePermissions (\ s a -> s{_uipImagePermissions = a})

instance AWSRequest UpdateImagePermissions where
        type Rs UpdateImagePermissions =
             UpdateImagePermissionsResponse
        request = postJSON appStream
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateImagePermissionsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateImagePermissions where

instance NFData UpdateImagePermissions where

instance ToHeaders UpdateImagePermissions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.UpdateImagePermissions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateImagePermissions where
        toJSON UpdateImagePermissions'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _uipName),
                  Just ("SharedAccountId" .= _uipSharedAccountId),
                  Just ("ImagePermissions" .= _uipImagePermissions)])

instance ToPath UpdateImagePermissions where
        toPath = const "/"

instance ToQuery UpdateImagePermissions where
        toQuery = const mempty

-- | /See:/ 'updateImagePermissionsResponse' smart constructor.
newtype UpdateImagePermissionsResponse = UpdateImagePermissionsResponse'
  { _uiprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateImagePermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiprsResponseStatus' - -- | The response status code.
updateImagePermissionsResponse
    :: Int -- ^ 'uiprsResponseStatus'
    -> UpdateImagePermissionsResponse
updateImagePermissionsResponse pResponseStatus_ =
  UpdateImagePermissionsResponse' {_uiprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uiprsResponseStatus :: Lens' UpdateImagePermissionsResponse Int
uiprsResponseStatus = lens _uiprsResponseStatus (\ s a -> s{_uiprsResponseStatus = a})

instance NFData UpdateImagePermissionsResponse where
