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
-- Module      : Network.AWS.DeviceFarm.DeleteUpload
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an upload given the upload ARN.
--
--
module Network.AWS.DeviceFarm.DeleteUpload
    (
    -- * Creating a Request
      deleteUpload
    , DeleteUpload
    -- * Request Lenses
    , duArn

    -- * Destructuring the Response
    , deleteUploadResponse
    , DeleteUploadResponse
    -- * Response Lenses
    , dursResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the delete upload operation.
--
--
--
-- /See:/ 'deleteUpload' smart constructor.
newtype DeleteUpload = DeleteUpload'
  { _duArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duArn' - Represents the Amazon Resource Name (ARN) of the Device Farm upload you wish to delete.
deleteUpload
    :: Text -- ^ 'duArn'
    -> DeleteUpload
deleteUpload pArn_ = DeleteUpload' {_duArn = pArn_}


-- | Represents the Amazon Resource Name (ARN) of the Device Farm upload you wish to delete.
duArn :: Lens' DeleteUpload Text
duArn = lens _duArn (\ s a -> s{_duArn = a})

instance AWSRequest DeleteUpload where
        type Rs DeleteUpload = DeleteUploadResponse
        request = postJSON deviceFarm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteUploadResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteUpload where

instance NFData DeleteUpload where

instance ToHeaders DeleteUpload where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.DeleteUpload" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteUpload where
        toJSON DeleteUpload'{..}
          = object (catMaybes [Just ("arn" .= _duArn)])

instance ToPath DeleteUpload where
        toPath = const "/"

instance ToQuery DeleteUpload where
        toQuery = const mempty

-- | Represents the result of a delete upload request.
--
--
--
-- /See:/ 'deleteUploadResponse' smart constructor.
newtype DeleteUploadResponse = DeleteUploadResponse'
  { _dursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dursResponseStatus' - -- | The response status code.
deleteUploadResponse
    :: Int -- ^ 'dursResponseStatus'
    -> DeleteUploadResponse
deleteUploadResponse pResponseStatus_ =
  DeleteUploadResponse' {_dursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dursResponseStatus :: Lens' DeleteUploadResponse Int
dursResponseStatus = lens _dursResponseStatus (\ s a -> s{_dursResponseStatus = a})

instance NFData DeleteUploadResponse where
