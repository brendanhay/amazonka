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
-- Module      : Network.AWS.AppStream.DeleteDirectoryConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified directory configuration.
--
--
module Network.AWS.AppStream.DeleteDirectoryConfig
    (
    -- * Creating a Request
      deleteDirectoryConfig
    , DeleteDirectoryConfig
    -- * Request Lenses
    , ddcDirectoryName

    -- * Destructuring the Response
    , deleteDirectoryConfigResponse
    , DeleteDirectoryConfigResponse
    -- * Response Lenses
    , delrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDirectoryConfig' smart constructor.
newtype DeleteDirectoryConfig = DeleteDirectoryConfig'
  { _ddcDirectoryName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDirectoryConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcDirectoryName' - The name of the directory configuration.
deleteDirectoryConfig
    :: Text -- ^ 'ddcDirectoryName'
    -> DeleteDirectoryConfig
deleteDirectoryConfig pDirectoryName_ =
  DeleteDirectoryConfig' {_ddcDirectoryName = pDirectoryName_}


-- | The name of the directory configuration.
ddcDirectoryName :: Lens' DeleteDirectoryConfig Text
ddcDirectoryName = lens _ddcDirectoryName (\ s a -> s{_ddcDirectoryName = a})

instance AWSRequest DeleteDirectoryConfig where
        type Rs DeleteDirectoryConfig =
             DeleteDirectoryConfigResponse
        request = postJSON appStream
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteDirectoryConfigResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteDirectoryConfig where

instance NFData DeleteDirectoryConfig where

instance ToHeaders DeleteDirectoryConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DeleteDirectoryConfig" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDirectoryConfig where
        toJSON DeleteDirectoryConfig'{..}
          = object
              (catMaybes
                 [Just ("DirectoryName" .= _ddcDirectoryName)])

instance ToPath DeleteDirectoryConfig where
        toPath = const "/"

instance ToQuery DeleteDirectoryConfig where
        toQuery = const mempty

-- | /See:/ 'deleteDirectoryConfigResponse' smart constructor.
newtype DeleteDirectoryConfigResponse = DeleteDirectoryConfigResponse'
  { _delrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDirectoryConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteDirectoryConfigResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteDirectoryConfigResponse
deleteDirectoryConfigResponse pResponseStatus_ =
  DeleteDirectoryConfigResponse' {_delrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteDirectoryConfigResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteDirectoryConfigResponse where
