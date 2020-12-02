{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteDirectoryConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Directory Config object from AppStream 2.0. This object includes the information required to join streaming instances to an Active Directory domain.
module Network.AWS.AppStream.DeleteDirectoryConfig
  ( -- * Creating a Request
    deleteDirectoryConfig,
    DeleteDirectoryConfig,

    -- * Request Lenses
    ddcDirectoryName,

    -- * Destructuring the Response
    deleteDirectoryConfigResponse,
    DeleteDirectoryConfigResponse,

    -- * Response Lenses
    ddcdrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDirectoryConfig' smart constructor.
newtype DeleteDirectoryConfig = DeleteDirectoryConfig'
  { _ddcDirectoryName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDirectoryConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcDirectoryName' - The name of the directory configuration.
deleteDirectoryConfig ::
  -- | 'ddcDirectoryName'
  Text ->
  DeleteDirectoryConfig
deleteDirectoryConfig pDirectoryName_ =
  DeleteDirectoryConfig' {_ddcDirectoryName = pDirectoryName_}

-- | The name of the directory configuration.
ddcDirectoryName :: Lens' DeleteDirectoryConfig Text
ddcDirectoryName = lens _ddcDirectoryName (\s a -> s {_ddcDirectoryName = a})

instance AWSRequest DeleteDirectoryConfig where
  type Rs DeleteDirectoryConfig = DeleteDirectoryConfigResponse
  request = postJSON appStream
  response =
    receiveEmpty
      (\s h x -> DeleteDirectoryConfigResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDirectoryConfig

instance NFData DeleteDirectoryConfig

instance ToHeaders DeleteDirectoryConfig where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("PhotonAdminProxyService.DeleteDirectoryConfig" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteDirectoryConfig where
  toJSON DeleteDirectoryConfig' {..} =
    object (catMaybes [Just ("DirectoryName" .= _ddcDirectoryName)])

instance ToPath DeleteDirectoryConfig where
  toPath = const "/"

instance ToQuery DeleteDirectoryConfig where
  toQuery = const mempty

-- | /See:/ 'deleteDirectoryConfigResponse' smart constructor.
newtype DeleteDirectoryConfigResponse = DeleteDirectoryConfigResponse'
  { _ddcdrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDirectoryConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcdrsResponseStatus' - -- | The response status code.
deleteDirectoryConfigResponse ::
  -- | 'ddcdrsResponseStatus'
  Int ->
  DeleteDirectoryConfigResponse
deleteDirectoryConfigResponse pResponseStatus_ =
  DeleteDirectoryConfigResponse'
    { _ddcdrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ddcdrsResponseStatus :: Lens' DeleteDirectoryConfigResponse Int
ddcdrsResponseStatus = lens _ddcdrsResponseStatus (\s a -> s {_ddcdrsResponseStatus = a})

instance NFData DeleteDirectoryConfigResponse
