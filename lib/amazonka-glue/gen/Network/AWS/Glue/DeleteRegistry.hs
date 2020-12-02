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
-- Module      : Network.AWS.Glue.DeleteRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the entire registry including schema and all of its versions. To get the status of the delete operation, you can call the @GetRegistry@ API after the asynchronous call. Deleting a registry will disable all online operations for the registry such as the @UpdateRegistry@ , @CreateSchema@ , @UpdateSchema@ , and @RegisterSchemaVersion@ APIs.
module Network.AWS.Glue.DeleteRegistry
  ( -- * Creating a Request
    deleteRegistry,
    DeleteRegistry,

    -- * Request Lenses
    drRegistryId,

    -- * Destructuring the Response
    deleteRegistryResponse,
    DeleteRegistryResponse,

    -- * Response Lenses
    drrsStatus,
    drrsRegistryName,
    drrsRegistryARN,
    drrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRegistry' smart constructor.
newtype DeleteRegistry = DeleteRegistry'
  { _drRegistryId ::
      RegistryId
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRegistry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drRegistryId' - This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
deleteRegistry ::
  -- | 'drRegistryId'
  RegistryId ->
  DeleteRegistry
deleteRegistry pRegistryId_ =
  DeleteRegistry' {_drRegistryId = pRegistryId_}

-- | This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
drRegistryId :: Lens' DeleteRegistry RegistryId
drRegistryId = lens _drRegistryId (\s a -> s {_drRegistryId = a})

instance AWSRequest DeleteRegistry where
  type Rs DeleteRegistry = DeleteRegistryResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          DeleteRegistryResponse'
            <$> (x .?> "Status")
            <*> (x .?> "RegistryName")
            <*> (x .?> "RegistryArn")
            <*> (pure (fromEnum s))
      )

instance Hashable DeleteRegistry

instance NFData DeleteRegistry

instance ToHeaders DeleteRegistry where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.DeleteRegistry" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteRegistry where
  toJSON DeleteRegistry' {..} =
    object (catMaybes [Just ("RegistryId" .= _drRegistryId)])

instance ToPath DeleteRegistry where
  toPath = const "/"

instance ToQuery DeleteRegistry where
  toQuery = const mempty

-- | /See:/ 'deleteRegistryResponse' smart constructor.
data DeleteRegistryResponse = DeleteRegistryResponse'
  { _drrsStatus ::
      !(Maybe RegistryStatus),
    _drrsRegistryName :: !(Maybe Text),
    _drrsRegistryARN :: !(Maybe Text),
    _drrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRegistryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsStatus' - The status of the registry. A successful operation will return the @Deleting@ status.
--
-- * 'drrsRegistryName' - The name of the registry being deleted.
--
-- * 'drrsRegistryARN' - The Amazon Resource Name (ARN) of the registry being deleted.
--
-- * 'drrsResponseStatus' - -- | The response status code.
deleteRegistryResponse ::
  -- | 'drrsResponseStatus'
  Int ->
  DeleteRegistryResponse
deleteRegistryResponse pResponseStatus_ =
  DeleteRegistryResponse'
    { _drrsStatus = Nothing,
      _drrsRegistryName = Nothing,
      _drrsRegistryARN = Nothing,
      _drrsResponseStatus = pResponseStatus_
    }

-- | The status of the registry. A successful operation will return the @Deleting@ status.
drrsStatus :: Lens' DeleteRegistryResponse (Maybe RegistryStatus)
drrsStatus = lens _drrsStatus (\s a -> s {_drrsStatus = a})

-- | The name of the registry being deleted.
drrsRegistryName :: Lens' DeleteRegistryResponse (Maybe Text)
drrsRegistryName = lens _drrsRegistryName (\s a -> s {_drrsRegistryName = a})

-- | The Amazon Resource Name (ARN) of the registry being deleted.
drrsRegistryARN :: Lens' DeleteRegistryResponse (Maybe Text)
drrsRegistryARN = lens _drrsRegistryARN (\s a -> s {_drrsRegistryARN = a})

-- | -- | The response status code.
drrsResponseStatus :: Lens' DeleteRegistryResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\s a -> s {_drrsResponseStatus = a})

instance NFData DeleteRegistryResponse
