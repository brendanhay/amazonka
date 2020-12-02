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
-- Module      : Network.AWS.Lightsail.DeleteContainerService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes your Amazon Lightsail container service.
module Network.AWS.Lightsail.DeleteContainerService
  ( -- * Creating a Request
    deleteContainerService,
    DeleteContainerService,

    -- * Request Lenses
    dcsServiceName,

    -- * Destructuring the Response
    deleteContainerServiceResponse,
    DeleteContainerServiceResponse,

    -- * Response Lenses
    dcsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteContainerService' smart constructor.
newtype DeleteContainerService = DeleteContainerService'
  { _dcsServiceName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteContainerService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsServiceName' - The name of the container service to delete.
deleteContainerService ::
  -- | 'dcsServiceName'
  Text ->
  DeleteContainerService
deleteContainerService pServiceName_ =
  DeleteContainerService' {_dcsServiceName = pServiceName_}

-- | The name of the container service to delete.
dcsServiceName :: Lens' DeleteContainerService Text
dcsServiceName = lens _dcsServiceName (\s a -> s {_dcsServiceName = a})

instance AWSRequest DeleteContainerService where
  type Rs DeleteContainerService = DeleteContainerServiceResponse
  request = postJSON lightsail
  response =
    receiveEmpty
      ( \s h x ->
          DeleteContainerServiceResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteContainerService

instance NFData DeleteContainerService

instance ToHeaders DeleteContainerService where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.DeleteContainerService" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteContainerService where
  toJSON DeleteContainerService' {..} =
    object (catMaybes [Just ("serviceName" .= _dcsServiceName)])

instance ToPath DeleteContainerService where
  toPath = const "/"

instance ToQuery DeleteContainerService where
  toQuery = const mempty

-- | /See:/ 'deleteContainerServiceResponse' smart constructor.
newtype DeleteContainerServiceResponse = DeleteContainerServiceResponse'
  { _dcsrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteContainerServiceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsrsResponseStatus' - -- | The response status code.
deleteContainerServiceResponse ::
  -- | 'dcsrsResponseStatus'
  Int ->
  DeleteContainerServiceResponse
deleteContainerServiceResponse pResponseStatus_ =
  DeleteContainerServiceResponse'
    { _dcsrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dcsrsResponseStatus :: Lens' DeleteContainerServiceResponse Int
dcsrsResponseStatus = lens _dcsrsResponseStatus (\s a -> s {_dcsrsResponseStatus = a})

instance NFData DeleteContainerServiceResponse
