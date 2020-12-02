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
-- Module      : Network.AWS.Lightsail.DeleteInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Lightsail instance.
--
--
-- The @delete instance@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteInstance
  ( -- * Creating a Request
    deleteInstance,
    DeleteInstance,

    -- * Request Lenses
    diForceDeleteAddOns,
    diInstanceName,

    -- * Destructuring the Response
    deleteInstanceResponse,
    DeleteInstanceResponse,

    -- * Response Lenses
    dirsOperations,
    dirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteInstance' smart constructor.
data DeleteInstance = DeleteInstance'
  { _diForceDeleteAddOns ::
      !(Maybe Bool),
    _diInstanceName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diForceDeleteAddOns' - A Boolean value to indicate whether to delete the enabled add-ons for the disk.
--
-- * 'diInstanceName' - The name of the instance to delete.
deleteInstance ::
  -- | 'diInstanceName'
  Text ->
  DeleteInstance
deleteInstance pInstanceName_ =
  DeleteInstance'
    { _diForceDeleteAddOns = Nothing,
      _diInstanceName = pInstanceName_
    }

-- | A Boolean value to indicate whether to delete the enabled add-ons for the disk.
diForceDeleteAddOns :: Lens' DeleteInstance (Maybe Bool)
diForceDeleteAddOns = lens _diForceDeleteAddOns (\s a -> s {_diForceDeleteAddOns = a})

-- | The name of the instance to delete.
diInstanceName :: Lens' DeleteInstance Text
diInstanceName = lens _diInstanceName (\s a -> s {_diInstanceName = a})

instance AWSRequest DeleteInstance where
  type Rs DeleteInstance = DeleteInstanceResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          DeleteInstanceResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DeleteInstance

instance NFData DeleteInstance

instance ToHeaders DeleteInstance where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.DeleteInstance" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteInstance where
  toJSON DeleteInstance' {..} =
    object
      ( catMaybes
          [ ("forceDeleteAddOns" .=) <$> _diForceDeleteAddOns,
            Just ("instanceName" .= _diInstanceName)
          ]
      )

instance ToPath DeleteInstance where
  toPath = const "/"

instance ToQuery DeleteInstance where
  toQuery = const mempty

-- | /See:/ 'deleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
  { _dirsOperations ::
      !(Maybe [Operation]),
    _dirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'dirsResponseStatus' - -- | The response status code.
deleteInstanceResponse ::
  -- | 'dirsResponseStatus'
  Int ->
  DeleteInstanceResponse
deleteInstanceResponse pResponseStatus_ =
  DeleteInstanceResponse'
    { _dirsOperations = Nothing,
      _dirsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
dirsOperations :: Lens' DeleteInstanceResponse [Operation]
dirsOperations = lens _dirsOperations (\s a -> s {_dirsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
dirsResponseStatus :: Lens' DeleteInstanceResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\s a -> s {_dirsResponseStatus = a})

instance NFData DeleteInstanceResponse
