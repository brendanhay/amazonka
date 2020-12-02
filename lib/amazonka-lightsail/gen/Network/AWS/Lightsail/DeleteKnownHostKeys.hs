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
-- Module      : Network.AWS.Lightsail.DeleteKnownHostKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the known host key or certificate used by the Amazon Lightsail browser-based SSH or RDP clients to authenticate an instance. This operation enables the Lightsail browser-based SSH or RDP clients to connect to the instance after a host key mismatch.
--
--
-- /Important:/ Perform this operation only if you were expecting the host key or certificate mismatch or if you are familiar with the new host key or certificate on the instance. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-troubleshooting-browser-based-ssh-rdp-client-connection Troubleshooting connection issues when using the Amazon Lightsail browser-based SSH or RDP client> .
module Network.AWS.Lightsail.DeleteKnownHostKeys
  ( -- * Creating a Request
    deleteKnownHostKeys,
    DeleteKnownHostKeys,

    -- * Request Lenses
    dkhkInstanceName,

    -- * Destructuring the Response
    deleteKnownHostKeysResponse,
    DeleteKnownHostKeysResponse,

    -- * Response Lenses
    dkhkrsOperations,
    dkhkrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteKnownHostKeys' smart constructor.
newtype DeleteKnownHostKeys = DeleteKnownHostKeys'
  { _dkhkInstanceName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteKnownHostKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkhkInstanceName' - The name of the instance for which you want to reset the host key or certificate.
deleteKnownHostKeys ::
  -- | 'dkhkInstanceName'
  Text ->
  DeleteKnownHostKeys
deleteKnownHostKeys pInstanceName_ =
  DeleteKnownHostKeys' {_dkhkInstanceName = pInstanceName_}

-- | The name of the instance for which you want to reset the host key or certificate.
dkhkInstanceName :: Lens' DeleteKnownHostKeys Text
dkhkInstanceName = lens _dkhkInstanceName (\s a -> s {_dkhkInstanceName = a})

instance AWSRequest DeleteKnownHostKeys where
  type Rs DeleteKnownHostKeys = DeleteKnownHostKeysResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          DeleteKnownHostKeysResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DeleteKnownHostKeys

instance NFData DeleteKnownHostKeys

instance ToHeaders DeleteKnownHostKeys where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.DeleteKnownHostKeys" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteKnownHostKeys where
  toJSON DeleteKnownHostKeys' {..} =
    object (catMaybes [Just ("instanceName" .= _dkhkInstanceName)])

instance ToPath DeleteKnownHostKeys where
  toPath = const "/"

instance ToQuery DeleteKnownHostKeys where
  toQuery = const mempty

-- | /See:/ 'deleteKnownHostKeysResponse' smart constructor.
data DeleteKnownHostKeysResponse = DeleteKnownHostKeysResponse'
  { _dkhkrsOperations ::
      !(Maybe [Operation]),
    _dkhkrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteKnownHostKeysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkhkrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'dkhkrsResponseStatus' - -- | The response status code.
deleteKnownHostKeysResponse ::
  -- | 'dkhkrsResponseStatus'
  Int ->
  DeleteKnownHostKeysResponse
deleteKnownHostKeysResponse pResponseStatus_ =
  DeleteKnownHostKeysResponse'
    { _dkhkrsOperations = Nothing,
      _dkhkrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
dkhkrsOperations :: Lens' DeleteKnownHostKeysResponse [Operation]
dkhkrsOperations = lens _dkhkrsOperations (\s a -> s {_dkhkrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
dkhkrsResponseStatus :: Lens' DeleteKnownHostKeysResponse Int
dkhkrsResponseStatus = lens _dkhkrsResponseStatus (\s a -> s {_dkhkrsResponseStatus = a})

instance NFData DeleteKnownHostKeysResponse
