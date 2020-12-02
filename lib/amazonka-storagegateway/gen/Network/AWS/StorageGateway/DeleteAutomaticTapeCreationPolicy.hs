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
-- Module      : Network.AWS.StorageGateway.DeleteAutomaticTapeCreationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the automatic tape creation policy of a gateway. If you delete this policy, new virtual tapes must be created manually. Use the Amazon Resource Name (ARN) of the gateway in your request to remove the policy.
module Network.AWS.StorageGateway.DeleteAutomaticTapeCreationPolicy
  ( -- * Creating a Request
    deleteAutomaticTapeCreationPolicy,
    DeleteAutomaticTapeCreationPolicy,

    -- * Request Lenses
    datcpGatewayARN,

    -- * Destructuring the Response
    deleteAutomaticTapeCreationPolicyResponse,
    DeleteAutomaticTapeCreationPolicyResponse,

    -- * Response Lenses
    datcprsGatewayARN,
    datcprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'deleteAutomaticTapeCreationPolicy' smart constructor.
newtype DeleteAutomaticTapeCreationPolicy = DeleteAutomaticTapeCreationPolicy'
  { _datcpGatewayARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAutomaticTapeCreationPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datcpGatewayARN' - Undocumented member.
deleteAutomaticTapeCreationPolicy ::
  -- | 'datcpGatewayARN'
  Text ->
  DeleteAutomaticTapeCreationPolicy
deleteAutomaticTapeCreationPolicy pGatewayARN_ =
  DeleteAutomaticTapeCreationPolicy'
    { _datcpGatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
datcpGatewayARN :: Lens' DeleteAutomaticTapeCreationPolicy Text
datcpGatewayARN = lens _datcpGatewayARN (\s a -> s {_datcpGatewayARN = a})

instance AWSRequest DeleteAutomaticTapeCreationPolicy where
  type
    Rs DeleteAutomaticTapeCreationPolicy =
      DeleteAutomaticTapeCreationPolicyResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          DeleteAutomaticTapeCreationPolicyResponse'
            <$> (x .?> "GatewayARN") <*> (pure (fromEnum s))
      )

instance Hashable DeleteAutomaticTapeCreationPolicy

instance NFData DeleteAutomaticTapeCreationPolicy

instance ToHeaders DeleteAutomaticTapeCreationPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StorageGateway_20130630.DeleteAutomaticTapeCreationPolicy" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteAutomaticTapeCreationPolicy where
  toJSON DeleteAutomaticTapeCreationPolicy' {..} =
    object (catMaybes [Just ("GatewayARN" .= _datcpGatewayARN)])

instance ToPath DeleteAutomaticTapeCreationPolicy where
  toPath = const "/"

instance ToQuery DeleteAutomaticTapeCreationPolicy where
  toQuery = const mempty

-- | /See:/ 'deleteAutomaticTapeCreationPolicyResponse' smart constructor.
data DeleteAutomaticTapeCreationPolicyResponse = DeleteAutomaticTapeCreationPolicyResponse'
  { _datcprsGatewayARN ::
      !( Maybe
           Text
       ),
    _datcprsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteAutomaticTapeCreationPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datcprsGatewayARN' - Undocumented member.
--
-- * 'datcprsResponseStatus' - -- | The response status code.
deleteAutomaticTapeCreationPolicyResponse ::
  -- | 'datcprsResponseStatus'
  Int ->
  DeleteAutomaticTapeCreationPolicyResponse
deleteAutomaticTapeCreationPolicyResponse pResponseStatus_ =
  DeleteAutomaticTapeCreationPolicyResponse'
    { _datcprsGatewayARN =
        Nothing,
      _datcprsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
datcprsGatewayARN :: Lens' DeleteAutomaticTapeCreationPolicyResponse (Maybe Text)
datcprsGatewayARN = lens _datcprsGatewayARN (\s a -> s {_datcprsGatewayARN = a})

-- | -- | The response status code.
datcprsResponseStatus :: Lens' DeleteAutomaticTapeCreationPolicyResponse Int
datcprsResponseStatus = lens _datcprsResponseStatus (\s a -> s {_datcprsResponseStatus = a})

instance NFData DeleteAutomaticTapeCreationPolicyResponse
