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
-- Module      : Network.AWS.Connect.AssociateSecurityKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a security key to the instance.
module Network.AWS.Connect.AssociateSecurityKey
  ( -- * Creating a Request
    associateSecurityKey,
    AssociateSecurityKey,

    -- * Request Lenses
    askInstanceId,
    askKey,

    -- * Destructuring the Response
    associateSecurityKeyResponse,
    AssociateSecurityKeyResponse,

    -- * Response Lenses
    askrsAssociationId,
    askrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateSecurityKey' smart constructor.
data AssociateSecurityKey = AssociateSecurityKey'
  { _askInstanceId ::
      !Text,
    _askKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateSecurityKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'askInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'askKey' - A valid security key in PEM format.
associateSecurityKey ::
  -- | 'askInstanceId'
  Text ->
  -- | 'askKey'
  Text ->
  AssociateSecurityKey
associateSecurityKey pInstanceId_ pKey_ =
  AssociateSecurityKey'
    { _askInstanceId = pInstanceId_,
      _askKey = pKey_
    }

-- | The identifier of the Amazon Connect instance.
askInstanceId :: Lens' AssociateSecurityKey Text
askInstanceId = lens _askInstanceId (\s a -> s {_askInstanceId = a})

-- | A valid security key in PEM format.
askKey :: Lens' AssociateSecurityKey Text
askKey = lens _askKey (\s a -> s {_askKey = a})

instance AWSRequest AssociateSecurityKey where
  type Rs AssociateSecurityKey = AssociateSecurityKeyResponse
  request = putJSON connect
  response =
    receiveJSON
      ( \s h x ->
          AssociateSecurityKeyResponse'
            <$> (x .?> "AssociationId") <*> (pure (fromEnum s))
      )

instance Hashable AssociateSecurityKey

instance NFData AssociateSecurityKey

instance ToHeaders AssociateSecurityKey where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON AssociateSecurityKey where
  toJSON AssociateSecurityKey' {..} =
    object (catMaybes [Just ("Key" .= _askKey)])

instance ToPath AssociateSecurityKey where
  toPath AssociateSecurityKey' {..} =
    mconcat ["/instance/", toBS _askInstanceId, "/security-key"]

instance ToQuery AssociateSecurityKey where
  toQuery = const mempty

-- | /See:/ 'associateSecurityKeyResponse' smart constructor.
data AssociateSecurityKeyResponse = AssociateSecurityKeyResponse'
  { _askrsAssociationId ::
      !(Maybe Text),
    _askrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateSecurityKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'askrsAssociationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- * 'askrsResponseStatus' - -- | The response status code.
associateSecurityKeyResponse ::
  -- | 'askrsResponseStatus'
  Int ->
  AssociateSecurityKeyResponse
associateSecurityKeyResponse pResponseStatus_ =
  AssociateSecurityKeyResponse'
    { _askrsAssociationId = Nothing,
      _askrsResponseStatus = pResponseStatus_
    }

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
askrsAssociationId :: Lens' AssociateSecurityKeyResponse (Maybe Text)
askrsAssociationId = lens _askrsAssociationId (\s a -> s {_askrsAssociationId = a})

-- | -- | The response status code.
askrsResponseStatus :: Lens' AssociateSecurityKeyResponse Int
askrsResponseStatus = lens _askrsResponseStatus (\s a -> s {_askrsResponseStatus = a})

instance NFData AssociateSecurityKeyResponse
