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
-- Module      : Network.AWS.Connect.DisassociateSecurityKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified security key.
module Network.AWS.Connect.DisassociateSecurityKey
  ( -- * Creating a Request
    disassociateSecurityKey,
    DisassociateSecurityKey,

    -- * Request Lenses
    dskInstanceId,
    dskAssociationId,

    -- * Destructuring the Response
    disassociateSecurityKeyResponse,
    DisassociateSecurityKeyResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateSecurityKey' smart constructor.
data DisassociateSecurityKey = DisassociateSecurityKey'
  { _dskInstanceId ::
      !Text,
    _dskAssociationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateSecurityKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dskInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'dskAssociationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
disassociateSecurityKey ::
  -- | 'dskInstanceId'
  Text ->
  -- | 'dskAssociationId'
  Text ->
  DisassociateSecurityKey
disassociateSecurityKey pInstanceId_ pAssociationId_ =
  DisassociateSecurityKey'
    { _dskInstanceId = pInstanceId_,
      _dskAssociationId = pAssociationId_
    }

-- | The identifier of the Amazon Connect instance.
dskInstanceId :: Lens' DisassociateSecurityKey Text
dskInstanceId = lens _dskInstanceId (\s a -> s {_dskInstanceId = a})

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
dskAssociationId :: Lens' DisassociateSecurityKey Text
dskAssociationId = lens _dskAssociationId (\s a -> s {_dskAssociationId = a})

instance AWSRequest DisassociateSecurityKey where
  type Rs DisassociateSecurityKey = DisassociateSecurityKeyResponse
  request = delete connect
  response = receiveNull DisassociateSecurityKeyResponse'

instance Hashable DisassociateSecurityKey

instance NFData DisassociateSecurityKey

instance ToHeaders DisassociateSecurityKey where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DisassociateSecurityKey where
  toPath DisassociateSecurityKey' {..} =
    mconcat
      [ "/instance/",
        toBS _dskInstanceId,
        "/security-key/",
        toBS _dskAssociationId
      ]

instance ToQuery DisassociateSecurityKey where
  toQuery = const mempty

-- | /See:/ 'disassociateSecurityKeyResponse' smart constructor.
data DisassociateSecurityKeyResponse = DisassociateSecurityKeyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateSecurityKeyResponse' with the minimum fields required to make a request.
disassociateSecurityKeyResponse ::
  DisassociateSecurityKeyResponse
disassociateSecurityKeyResponse = DisassociateSecurityKeyResponse'

instance NFData DisassociateSecurityKeyResponse
