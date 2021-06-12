{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.EnableClientAuthentication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables alternative client authentication methods for the specified
-- directory.
module Network.AWS.DirectoryService.EnableClientAuthentication
  ( -- * Creating a Request
    EnableClientAuthentication (..),
    newEnableClientAuthentication,

    -- * Request Lenses
    enableClientAuthentication_directoryId,
    enableClientAuthentication_type,

    -- * Destructuring the Response
    EnableClientAuthenticationResponse (..),
    newEnableClientAuthenticationResponse,

    -- * Response Lenses
    enableClientAuthenticationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableClientAuthentication' smart constructor.
data EnableClientAuthentication = EnableClientAuthentication'
  { -- | The identifier of the specified directory.
    directoryId :: Core.Text,
    -- | The type of client authentication to enable. Currently only the value
    -- @SmartCard@ is supported. Smart card authentication in AD Connector
    -- requires that you enable Kerberos Constrained Delegation for the Service
    -- User to the LDAP service in the on-premises AD.
    type' :: ClientAuthenticationType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableClientAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'enableClientAuthentication_directoryId' - The identifier of the specified directory.
--
-- 'type'', 'enableClientAuthentication_type' - The type of client authentication to enable. Currently only the value
-- @SmartCard@ is supported. Smart card authentication in AD Connector
-- requires that you enable Kerberos Constrained Delegation for the Service
-- User to the LDAP service in the on-premises AD.
newEnableClientAuthentication ::
  -- | 'directoryId'
  Core.Text ->
  -- | 'type''
  ClientAuthenticationType ->
  EnableClientAuthentication
newEnableClientAuthentication pDirectoryId_ pType_ =
  EnableClientAuthentication'
    { directoryId =
        pDirectoryId_,
      type' = pType_
    }

-- | The identifier of the specified directory.
enableClientAuthentication_directoryId :: Lens.Lens' EnableClientAuthentication Core.Text
enableClientAuthentication_directoryId = Lens.lens (\EnableClientAuthentication' {directoryId} -> directoryId) (\s@EnableClientAuthentication' {} a -> s {directoryId = a} :: EnableClientAuthentication)

-- | The type of client authentication to enable. Currently only the value
-- @SmartCard@ is supported. Smart card authentication in AD Connector
-- requires that you enable Kerberos Constrained Delegation for the Service
-- User to the LDAP service in the on-premises AD.
enableClientAuthentication_type :: Lens.Lens' EnableClientAuthentication ClientAuthenticationType
enableClientAuthentication_type = Lens.lens (\EnableClientAuthentication' {type'} -> type') (\s@EnableClientAuthentication' {} a -> s {type' = a} :: EnableClientAuthentication)

instance Core.AWSRequest EnableClientAuthentication where
  type
    AWSResponse EnableClientAuthentication =
      EnableClientAuthenticationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableClientAuthenticationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable EnableClientAuthentication

instance Core.NFData EnableClientAuthentication

instance Core.ToHeaders EnableClientAuthentication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.EnableClientAuthentication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON EnableClientAuthentication where
  toJSON EnableClientAuthentication' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("Type" Core..= type')
          ]
      )

instance Core.ToPath EnableClientAuthentication where
  toPath = Core.const "/"

instance Core.ToQuery EnableClientAuthentication where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEnableClientAuthenticationResponse' smart constructor.
data EnableClientAuthenticationResponse = EnableClientAuthenticationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableClientAuthenticationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableClientAuthenticationResponse_httpStatus' - The response's http status code.
newEnableClientAuthenticationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  EnableClientAuthenticationResponse
newEnableClientAuthenticationResponse pHttpStatus_ =
  EnableClientAuthenticationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
enableClientAuthenticationResponse_httpStatus :: Lens.Lens' EnableClientAuthenticationResponse Core.Int
enableClientAuthenticationResponse_httpStatus = Lens.lens (\EnableClientAuthenticationResponse' {httpStatus} -> httpStatus) (\s@EnableClientAuthenticationResponse' {} a -> s {httpStatus = a} :: EnableClientAuthenticationResponse)

instance
  Core.NFData
    EnableClientAuthenticationResponse
