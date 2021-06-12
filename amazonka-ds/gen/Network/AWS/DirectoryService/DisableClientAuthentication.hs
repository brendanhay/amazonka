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
-- Module      : Network.AWS.DirectoryService.DisableClientAuthentication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables alternative client authentication methods for the specified
-- directory.
module Network.AWS.DirectoryService.DisableClientAuthentication
  ( -- * Creating a Request
    DisableClientAuthentication (..),
    newDisableClientAuthentication,

    -- * Request Lenses
    disableClientAuthentication_directoryId,
    disableClientAuthentication_type,

    -- * Destructuring the Response
    DisableClientAuthenticationResponse (..),
    newDisableClientAuthenticationResponse,

    -- * Response Lenses
    disableClientAuthenticationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableClientAuthentication' smart constructor.
data DisableClientAuthentication = DisableClientAuthentication'
  { -- | The identifier of the directory
    directoryId :: Core.Text,
    -- | The type of client authentication to disable. Currently, only the
    -- parameter, @SmartCard@ is supported.
    type' :: ClientAuthenticationType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableClientAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'disableClientAuthentication_directoryId' - The identifier of the directory
--
-- 'type'', 'disableClientAuthentication_type' - The type of client authentication to disable. Currently, only the
-- parameter, @SmartCard@ is supported.
newDisableClientAuthentication ::
  -- | 'directoryId'
  Core.Text ->
  -- | 'type''
  ClientAuthenticationType ->
  DisableClientAuthentication
newDisableClientAuthentication pDirectoryId_ pType_ =
  DisableClientAuthentication'
    { directoryId =
        pDirectoryId_,
      type' = pType_
    }

-- | The identifier of the directory
disableClientAuthentication_directoryId :: Lens.Lens' DisableClientAuthentication Core.Text
disableClientAuthentication_directoryId = Lens.lens (\DisableClientAuthentication' {directoryId} -> directoryId) (\s@DisableClientAuthentication' {} a -> s {directoryId = a} :: DisableClientAuthentication)

-- | The type of client authentication to disable. Currently, only the
-- parameter, @SmartCard@ is supported.
disableClientAuthentication_type :: Lens.Lens' DisableClientAuthentication ClientAuthenticationType
disableClientAuthentication_type = Lens.lens (\DisableClientAuthentication' {type'} -> type') (\s@DisableClientAuthentication' {} a -> s {type' = a} :: DisableClientAuthentication)

instance Core.AWSRequest DisableClientAuthentication where
  type
    AWSResponse DisableClientAuthentication =
      DisableClientAuthenticationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableClientAuthenticationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisableClientAuthentication

instance Core.NFData DisableClientAuthentication

instance Core.ToHeaders DisableClientAuthentication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DisableClientAuthentication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisableClientAuthentication where
  toJSON DisableClientAuthentication' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("Type" Core..= type')
          ]
      )

instance Core.ToPath DisableClientAuthentication where
  toPath = Core.const "/"

instance Core.ToQuery DisableClientAuthentication where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisableClientAuthenticationResponse' smart constructor.
data DisableClientAuthenticationResponse = DisableClientAuthenticationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableClientAuthenticationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableClientAuthenticationResponse_httpStatus' - The response's http status code.
newDisableClientAuthenticationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisableClientAuthenticationResponse
newDisableClientAuthenticationResponse pHttpStatus_ =
  DisableClientAuthenticationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disableClientAuthenticationResponse_httpStatus :: Lens.Lens' DisableClientAuthenticationResponse Core.Int
disableClientAuthenticationResponse_httpStatus = Lens.lens (\DisableClientAuthenticationResponse' {httpStatus} -> httpStatus) (\s@DisableClientAuthenticationResponse' {} a -> s {httpStatus = a} :: DisableClientAuthenticationResponse)

instance
  Core.NFData
    DisableClientAuthenticationResponse
