{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableClientAuthentication' smart constructor.
data DisableClientAuthentication = DisableClientAuthentication'
  { -- | The identifier of the directory
    directoryId :: Prelude.Text,
    -- | The type of client authentication to disable. Currently, only the
    -- parameter, @SmartCard@ is supported.
    type' :: ClientAuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
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
disableClientAuthentication_directoryId :: Lens.Lens' DisableClientAuthentication Prelude.Text
disableClientAuthentication_directoryId = Lens.lens (\DisableClientAuthentication' {directoryId} -> directoryId) (\s@DisableClientAuthentication' {} a -> s {directoryId = a} :: DisableClientAuthentication)

-- | The type of client authentication to disable. Currently, only the
-- parameter, @SmartCard@ is supported.
disableClientAuthentication_type :: Lens.Lens' DisableClientAuthentication ClientAuthenticationType
disableClientAuthentication_type = Lens.lens (\DisableClientAuthentication' {type'} -> type') (\s@DisableClientAuthentication' {} a -> s {type' = a} :: DisableClientAuthentication)

instance
  Prelude.AWSRequest
    DisableClientAuthentication
  where
  type
    Rs DisableClientAuthentication =
      DisableClientAuthenticationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableClientAuthenticationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableClientAuthentication

instance Prelude.NFData DisableClientAuthentication

instance
  Prelude.ToHeaders
    DisableClientAuthentication
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.DisableClientAuthentication" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisableClientAuthentication where
  toJSON DisableClientAuthentication' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Prelude..= directoryId),
            Prelude.Just ("Type" Prelude..= type')
          ]
      )

instance Prelude.ToPath DisableClientAuthentication where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableClientAuthentication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableClientAuthenticationResponse' smart constructor.
data DisableClientAuthenticationResponse = DisableClientAuthenticationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DisableClientAuthenticationResponse
newDisableClientAuthenticationResponse pHttpStatus_ =
  DisableClientAuthenticationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disableClientAuthenticationResponse_httpStatus :: Lens.Lens' DisableClientAuthenticationResponse Prelude.Int
disableClientAuthenticationResponse_httpStatus = Lens.lens (\DisableClientAuthenticationResponse' {httpStatus} -> httpStatus) (\s@DisableClientAuthenticationResponse' {} a -> s {httpStatus = a} :: DisableClientAuthenticationResponse)

instance
  Prelude.NFData
    DisableClientAuthenticationResponse
