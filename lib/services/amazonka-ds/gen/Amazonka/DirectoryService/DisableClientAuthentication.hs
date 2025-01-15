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
-- Module      : Amazonka.DirectoryService.DisableClientAuthentication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables alternative client authentication methods for the specified
-- directory.
module Amazonka.DirectoryService.DisableClientAuthentication
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableClientAuthentication' smart constructor.
data DisableClientAuthentication = DisableClientAuthentication'
  { -- | The identifier of the directory
    directoryId :: Prelude.Text,
    -- | The type of client authentication to disable. Currently, only the
    -- parameter, @SmartCard@ is supported.
    type' :: ClientAuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DisableClientAuthentication where
  type
    AWSResponse DisableClientAuthentication =
      DisableClientAuthenticationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableClientAuthenticationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableClientAuthentication where
  hashWithSalt _salt DisableClientAuthentication' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DisableClientAuthentication where
  rnf DisableClientAuthentication' {..} =
    Prelude.rnf directoryId `Prelude.seq`
      Prelude.rnf type'

instance Data.ToHeaders DisableClientAuthentication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.DisableClientAuthentication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableClientAuthentication where
  toJSON DisableClientAuthentication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath DisableClientAuthentication where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableClientAuthentication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableClientAuthenticationResponse' smart constructor.
data DisableClientAuthenticationResponse = DisableClientAuthenticationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DisableClientAuthenticationResponse' {..} =
    Prelude.rnf httpStatus
