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
-- Module      : Network.AWS.DirectoryService.DisableSso
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables single-sign on for a directory.
module Network.AWS.DirectoryService.DisableSso
  ( -- * Creating a Request
    DisableSso (..),
    newDisableSso,

    -- * Request Lenses
    disableSso_password,
    disableSso_userName,
    disableSso_directoryId,

    -- * Destructuring the Response
    DisableSsoResponse (..),
    newDisableSsoResponse,

    -- * Response Lenses
    disableSsoResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the DisableSso operation.
--
-- /See:/ 'newDisableSso' smart constructor.
data DisableSso = DisableSso'
  { -- | The password of an alternate account to use to disable single-sign on.
    -- This is only used for AD Connector directories. For more information,
    -- see the /UserName/ parameter.
    password :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The username of an alternate account to use to disable single-sign on.
    -- This is only used for AD Connector directories. This account must have
    -- privileges to remove a service principal name.
    --
    -- If the AD Connector service account does not have privileges to remove a
    -- service principal name, you can specify an alternate account with the
    -- /UserName/ and /Password/ parameters. These credentials are only used to
    -- disable single sign-on and are not stored by the service. The AD
    -- Connector service account is not changed.
    userName :: Core.Maybe Core.Text,
    -- | The identifier of the directory for which to disable single-sign on.
    directoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableSso' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'password', 'disableSso_password' - The password of an alternate account to use to disable single-sign on.
-- This is only used for AD Connector directories. For more information,
-- see the /UserName/ parameter.
--
-- 'userName', 'disableSso_userName' - The username of an alternate account to use to disable single-sign on.
-- This is only used for AD Connector directories. This account must have
-- privileges to remove a service principal name.
--
-- If the AD Connector service account does not have privileges to remove a
-- service principal name, you can specify an alternate account with the
-- /UserName/ and /Password/ parameters. These credentials are only used to
-- disable single sign-on and are not stored by the service. The AD
-- Connector service account is not changed.
--
-- 'directoryId', 'disableSso_directoryId' - The identifier of the directory for which to disable single-sign on.
newDisableSso ::
  -- | 'directoryId'
  Core.Text ->
  DisableSso
newDisableSso pDirectoryId_ =
  DisableSso'
    { password = Core.Nothing,
      userName = Core.Nothing,
      directoryId = pDirectoryId_
    }

-- | The password of an alternate account to use to disable single-sign on.
-- This is only used for AD Connector directories. For more information,
-- see the /UserName/ parameter.
disableSso_password :: Lens.Lens' DisableSso (Core.Maybe Core.Text)
disableSso_password = Lens.lens (\DisableSso' {password} -> password) (\s@DisableSso' {} a -> s {password = a} :: DisableSso) Core.. Lens.mapping Core._Sensitive

-- | The username of an alternate account to use to disable single-sign on.
-- This is only used for AD Connector directories. This account must have
-- privileges to remove a service principal name.
--
-- If the AD Connector service account does not have privileges to remove a
-- service principal name, you can specify an alternate account with the
-- /UserName/ and /Password/ parameters. These credentials are only used to
-- disable single sign-on and are not stored by the service. The AD
-- Connector service account is not changed.
disableSso_userName :: Lens.Lens' DisableSso (Core.Maybe Core.Text)
disableSso_userName = Lens.lens (\DisableSso' {userName} -> userName) (\s@DisableSso' {} a -> s {userName = a} :: DisableSso)

-- | The identifier of the directory for which to disable single-sign on.
disableSso_directoryId :: Lens.Lens' DisableSso Core.Text
disableSso_directoryId = Lens.lens (\DisableSso' {directoryId} -> directoryId) (\s@DisableSso' {} a -> s {directoryId = a} :: DisableSso)

instance Core.AWSRequest DisableSso where
  type AWSResponse DisableSso = DisableSsoResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableSsoResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisableSso

instance Core.NFData DisableSso

instance Core.ToHeaders DisableSso where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DisableSso" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisableSso where
  toJSON DisableSso' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Password" Core..=) Core.<$> password,
            ("UserName" Core..=) Core.<$> userName,
            Core.Just ("DirectoryId" Core..= directoryId)
          ]
      )

instance Core.ToPath DisableSso where
  toPath = Core.const "/"

instance Core.ToQuery DisableSso where
  toQuery = Core.const Core.mempty

-- | Contains the results of the DisableSso operation.
--
-- /See:/ 'newDisableSsoResponse' smart constructor.
data DisableSsoResponse = DisableSsoResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableSsoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableSsoResponse_httpStatus' - The response's http status code.
newDisableSsoResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisableSsoResponse
newDisableSsoResponse pHttpStatus_ =
  DisableSsoResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
disableSsoResponse_httpStatus :: Lens.Lens' DisableSsoResponse Core.Int
disableSsoResponse_httpStatus = Lens.lens (\DisableSsoResponse' {httpStatus} -> httpStatus) (\s@DisableSsoResponse' {} a -> s {httpStatus = a} :: DisableSsoResponse)

instance Core.NFData DisableSsoResponse
