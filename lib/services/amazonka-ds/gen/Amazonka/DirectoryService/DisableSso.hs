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
-- Module      : Amazonka.DirectoryService.DisableSso
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables single-sign on for a directory.
module Amazonka.DirectoryService.DisableSso
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the DisableSso operation.
--
-- /See:/ 'newDisableSso' smart constructor.
data DisableSso = DisableSso'
  { -- | The password of an alternate account to use to disable single-sign on.
    -- This is only used for AD Connector directories. For more information,
    -- see the /UserName/ parameter.
    password :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The username of an alternate account to use to disable single-sign on.
    -- This is only used for AD Connector directories. This account must have
    -- privileges to remove a service principal name.
    --
    -- If the AD Connector service account does not have privileges to remove a
    -- service principal name, you can specify an alternate account with the
    -- /UserName/ and /Password/ parameters. These credentials are only used to
    -- disable single sign-on and are not stored by the service. The AD
    -- Connector service account is not changed.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the directory for which to disable single-sign on.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DisableSso
newDisableSso pDirectoryId_ =
  DisableSso'
    { password = Prelude.Nothing,
      userName = Prelude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The password of an alternate account to use to disable single-sign on.
-- This is only used for AD Connector directories. For more information,
-- see the /UserName/ parameter.
disableSso_password :: Lens.Lens' DisableSso (Prelude.Maybe Prelude.Text)
disableSso_password = Lens.lens (\DisableSso' {password} -> password) (\s@DisableSso' {} a -> s {password = a} :: DisableSso) Prelude.. Lens.mapping Core._Sensitive

-- | The username of an alternate account to use to disable single-sign on.
-- This is only used for AD Connector directories. This account must have
-- privileges to remove a service principal name.
--
-- If the AD Connector service account does not have privileges to remove a
-- service principal name, you can specify an alternate account with the
-- /UserName/ and /Password/ parameters. These credentials are only used to
-- disable single sign-on and are not stored by the service. The AD
-- Connector service account is not changed.
disableSso_userName :: Lens.Lens' DisableSso (Prelude.Maybe Prelude.Text)
disableSso_userName = Lens.lens (\DisableSso' {userName} -> userName) (\s@DisableSso' {} a -> s {userName = a} :: DisableSso)

-- | The identifier of the directory for which to disable single-sign on.
disableSso_directoryId :: Lens.Lens' DisableSso Prelude.Text
disableSso_directoryId = Lens.lens (\DisableSso' {directoryId} -> directoryId) (\s@DisableSso' {} a -> s {directoryId = a} :: DisableSso)

instance Core.AWSRequest DisableSso where
  type AWSResponse DisableSso = DisableSsoResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableSsoResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableSso where
  hashWithSalt _salt DisableSso' {..} =
    _salt `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` directoryId

instance Prelude.NFData DisableSso where
  rnf DisableSso' {..} =
    Prelude.rnf password
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf directoryId

instance Core.ToHeaders DisableSso where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DisableSso" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisableSso where
  toJSON DisableSso' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Password" Core..=) Prelude.<$> password,
            ("UserName" Core..=) Prelude.<$> userName,
            Prelude.Just ("DirectoryId" Core..= directoryId)
          ]
      )

instance Core.ToPath DisableSso where
  toPath = Prelude.const "/"

instance Core.ToQuery DisableSso where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the DisableSso operation.
--
-- /See:/ 'newDisableSsoResponse' smart constructor.
data DisableSsoResponse = DisableSsoResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DisableSsoResponse
newDisableSsoResponse pHttpStatus_ =
  DisableSsoResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
disableSsoResponse_httpStatus :: Lens.Lens' DisableSsoResponse Prelude.Int
disableSsoResponse_httpStatus = Lens.lens (\DisableSsoResponse' {httpStatus} -> httpStatus) (\s@DisableSsoResponse' {} a -> s {httpStatus = a} :: DisableSsoResponse)

instance Prelude.NFData DisableSsoResponse where
  rnf DisableSsoResponse' {..} = Prelude.rnf httpStatus
