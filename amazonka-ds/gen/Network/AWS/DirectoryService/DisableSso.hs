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

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the DisableSso operation.
--
-- /See:/ 'newDisableSso' smart constructor.
data DisableSso = DisableSso'
  { -- | The password of an alternate account to use to disable single-sign on.
    -- This is only used for AD Connector directories. For more information,
    -- see the /UserName/ parameter.
    password :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
disableSso_password = Lens.lens (\DisableSso' {password} -> password) (\s@DisableSso' {} a -> s {password = a} :: DisableSso) Prelude.. Lens.mapping Prelude._Sensitive

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

instance Prelude.AWSRequest DisableSso where
  type Rs DisableSso = DisableSsoResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableSsoResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableSso

instance Prelude.NFData DisableSso

instance Prelude.ToHeaders DisableSso where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.DisableSso" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisableSso where
  toJSON DisableSso' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Password" Prelude..=) Prelude.<$> password,
            ("UserName" Prelude..=) Prelude.<$> userName,
            Prelude.Just ("DirectoryId" Prelude..= directoryId)
          ]
      )

instance Prelude.ToPath DisableSso where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableSso where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the DisableSso operation.
--
-- /See:/ 'newDisableSsoResponse' smart constructor.
data DisableSsoResponse = DisableSsoResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DisableSsoResponse
