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
-- Module      : Network.AWS.DirectoryService.EnableSso
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables single sign-on for a directory. Single sign-on allows users in
-- your directory to access certain AWS services from a computer joined to
-- the directory without having to enter their credentials separately.
module Network.AWS.DirectoryService.EnableSso
  ( -- * Creating a Request
    EnableSso (..),
    newEnableSso,

    -- * Request Lenses
    enableSso_password,
    enableSso_userName,
    enableSso_directoryId,

    -- * Destructuring the Response
    EnableSsoResponse (..),
    newEnableSsoResponse,

    -- * Response Lenses
    enableSsoResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the EnableSso operation.
--
-- /See:/ 'newEnableSso' smart constructor.
data EnableSso = EnableSso'
  { -- | The password of an alternate account to use to enable single-sign on.
    -- This is only used for AD Connector directories. For more information,
    -- see the /UserName/ parameter.
    password :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The username of an alternate account to use to enable single-sign on.
    -- This is only used for AD Connector directories. This account must have
    -- privileges to add a service principal name.
    --
    -- If the AD Connector service account does not have privileges to add a
    -- service principal name, you can specify an alternate account with the
    -- /UserName/ and /Password/ parameters. These credentials are only used to
    -- enable single sign-on and are not stored by the service. The AD
    -- Connector service account is not changed.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the directory for which to enable single-sign on.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableSso' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'password', 'enableSso_password' - The password of an alternate account to use to enable single-sign on.
-- This is only used for AD Connector directories. For more information,
-- see the /UserName/ parameter.
--
-- 'userName', 'enableSso_userName' - The username of an alternate account to use to enable single-sign on.
-- This is only used for AD Connector directories. This account must have
-- privileges to add a service principal name.
--
-- If the AD Connector service account does not have privileges to add a
-- service principal name, you can specify an alternate account with the
-- /UserName/ and /Password/ parameters. These credentials are only used to
-- enable single sign-on and are not stored by the service. The AD
-- Connector service account is not changed.
--
-- 'directoryId', 'enableSso_directoryId' - The identifier of the directory for which to enable single-sign on.
newEnableSso ::
  -- | 'directoryId'
  Prelude.Text ->
  EnableSso
newEnableSso pDirectoryId_ =
  EnableSso'
    { password = Prelude.Nothing,
      userName = Prelude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The password of an alternate account to use to enable single-sign on.
-- This is only used for AD Connector directories. For more information,
-- see the /UserName/ parameter.
enableSso_password :: Lens.Lens' EnableSso (Prelude.Maybe Prelude.Text)
enableSso_password = Lens.lens (\EnableSso' {password} -> password) (\s@EnableSso' {} a -> s {password = a} :: EnableSso) Prelude.. Lens.mapping Prelude._Sensitive

-- | The username of an alternate account to use to enable single-sign on.
-- This is only used for AD Connector directories. This account must have
-- privileges to add a service principal name.
--
-- If the AD Connector service account does not have privileges to add a
-- service principal name, you can specify an alternate account with the
-- /UserName/ and /Password/ parameters. These credentials are only used to
-- enable single sign-on and are not stored by the service. The AD
-- Connector service account is not changed.
enableSso_userName :: Lens.Lens' EnableSso (Prelude.Maybe Prelude.Text)
enableSso_userName = Lens.lens (\EnableSso' {userName} -> userName) (\s@EnableSso' {} a -> s {userName = a} :: EnableSso)

-- | The identifier of the directory for which to enable single-sign on.
enableSso_directoryId :: Lens.Lens' EnableSso Prelude.Text
enableSso_directoryId = Lens.lens (\EnableSso' {directoryId} -> directoryId) (\s@EnableSso' {} a -> s {directoryId = a} :: EnableSso)

instance Prelude.AWSRequest EnableSso where
  type Rs EnableSso = EnableSsoResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableSsoResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableSso

instance Prelude.NFData EnableSso

instance Prelude.ToHeaders EnableSso where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.EnableSso" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON EnableSso where
  toJSON EnableSso' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Password" Prelude..=) Prelude.<$> password,
            ("UserName" Prelude..=) Prelude.<$> userName,
            Prelude.Just ("DirectoryId" Prelude..= directoryId)
          ]
      )

instance Prelude.ToPath EnableSso where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableSso where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the EnableSso operation.
--
-- /See:/ 'newEnableSsoResponse' smart constructor.
data EnableSsoResponse = EnableSsoResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableSsoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableSsoResponse_httpStatus' - The response's http status code.
newEnableSsoResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableSsoResponse
newEnableSsoResponse pHttpStatus_ =
  EnableSsoResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
enableSsoResponse_httpStatus :: Lens.Lens' EnableSsoResponse Prelude.Int
enableSsoResponse_httpStatus = Lens.lens (\EnableSsoResponse' {httpStatus} -> httpStatus) (\s@EnableSsoResponse' {} a -> s {httpStatus = a} :: EnableSsoResponse)

instance Prelude.NFData EnableSsoResponse
