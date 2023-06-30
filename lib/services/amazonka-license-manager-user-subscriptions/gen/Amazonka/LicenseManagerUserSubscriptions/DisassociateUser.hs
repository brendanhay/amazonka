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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.DisassociateUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the user from an EC2 instance providing user-based
-- subscriptions.
module Amazonka.LicenseManagerUserSubscriptions.DisassociateUser
  ( -- * Creating a Request
    DisassociateUser (..),
    newDisassociateUser,

    -- * Request Lenses
    disassociateUser_domain,
    disassociateUser_identityProvider,
    disassociateUser_instanceId,
    disassociateUser_username,

    -- * Destructuring the Response
    DisassociateUserResponse (..),
    newDisassociateUserResponse,

    -- * Response Lenses
    disassociateUserResponse_httpStatus,
    disassociateUserResponse_instanceUserSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateUser' smart constructor.
data DisassociateUser = DisassociateUser'
  { -- | The domain name of the user.
    domain :: Prelude.Maybe Prelude.Text,
    -- | An object that specifies details for the identity provider.
    identityProvider :: IdentityProvider,
    -- | The ID of the EC2 instance, which provides user-based subscriptions.
    instanceId :: Prelude.Text,
    -- | The user name from the identity provider for the user.
    username :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'disassociateUser_domain' - The domain name of the user.
--
-- 'identityProvider', 'disassociateUser_identityProvider' - An object that specifies details for the identity provider.
--
-- 'instanceId', 'disassociateUser_instanceId' - The ID of the EC2 instance, which provides user-based subscriptions.
--
-- 'username', 'disassociateUser_username' - The user name from the identity provider for the user.
newDisassociateUser ::
  -- | 'identityProvider'
  IdentityProvider ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  DisassociateUser
newDisassociateUser
  pIdentityProvider_
  pInstanceId_
  pUsername_ =
    DisassociateUser'
      { domain = Prelude.Nothing,
        identityProvider = pIdentityProvider_,
        instanceId = pInstanceId_,
        username = pUsername_
      }

-- | The domain name of the user.
disassociateUser_domain :: Lens.Lens' DisassociateUser (Prelude.Maybe Prelude.Text)
disassociateUser_domain = Lens.lens (\DisassociateUser' {domain} -> domain) (\s@DisassociateUser' {} a -> s {domain = a} :: DisassociateUser)

-- | An object that specifies details for the identity provider.
disassociateUser_identityProvider :: Lens.Lens' DisassociateUser IdentityProvider
disassociateUser_identityProvider = Lens.lens (\DisassociateUser' {identityProvider} -> identityProvider) (\s@DisassociateUser' {} a -> s {identityProvider = a} :: DisassociateUser)

-- | The ID of the EC2 instance, which provides user-based subscriptions.
disassociateUser_instanceId :: Lens.Lens' DisassociateUser Prelude.Text
disassociateUser_instanceId = Lens.lens (\DisassociateUser' {instanceId} -> instanceId) (\s@DisassociateUser' {} a -> s {instanceId = a} :: DisassociateUser)

-- | The user name from the identity provider for the user.
disassociateUser_username :: Lens.Lens' DisassociateUser Prelude.Text
disassociateUser_username = Lens.lens (\DisassociateUser' {username} -> username) (\s@DisassociateUser' {} a -> s {username = a} :: DisassociateUser)

instance Core.AWSRequest DisassociateUser where
  type
    AWSResponse DisassociateUser =
      DisassociateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "InstanceUserSummary")
      )

instance Prelude.Hashable DisassociateUser where
  hashWithSalt _salt DisassociateUser' {..} =
    _salt
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` identityProvider
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` username

instance Prelude.NFData DisassociateUser where
  rnf DisassociateUser' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf username

instance Data.ToHeaders DisassociateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateUser where
  toJSON DisassociateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Domain" Data..=) Prelude.<$> domain,
            Prelude.Just
              ("IdentityProvider" Data..= identityProvider),
            Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("Username" Data..= username)
          ]
      )

instance Data.ToPath DisassociateUser where
  toPath = Prelude.const "/user/DisassociateUser"

instance Data.ToQuery DisassociateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateUserResponse' smart constructor.
data DisassociateUserResponse = DisassociateUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Metadata that describes the associate user operation.
    instanceUserSummary :: InstanceUserSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateUserResponse_httpStatus' - The response's http status code.
--
-- 'instanceUserSummary', 'disassociateUserResponse_instanceUserSummary' - Metadata that describes the associate user operation.
newDisassociateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'instanceUserSummary'
  InstanceUserSummary ->
  DisassociateUserResponse
newDisassociateUserResponse
  pHttpStatus_
  pInstanceUserSummary_ =
    DisassociateUserResponse'
      { httpStatus =
          pHttpStatus_,
        instanceUserSummary = pInstanceUserSummary_
      }

-- | The response's http status code.
disassociateUserResponse_httpStatus :: Lens.Lens' DisassociateUserResponse Prelude.Int
disassociateUserResponse_httpStatus = Lens.lens (\DisassociateUserResponse' {httpStatus} -> httpStatus) (\s@DisassociateUserResponse' {} a -> s {httpStatus = a} :: DisassociateUserResponse)

-- | Metadata that describes the associate user operation.
disassociateUserResponse_instanceUserSummary :: Lens.Lens' DisassociateUserResponse InstanceUserSummary
disassociateUserResponse_instanceUserSummary = Lens.lens (\DisassociateUserResponse' {instanceUserSummary} -> instanceUserSummary) (\s@DisassociateUserResponse' {} a -> s {instanceUserSummary = a} :: DisassociateUserResponse)

instance Prelude.NFData DisassociateUserResponse where
  rnf DisassociateUserResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf instanceUserSummary
