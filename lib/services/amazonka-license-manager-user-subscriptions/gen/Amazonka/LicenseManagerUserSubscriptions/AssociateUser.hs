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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.AssociateUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the user to an EC2 instance to utilize user-based
-- subscriptions.
--
-- Your estimated bill for charges on the number of users and related costs
-- will take 48 hours to appear for billing periods that haven\'t closed
-- (marked as __Pending__ billing status) in Amazon Web Services Billing.
-- For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/invoice.html Viewing your monthly charges>
-- in the /Amazon Web Services Billing User Guide/.
module Amazonka.LicenseManagerUserSubscriptions.AssociateUser
  ( -- * Creating a Request
    AssociateUser (..),
    newAssociateUser,

    -- * Request Lenses
    associateUser_domain,
    associateUser_identityProvider,
    associateUser_instanceId,
    associateUser_username,

    -- * Destructuring the Response
    AssociateUserResponse (..),
    newAssociateUserResponse,

    -- * Response Lenses
    associateUserResponse_httpStatus,
    associateUserResponse_instanceUserSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateUser' smart constructor.
data AssociateUser = AssociateUser'
  { -- | The domain name of the user.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The identity provider of the user.
    identityProvider :: IdentityProvider,
    -- | The ID of the EC2 instance, which provides user-based subscriptions.
    instanceId :: Prelude.Text,
    -- | The user name from the identity provider for the user.
    username :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'associateUser_domain' - The domain name of the user.
--
-- 'identityProvider', 'associateUser_identityProvider' - The identity provider of the user.
--
-- 'instanceId', 'associateUser_instanceId' - The ID of the EC2 instance, which provides user-based subscriptions.
--
-- 'username', 'associateUser_username' - The user name from the identity provider for the user.
newAssociateUser ::
  -- | 'identityProvider'
  IdentityProvider ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AssociateUser
newAssociateUser
  pIdentityProvider_
  pInstanceId_
  pUsername_ =
    AssociateUser'
      { domain = Prelude.Nothing,
        identityProvider = pIdentityProvider_,
        instanceId = pInstanceId_,
        username = pUsername_
      }

-- | The domain name of the user.
associateUser_domain :: Lens.Lens' AssociateUser (Prelude.Maybe Prelude.Text)
associateUser_domain = Lens.lens (\AssociateUser' {domain} -> domain) (\s@AssociateUser' {} a -> s {domain = a} :: AssociateUser)

-- | The identity provider of the user.
associateUser_identityProvider :: Lens.Lens' AssociateUser IdentityProvider
associateUser_identityProvider = Lens.lens (\AssociateUser' {identityProvider} -> identityProvider) (\s@AssociateUser' {} a -> s {identityProvider = a} :: AssociateUser)

-- | The ID of the EC2 instance, which provides user-based subscriptions.
associateUser_instanceId :: Lens.Lens' AssociateUser Prelude.Text
associateUser_instanceId = Lens.lens (\AssociateUser' {instanceId} -> instanceId) (\s@AssociateUser' {} a -> s {instanceId = a} :: AssociateUser)

-- | The user name from the identity provider for the user.
associateUser_username :: Lens.Lens' AssociateUser Prelude.Text
associateUser_username = Lens.lens (\AssociateUser' {username} -> username) (\s@AssociateUser' {} a -> s {username = a} :: AssociateUser)

instance Core.AWSRequest AssociateUser where
  type
    AWSResponse AssociateUser =
      AssociateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "InstanceUserSummary")
      )

instance Prelude.Hashable AssociateUser where
  hashWithSalt _salt AssociateUser' {..} =
    _salt
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` identityProvider
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` username

instance Prelude.NFData AssociateUser where
  rnf AssociateUser' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf username

instance Data.ToHeaders AssociateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateUser where
  toJSON AssociateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Domain" Data..=) Prelude.<$> domain,
            Prelude.Just
              ("IdentityProvider" Data..= identityProvider),
            Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("Username" Data..= username)
          ]
      )

instance Data.ToPath AssociateUser where
  toPath = Prelude.const "/user/AssociateUser"

instance Data.ToQuery AssociateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateUserResponse' smart constructor.
data AssociateUserResponse = AssociateUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Metadata that describes the associate user operation.
    instanceUserSummary :: InstanceUserSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateUserResponse_httpStatus' - The response's http status code.
--
-- 'instanceUserSummary', 'associateUserResponse_instanceUserSummary' - Metadata that describes the associate user operation.
newAssociateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'instanceUserSummary'
  InstanceUserSummary ->
  AssociateUserResponse
newAssociateUserResponse
  pHttpStatus_
  pInstanceUserSummary_ =
    AssociateUserResponse'
      { httpStatus = pHttpStatus_,
        instanceUserSummary = pInstanceUserSummary_
      }

-- | The response's http status code.
associateUserResponse_httpStatus :: Lens.Lens' AssociateUserResponse Prelude.Int
associateUserResponse_httpStatus = Lens.lens (\AssociateUserResponse' {httpStatus} -> httpStatus) (\s@AssociateUserResponse' {} a -> s {httpStatus = a} :: AssociateUserResponse)

-- | Metadata that describes the associate user operation.
associateUserResponse_instanceUserSummary :: Lens.Lens' AssociateUserResponse InstanceUserSummary
associateUserResponse_instanceUserSummary = Lens.lens (\AssociateUserResponse' {instanceUserSummary} -> instanceUserSummary) (\s@AssociateUserResponse' {} a -> s {instanceUserSummary = a} :: AssociateUserResponse)

instance Prelude.NFData AssociateUserResponse where
  rnf AssociateUserResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf instanceUserSummary
