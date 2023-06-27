{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VerifiedPermissions.Types.IdentitySourceItemDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.IdentitySourceItemDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.OpenIdIssuer

-- | A structure that contains configuration of the identity source.
--
-- This data type is used as a response parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_CreateIdentitySource.html CreateIdentitySource>
-- operation.
--
-- /See:/ 'newIdentitySourceItemDetails' smart constructor.
data IdentitySourceItemDetails = IdentitySourceItemDetails'
  { -- | The application client IDs associated with the specified Amazon Cognito
    -- user pool that are enabled for this identity source.
    clientIds :: Prelude.Maybe [Prelude.Text],
    -- | The well-known URL that points to this user pool\'s OIDC discovery
    -- endpoint. This is a URL string in the following format. This URL
    -- replaces the placeholders for both the Amazon Web Services Region and
    -- the user pool identifier with those appropriate for this user pool.
    --
    -- @https:\/\/cognito-idp.@/@\<region>@/@.amazonaws.com\/@/@\<user-pool-id>@/@\/.well-known\/openid-configuration@
    discoveryUrl :: Prelude.Maybe Prelude.Text,
    -- | A string that identifies the type of OIDC service represented by this
    -- identity source.
    --
    -- At this time, the only valid value is @cognito@.
    openIdIssuer :: Prelude.Maybe OpenIdIssuer,
    -- | The Amazon Cognito user pool whose identities are accessible to this
    -- Verified Permissions policy store.
    userPoolArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentitySourceItemDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientIds', 'identitySourceItemDetails_clientIds' - The application client IDs associated with the specified Amazon Cognito
-- user pool that are enabled for this identity source.
--
-- 'discoveryUrl', 'identitySourceItemDetails_discoveryUrl' - The well-known URL that points to this user pool\'s OIDC discovery
-- endpoint. This is a URL string in the following format. This URL
-- replaces the placeholders for both the Amazon Web Services Region and
-- the user pool identifier with those appropriate for this user pool.
--
-- @https:\/\/cognito-idp.@/@\<region>@/@.amazonaws.com\/@/@\<user-pool-id>@/@\/.well-known\/openid-configuration@
--
-- 'openIdIssuer', 'identitySourceItemDetails_openIdIssuer' - A string that identifies the type of OIDC service represented by this
-- identity source.
--
-- At this time, the only valid value is @cognito@.
--
-- 'userPoolArn', 'identitySourceItemDetails_userPoolArn' - The Amazon Cognito user pool whose identities are accessible to this
-- Verified Permissions policy store.
newIdentitySourceItemDetails ::
  IdentitySourceItemDetails
newIdentitySourceItemDetails =
  IdentitySourceItemDetails'
    { clientIds =
        Prelude.Nothing,
      discoveryUrl = Prelude.Nothing,
      openIdIssuer = Prelude.Nothing,
      userPoolArn = Prelude.Nothing
    }

-- | The application client IDs associated with the specified Amazon Cognito
-- user pool that are enabled for this identity source.
identitySourceItemDetails_clientIds :: Lens.Lens' IdentitySourceItemDetails (Prelude.Maybe [Prelude.Text])
identitySourceItemDetails_clientIds = Lens.lens (\IdentitySourceItemDetails' {clientIds} -> clientIds) (\s@IdentitySourceItemDetails' {} a -> s {clientIds = a} :: IdentitySourceItemDetails) Prelude.. Lens.mapping Lens.coerced

-- | The well-known URL that points to this user pool\'s OIDC discovery
-- endpoint. This is a URL string in the following format. This URL
-- replaces the placeholders for both the Amazon Web Services Region and
-- the user pool identifier with those appropriate for this user pool.
--
-- @https:\/\/cognito-idp.@/@\<region>@/@.amazonaws.com\/@/@\<user-pool-id>@/@\/.well-known\/openid-configuration@
identitySourceItemDetails_discoveryUrl :: Lens.Lens' IdentitySourceItemDetails (Prelude.Maybe Prelude.Text)
identitySourceItemDetails_discoveryUrl = Lens.lens (\IdentitySourceItemDetails' {discoveryUrl} -> discoveryUrl) (\s@IdentitySourceItemDetails' {} a -> s {discoveryUrl = a} :: IdentitySourceItemDetails)

-- | A string that identifies the type of OIDC service represented by this
-- identity source.
--
-- At this time, the only valid value is @cognito@.
identitySourceItemDetails_openIdIssuer :: Lens.Lens' IdentitySourceItemDetails (Prelude.Maybe OpenIdIssuer)
identitySourceItemDetails_openIdIssuer = Lens.lens (\IdentitySourceItemDetails' {openIdIssuer} -> openIdIssuer) (\s@IdentitySourceItemDetails' {} a -> s {openIdIssuer = a} :: IdentitySourceItemDetails)

-- | The Amazon Cognito user pool whose identities are accessible to this
-- Verified Permissions policy store.
identitySourceItemDetails_userPoolArn :: Lens.Lens' IdentitySourceItemDetails (Prelude.Maybe Prelude.Text)
identitySourceItemDetails_userPoolArn = Lens.lens (\IdentitySourceItemDetails' {userPoolArn} -> userPoolArn) (\s@IdentitySourceItemDetails' {} a -> s {userPoolArn = a} :: IdentitySourceItemDetails)

instance Data.FromJSON IdentitySourceItemDetails where
  parseJSON =
    Data.withObject
      "IdentitySourceItemDetails"
      ( \x ->
          IdentitySourceItemDetails'
            Prelude.<$> (x Data..:? "clientIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "discoveryUrl")
            Prelude.<*> (x Data..:? "openIdIssuer")
            Prelude.<*> (x Data..:? "userPoolArn")
      )

instance Prelude.Hashable IdentitySourceItemDetails where
  hashWithSalt _salt IdentitySourceItemDetails' {..} =
    _salt
      `Prelude.hashWithSalt` clientIds
      `Prelude.hashWithSalt` discoveryUrl
      `Prelude.hashWithSalt` openIdIssuer
      `Prelude.hashWithSalt` userPoolArn

instance Prelude.NFData IdentitySourceItemDetails where
  rnf IdentitySourceItemDetails' {..} =
    Prelude.rnf clientIds
      `Prelude.seq` Prelude.rnf discoveryUrl
      `Prelude.seq` Prelude.rnf openIdIssuer
      `Prelude.seq` Prelude.rnf userPoolArn
