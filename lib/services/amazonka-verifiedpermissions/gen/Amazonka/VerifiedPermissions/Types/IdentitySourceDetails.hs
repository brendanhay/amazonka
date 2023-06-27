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
-- Module      : Amazonka.VerifiedPermissions.Types.IdentitySourceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.IdentitySourceDetails where

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
-- /See:/ 'newIdentitySourceDetails' smart constructor.
data IdentitySourceDetails = IdentitySourceDetails'
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
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the Amazon Cognito user pool whose identities are accessible to this
    -- Verified Permissions policy store.
    userPoolArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentitySourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientIds', 'identitySourceDetails_clientIds' - The application client IDs associated with the specified Amazon Cognito
-- user pool that are enabled for this identity source.
--
-- 'discoveryUrl', 'identitySourceDetails_discoveryUrl' - The well-known URL that points to this user pool\'s OIDC discovery
-- endpoint. This is a URL string in the following format. This URL
-- replaces the placeholders for both the Amazon Web Services Region and
-- the user pool identifier with those appropriate for this user pool.
--
-- @https:\/\/cognito-idp.@/@\<region>@/@.amazonaws.com\/@/@\<user-pool-id>@/@\/.well-known\/openid-configuration@
--
-- 'openIdIssuer', 'identitySourceDetails_openIdIssuer' - A string that identifies the type of OIDC service represented by this
-- identity source.
--
-- At this time, the only valid value is @cognito@.
--
-- 'userPoolArn', 'identitySourceDetails_userPoolArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the Amazon Cognito user pool whose identities are accessible to this
-- Verified Permissions policy store.
newIdentitySourceDetails ::
  IdentitySourceDetails
newIdentitySourceDetails =
  IdentitySourceDetails'
    { clientIds = Prelude.Nothing,
      discoveryUrl = Prelude.Nothing,
      openIdIssuer = Prelude.Nothing,
      userPoolArn = Prelude.Nothing
    }

-- | The application client IDs associated with the specified Amazon Cognito
-- user pool that are enabled for this identity source.
identitySourceDetails_clientIds :: Lens.Lens' IdentitySourceDetails (Prelude.Maybe [Prelude.Text])
identitySourceDetails_clientIds = Lens.lens (\IdentitySourceDetails' {clientIds} -> clientIds) (\s@IdentitySourceDetails' {} a -> s {clientIds = a} :: IdentitySourceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The well-known URL that points to this user pool\'s OIDC discovery
-- endpoint. This is a URL string in the following format. This URL
-- replaces the placeholders for both the Amazon Web Services Region and
-- the user pool identifier with those appropriate for this user pool.
--
-- @https:\/\/cognito-idp.@/@\<region>@/@.amazonaws.com\/@/@\<user-pool-id>@/@\/.well-known\/openid-configuration@
identitySourceDetails_discoveryUrl :: Lens.Lens' IdentitySourceDetails (Prelude.Maybe Prelude.Text)
identitySourceDetails_discoveryUrl = Lens.lens (\IdentitySourceDetails' {discoveryUrl} -> discoveryUrl) (\s@IdentitySourceDetails' {} a -> s {discoveryUrl = a} :: IdentitySourceDetails)

-- | A string that identifies the type of OIDC service represented by this
-- identity source.
--
-- At this time, the only valid value is @cognito@.
identitySourceDetails_openIdIssuer :: Lens.Lens' IdentitySourceDetails (Prelude.Maybe OpenIdIssuer)
identitySourceDetails_openIdIssuer = Lens.lens (\IdentitySourceDetails' {openIdIssuer} -> openIdIssuer) (\s@IdentitySourceDetails' {} a -> s {openIdIssuer = a} :: IdentitySourceDetails)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the Amazon Cognito user pool whose identities are accessible to this
-- Verified Permissions policy store.
identitySourceDetails_userPoolArn :: Lens.Lens' IdentitySourceDetails (Prelude.Maybe Prelude.Text)
identitySourceDetails_userPoolArn = Lens.lens (\IdentitySourceDetails' {userPoolArn} -> userPoolArn) (\s@IdentitySourceDetails' {} a -> s {userPoolArn = a} :: IdentitySourceDetails)

instance Data.FromJSON IdentitySourceDetails where
  parseJSON =
    Data.withObject
      "IdentitySourceDetails"
      ( \x ->
          IdentitySourceDetails'
            Prelude.<$> (x Data..:? "clientIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "discoveryUrl")
            Prelude.<*> (x Data..:? "openIdIssuer")
            Prelude.<*> (x Data..:? "userPoolArn")
      )

instance Prelude.Hashable IdentitySourceDetails where
  hashWithSalt _salt IdentitySourceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` clientIds
      `Prelude.hashWithSalt` discoveryUrl
      `Prelude.hashWithSalt` openIdIssuer
      `Prelude.hashWithSalt` userPoolArn

instance Prelude.NFData IdentitySourceDetails where
  rnf IdentitySourceDetails' {..} =
    Prelude.rnf clientIds
      `Prelude.seq` Prelude.rnf discoveryUrl
      `Prelude.seq` Prelude.rnf openIdIssuer
      `Prelude.seq` Prelude.rnf userPoolArn
