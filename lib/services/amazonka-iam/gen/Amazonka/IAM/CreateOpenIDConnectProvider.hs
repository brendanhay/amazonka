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
-- Module      : Amazonka.IAM.CreateOpenIDConnectProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IAM entity to describe an identity provider (IdP) that
-- supports <http://openid.net/connect/ OpenID Connect (OIDC)>.
--
-- The OIDC provider that you create with this operation can be used as a
-- principal in a role\'s trust policy. Such a policy establishes a trust
-- relationship between Amazon Web Services and the OIDC provider.
--
-- If you are using an OIDC identity provider from Google, Facebook, or
-- Amazon Cognito, you don\'t need to create a separate IAM identity
-- provider. These OIDC identity providers are already built-in to Amazon
-- Web Services and are available for your use. Instead, you can move
-- directly to creating new roles using your identity provider. To learn
-- more, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-idp_oidc.html Creating a role for web identity or OpenID connect federation>
-- in the /IAM User Guide/.
--
-- When you create the IAM OIDC provider, you specify the following:
--
-- -   The URL of the OIDC identity provider (IdP) to trust
--
-- -   A list of client IDs (also known as audiences) that identify the
--     application or applications allowed to authenticate using the OIDC
--     provider
--
-- -   A list of thumbprints of one or more server certificates that the
--     IdP uses
--
-- You get all of this information from the OIDC IdP you want to use to
-- access Amazon Web Services.
--
-- Amazon Web Services secures communication with some OIDC identity
-- providers (IdPs) through our library of trusted certificate authorities
-- (CAs) instead of using a certificate thumbprint to verify your IdP
-- server certificate. These OIDC IdPs include Google, and those that use
-- an Amazon S3 bucket to host a JSON Web Key Set (JWKS) endpoint. In these
-- cases, your legacy thumbprint remains in your configuration, but is no
-- longer used for validation.
--
-- The trust for the OIDC provider is derived from the IAM provider that
-- this operation creates. Therefore, it is best to limit access to the
-- CreateOpenIDConnectProvider operation to highly privileged users.
module Amazonka.IAM.CreateOpenIDConnectProvider
  ( -- * Creating a Request
    CreateOpenIDConnectProvider (..),
    newCreateOpenIDConnectProvider,

    -- * Request Lenses
    createOpenIDConnectProvider_clientIDList,
    createOpenIDConnectProvider_tags,
    createOpenIDConnectProvider_url,
    createOpenIDConnectProvider_thumbprintList,

    -- * Destructuring the Response
    CreateOpenIDConnectProviderResponse (..),
    newCreateOpenIDConnectProviderResponse,

    -- * Response Lenses
    createOpenIDConnectProviderResponse_openIDConnectProviderArn,
    createOpenIDConnectProviderResponse_tags,
    createOpenIDConnectProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateOpenIDConnectProvider' smart constructor.
data CreateOpenIDConnectProvider = CreateOpenIDConnectProvider'
  { -- | Provides a list of client IDs, also known as audiences. When a mobile or
    -- web app registers with an OpenID Connect provider, they establish a
    -- value that identifies the application. This is the value that\'s sent as
    -- the @client_id@ parameter on OAuth requests.
    --
    -- You can register multiple client IDs with the same provider. For
    -- example, you might have multiple applications that use the same OIDC
    -- provider. You cannot register more than 100 client IDs with a single IAM
    -- OIDC provider.
    --
    -- There is no defined format for a client ID. The
    -- @CreateOpenIDConnectProviderRequest@ operation accepts client IDs up to
    -- 255 characters long.
    clientIDList :: Prelude.Maybe [Prelude.Text],
    -- | A list of tags that you want to attach to the new IAM OpenID Connect
    -- (OIDC) provider. Each tag consists of a key name and an associated
    -- value. For more information about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    --
    -- If any one of the tags is invalid or if you exceed the allowed maximum
    -- number of tags, then the entire request fails and the resource is not
    -- created.
    tags :: Prelude.Maybe [Tag],
    -- | The URL of the identity provider. The URL must begin with @https:\/\/@
    -- and should correspond to the @iss@ claim in the provider\'s OpenID
    -- Connect ID tokens. Per the OIDC standard, path components are allowed
    -- but query parameters are not. Typically the URL consists of only a
    -- hostname, like @https:\/\/server.example.org@ or
    -- @https:\/\/example.com@. The URL should not contain a port number.
    --
    -- You cannot register the same provider multiple times in a single Amazon
    -- Web Services account. If you try to submit a URL that has already been
    -- used for an OpenID Connect provider in the Amazon Web Services account,
    -- you will get an error.
    url :: Prelude.Text,
    -- | A list of server certificate thumbprints for the OpenID Connect (OIDC)
    -- identity provider\'s server certificates. Typically this list includes
    -- only one entry. However, IAM lets you have up to five thumbprints for an
    -- OIDC provider. This lets you maintain multiple thumbprints if the
    -- identity provider is rotating certificates.
    --
    -- The server certificate thumbprint is the hex-encoded SHA-1 hash value of
    -- the X.509 certificate used by the domain where the OpenID Connect
    -- provider makes its keys available. It is always a 40-character string.
    --
    -- You must provide at least one thumbprint when creating an IAM OIDC
    -- provider. For example, assume that the OIDC provider is
    -- @server.example.com@ and the provider stores its keys at
    -- https:\/\/keys.server.example.com\/openid-connect. In that case, the
    -- thumbprint string would be the hex-encoded SHA-1 hash value of the
    -- certificate used by @https:\/\/keys.server.example.com.@
    --
    -- For more information about obtaining the OIDC provider thumbprint, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/identity-providers-oidc-obtain-thumbprint.html Obtaining the thumbprint for an OpenID Connect provider>
    -- in the /IAM User Guide/.
    thumbprintList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOpenIDConnectProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientIDList', 'createOpenIDConnectProvider_clientIDList' - Provides a list of client IDs, also known as audiences. When a mobile or
-- web app registers with an OpenID Connect provider, they establish a
-- value that identifies the application. This is the value that\'s sent as
-- the @client_id@ parameter on OAuth requests.
--
-- You can register multiple client IDs with the same provider. For
-- example, you might have multiple applications that use the same OIDC
-- provider. You cannot register more than 100 client IDs with a single IAM
-- OIDC provider.
--
-- There is no defined format for a client ID. The
-- @CreateOpenIDConnectProviderRequest@ operation accepts client IDs up to
-- 255 characters long.
--
-- 'tags', 'createOpenIDConnectProvider_tags' - A list of tags that you want to attach to the new IAM OpenID Connect
-- (OIDC) provider. Each tag consists of a key name and an associated
-- value. For more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
--
-- 'url', 'createOpenIDConnectProvider_url' - The URL of the identity provider. The URL must begin with @https:\/\/@
-- and should correspond to the @iss@ claim in the provider\'s OpenID
-- Connect ID tokens. Per the OIDC standard, path components are allowed
-- but query parameters are not. Typically the URL consists of only a
-- hostname, like @https:\/\/server.example.org@ or
-- @https:\/\/example.com@. The URL should not contain a port number.
--
-- You cannot register the same provider multiple times in a single Amazon
-- Web Services account. If you try to submit a URL that has already been
-- used for an OpenID Connect provider in the Amazon Web Services account,
-- you will get an error.
--
-- 'thumbprintList', 'createOpenIDConnectProvider_thumbprintList' - A list of server certificate thumbprints for the OpenID Connect (OIDC)
-- identity provider\'s server certificates. Typically this list includes
-- only one entry. However, IAM lets you have up to five thumbprints for an
-- OIDC provider. This lets you maintain multiple thumbprints if the
-- identity provider is rotating certificates.
--
-- The server certificate thumbprint is the hex-encoded SHA-1 hash value of
-- the X.509 certificate used by the domain where the OpenID Connect
-- provider makes its keys available. It is always a 40-character string.
--
-- You must provide at least one thumbprint when creating an IAM OIDC
-- provider. For example, assume that the OIDC provider is
-- @server.example.com@ and the provider stores its keys at
-- https:\/\/keys.server.example.com\/openid-connect. In that case, the
-- thumbprint string would be the hex-encoded SHA-1 hash value of the
-- certificate used by @https:\/\/keys.server.example.com.@
--
-- For more information about obtaining the OIDC provider thumbprint, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/identity-providers-oidc-obtain-thumbprint.html Obtaining the thumbprint for an OpenID Connect provider>
-- in the /IAM User Guide/.
newCreateOpenIDConnectProvider ::
  -- | 'url'
  Prelude.Text ->
  CreateOpenIDConnectProvider
newCreateOpenIDConnectProvider pUrl_ =
  CreateOpenIDConnectProvider'
    { clientIDList =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      url = pUrl_,
      thumbprintList = Prelude.mempty
    }

-- | Provides a list of client IDs, also known as audiences. When a mobile or
-- web app registers with an OpenID Connect provider, they establish a
-- value that identifies the application. This is the value that\'s sent as
-- the @client_id@ parameter on OAuth requests.
--
-- You can register multiple client IDs with the same provider. For
-- example, you might have multiple applications that use the same OIDC
-- provider. You cannot register more than 100 client IDs with a single IAM
-- OIDC provider.
--
-- There is no defined format for a client ID. The
-- @CreateOpenIDConnectProviderRequest@ operation accepts client IDs up to
-- 255 characters long.
createOpenIDConnectProvider_clientIDList :: Lens.Lens' CreateOpenIDConnectProvider (Prelude.Maybe [Prelude.Text])
createOpenIDConnectProvider_clientIDList = Lens.lens (\CreateOpenIDConnectProvider' {clientIDList} -> clientIDList) (\s@CreateOpenIDConnectProvider' {} a -> s {clientIDList = a} :: CreateOpenIDConnectProvider) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags that you want to attach to the new IAM OpenID Connect
-- (OIDC) provider. Each tag consists of a key name and an associated
-- value. For more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
createOpenIDConnectProvider_tags :: Lens.Lens' CreateOpenIDConnectProvider (Prelude.Maybe [Tag])
createOpenIDConnectProvider_tags = Lens.lens (\CreateOpenIDConnectProvider' {tags} -> tags) (\s@CreateOpenIDConnectProvider' {} a -> s {tags = a} :: CreateOpenIDConnectProvider) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the identity provider. The URL must begin with @https:\/\/@
-- and should correspond to the @iss@ claim in the provider\'s OpenID
-- Connect ID tokens. Per the OIDC standard, path components are allowed
-- but query parameters are not. Typically the URL consists of only a
-- hostname, like @https:\/\/server.example.org@ or
-- @https:\/\/example.com@. The URL should not contain a port number.
--
-- You cannot register the same provider multiple times in a single Amazon
-- Web Services account. If you try to submit a URL that has already been
-- used for an OpenID Connect provider in the Amazon Web Services account,
-- you will get an error.
createOpenIDConnectProvider_url :: Lens.Lens' CreateOpenIDConnectProvider Prelude.Text
createOpenIDConnectProvider_url = Lens.lens (\CreateOpenIDConnectProvider' {url} -> url) (\s@CreateOpenIDConnectProvider' {} a -> s {url = a} :: CreateOpenIDConnectProvider)

-- | A list of server certificate thumbprints for the OpenID Connect (OIDC)
-- identity provider\'s server certificates. Typically this list includes
-- only one entry. However, IAM lets you have up to five thumbprints for an
-- OIDC provider. This lets you maintain multiple thumbprints if the
-- identity provider is rotating certificates.
--
-- The server certificate thumbprint is the hex-encoded SHA-1 hash value of
-- the X.509 certificate used by the domain where the OpenID Connect
-- provider makes its keys available. It is always a 40-character string.
--
-- You must provide at least one thumbprint when creating an IAM OIDC
-- provider. For example, assume that the OIDC provider is
-- @server.example.com@ and the provider stores its keys at
-- https:\/\/keys.server.example.com\/openid-connect. In that case, the
-- thumbprint string would be the hex-encoded SHA-1 hash value of the
-- certificate used by @https:\/\/keys.server.example.com.@
--
-- For more information about obtaining the OIDC provider thumbprint, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/identity-providers-oidc-obtain-thumbprint.html Obtaining the thumbprint for an OpenID Connect provider>
-- in the /IAM User Guide/.
createOpenIDConnectProvider_thumbprintList :: Lens.Lens' CreateOpenIDConnectProvider [Prelude.Text]
createOpenIDConnectProvider_thumbprintList = Lens.lens (\CreateOpenIDConnectProvider' {thumbprintList} -> thumbprintList) (\s@CreateOpenIDConnectProvider' {} a -> s {thumbprintList = a} :: CreateOpenIDConnectProvider) Prelude.. Lens.coerced

instance Core.AWSRequest CreateOpenIDConnectProvider where
  type
    AWSResponse CreateOpenIDConnectProvider =
      CreateOpenIDConnectProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateOpenIDConnectProviderResult"
      ( \s h x ->
          CreateOpenIDConnectProviderResponse'
            Prelude.<$> (x Data..@? "OpenIDConnectProviderArn")
            Prelude.<*> ( x Data..@? "Tags" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOpenIDConnectProvider where
  hashWithSalt _salt CreateOpenIDConnectProvider' {..} =
    _salt `Prelude.hashWithSalt` clientIDList
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` thumbprintList

instance Prelude.NFData CreateOpenIDConnectProvider where
  rnf CreateOpenIDConnectProvider' {..} =
    Prelude.rnf clientIDList
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf thumbprintList

instance Data.ToHeaders CreateOpenIDConnectProvider where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateOpenIDConnectProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateOpenIDConnectProvider where
  toQuery CreateOpenIDConnectProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateOpenIDConnectProvider" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "ClientIDList"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> clientIDList),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "Url" Data.=: url,
        "ThumbprintList"
          Data.=: Data.toQueryList "member" thumbprintList
      ]

-- | Contains the response to a successful CreateOpenIDConnectProvider
-- request.
--
-- /See:/ 'newCreateOpenIDConnectProviderResponse' smart constructor.
data CreateOpenIDConnectProviderResponse = CreateOpenIDConnectProviderResponse'
  { -- | The Amazon Resource Name (ARN) of the new IAM OpenID Connect provider
    -- that is created. For more information, see
    -- OpenIDConnectProviderListEntry.
    openIDConnectProviderArn :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that are attached to the new IAM OIDC provider. The
    -- returned list of tags is sorted by tag key. For more information about
    -- tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOpenIDConnectProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectProviderArn', 'createOpenIDConnectProviderResponse_openIDConnectProviderArn' - The Amazon Resource Name (ARN) of the new IAM OpenID Connect provider
-- that is created. For more information, see
-- OpenIDConnectProviderListEntry.
--
-- 'tags', 'createOpenIDConnectProviderResponse_tags' - A list of tags that are attached to the new IAM OIDC provider. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'httpStatus', 'createOpenIDConnectProviderResponse_httpStatus' - The response's http status code.
newCreateOpenIDConnectProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOpenIDConnectProviderResponse
newCreateOpenIDConnectProviderResponse pHttpStatus_ =
  CreateOpenIDConnectProviderResponse'
    { openIDConnectProviderArn =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the new IAM OpenID Connect provider
-- that is created. For more information, see
-- OpenIDConnectProviderListEntry.
createOpenIDConnectProviderResponse_openIDConnectProviderArn :: Lens.Lens' CreateOpenIDConnectProviderResponse (Prelude.Maybe Prelude.Text)
createOpenIDConnectProviderResponse_openIDConnectProviderArn = Lens.lens (\CreateOpenIDConnectProviderResponse' {openIDConnectProviderArn} -> openIDConnectProviderArn) (\s@CreateOpenIDConnectProviderResponse' {} a -> s {openIDConnectProviderArn = a} :: CreateOpenIDConnectProviderResponse)

-- | A list of tags that are attached to the new IAM OIDC provider. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
createOpenIDConnectProviderResponse_tags :: Lens.Lens' CreateOpenIDConnectProviderResponse (Prelude.Maybe [Tag])
createOpenIDConnectProviderResponse_tags = Lens.lens (\CreateOpenIDConnectProviderResponse' {tags} -> tags) (\s@CreateOpenIDConnectProviderResponse' {} a -> s {tags = a} :: CreateOpenIDConnectProviderResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createOpenIDConnectProviderResponse_httpStatus :: Lens.Lens' CreateOpenIDConnectProviderResponse Prelude.Int
createOpenIDConnectProviderResponse_httpStatus = Lens.lens (\CreateOpenIDConnectProviderResponse' {httpStatus} -> httpStatus) (\s@CreateOpenIDConnectProviderResponse' {} a -> s {httpStatus = a} :: CreateOpenIDConnectProviderResponse)

instance
  Prelude.NFData
    CreateOpenIDConnectProviderResponse
  where
  rnf CreateOpenIDConnectProviderResponse' {..} =
    Prelude.rnf openIDConnectProviderArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
