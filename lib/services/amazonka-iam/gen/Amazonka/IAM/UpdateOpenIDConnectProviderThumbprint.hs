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
-- Module      : Amazonka.IAM.UpdateOpenIDConnectProviderThumbprint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the existing list of server certificate thumbprints associated
-- with an OpenID Connect (OIDC) provider resource object with a new list
-- of thumbprints.
--
-- The list that you pass with this operation completely replaces the
-- existing list of thumbprints. (The lists are not merged.)
--
-- Typically, you need to update a thumbprint only when the identity
-- provider certificate changes, which occurs rarely. However, if the
-- provider\'s certificate /does/ change, any attempt to assume an IAM role
-- that specifies the OIDC provider as a principal fails until the
-- certificate thumbprint is updated.
--
-- Amazon Web Services secures communication with some OIDC identity
-- providers (IdPs) through our library of trusted certificate authorities
-- (CAs) instead of using a certificate thumbprint to verify your IdP
-- server certificate. These OIDC IdPs include Google, and those that use
-- an Amazon S3 bucket to host a JSON Web Key Set (JWKS) endpoint. In these
-- cases, your legacy thumbprint remains in your configuration, but is no
-- longer used for validation.
--
-- Trust for the OIDC provider is derived from the provider certificate and
-- is validated by the thumbprint. Therefore, it is best to limit access to
-- the @UpdateOpenIDConnectProviderThumbprint@ operation to highly
-- privileged users.
module Amazonka.IAM.UpdateOpenIDConnectProviderThumbprint
  ( -- * Creating a Request
    UpdateOpenIDConnectProviderThumbprint (..),
    newUpdateOpenIDConnectProviderThumbprint,

    -- * Request Lenses
    updateOpenIDConnectProviderThumbprint_openIDConnectProviderArn,
    updateOpenIDConnectProviderThumbprint_thumbprintList,

    -- * Destructuring the Response
    UpdateOpenIDConnectProviderThumbprintResponse (..),
    newUpdateOpenIDConnectProviderThumbprintResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateOpenIDConnectProviderThumbprint' smart constructor.
data UpdateOpenIDConnectProviderThumbprint = UpdateOpenIDConnectProviderThumbprint'
  { -- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource object
    -- for which you want to update the thumbprint. You can get a list of OIDC
    -- provider ARNs by using the ListOpenIDConnectProviders operation.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    openIDConnectProviderArn :: Prelude.Text,
    -- | A list of certificate thumbprints that are associated with the specified
    -- IAM OpenID Connect provider. For more information, see
    -- CreateOpenIDConnectProvider.
    thumbprintList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOpenIDConnectProviderThumbprint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectProviderArn', 'updateOpenIDConnectProviderThumbprint_openIDConnectProviderArn' - The Amazon Resource Name (ARN) of the IAM OIDC provider resource object
-- for which you want to update the thumbprint. You can get a list of OIDC
-- provider ARNs by using the ListOpenIDConnectProviders operation.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'thumbprintList', 'updateOpenIDConnectProviderThumbprint_thumbprintList' - A list of certificate thumbprints that are associated with the specified
-- IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
newUpdateOpenIDConnectProviderThumbprint ::
  -- | 'openIDConnectProviderArn'
  Prelude.Text ->
  UpdateOpenIDConnectProviderThumbprint
newUpdateOpenIDConnectProviderThumbprint
  pOpenIDConnectProviderArn_ =
    UpdateOpenIDConnectProviderThumbprint'
      { openIDConnectProviderArn =
          pOpenIDConnectProviderArn_,
        thumbprintList = Prelude.mempty
      }

-- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource object
-- for which you want to update the thumbprint. You can get a list of OIDC
-- provider ARNs by using the ListOpenIDConnectProviders operation.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
updateOpenIDConnectProviderThumbprint_openIDConnectProviderArn :: Lens.Lens' UpdateOpenIDConnectProviderThumbprint Prelude.Text
updateOpenIDConnectProviderThumbprint_openIDConnectProviderArn = Lens.lens (\UpdateOpenIDConnectProviderThumbprint' {openIDConnectProviderArn} -> openIDConnectProviderArn) (\s@UpdateOpenIDConnectProviderThumbprint' {} a -> s {openIDConnectProviderArn = a} :: UpdateOpenIDConnectProviderThumbprint)

-- | A list of certificate thumbprints that are associated with the specified
-- IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
updateOpenIDConnectProviderThumbprint_thumbprintList :: Lens.Lens' UpdateOpenIDConnectProviderThumbprint [Prelude.Text]
updateOpenIDConnectProviderThumbprint_thumbprintList = Lens.lens (\UpdateOpenIDConnectProviderThumbprint' {thumbprintList} -> thumbprintList) (\s@UpdateOpenIDConnectProviderThumbprint' {} a -> s {thumbprintList = a} :: UpdateOpenIDConnectProviderThumbprint) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    UpdateOpenIDConnectProviderThumbprint
  where
  type
    AWSResponse
      UpdateOpenIDConnectProviderThumbprint =
      UpdateOpenIDConnectProviderThumbprintResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      UpdateOpenIDConnectProviderThumbprintResponse'

instance
  Prelude.Hashable
    UpdateOpenIDConnectProviderThumbprint
  where
  hashWithSalt
    _salt
    UpdateOpenIDConnectProviderThumbprint' {..} =
      _salt
        `Prelude.hashWithSalt` openIDConnectProviderArn
        `Prelude.hashWithSalt` thumbprintList

instance
  Prelude.NFData
    UpdateOpenIDConnectProviderThumbprint
  where
  rnf UpdateOpenIDConnectProviderThumbprint' {..} =
    Prelude.rnf openIDConnectProviderArn
      `Prelude.seq` Prelude.rnf thumbprintList

instance
  Data.ToHeaders
    UpdateOpenIDConnectProviderThumbprint
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    UpdateOpenIDConnectProviderThumbprint
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateOpenIDConnectProviderThumbprint
  where
  toQuery UpdateOpenIDConnectProviderThumbprint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "UpdateOpenIDConnectProviderThumbprint" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "OpenIDConnectProviderArn"
          Data.=: openIDConnectProviderArn,
        "ThumbprintList"
          Data.=: Data.toQueryList "member" thumbprintList
      ]

-- | /See:/ 'newUpdateOpenIDConnectProviderThumbprintResponse' smart constructor.
data UpdateOpenIDConnectProviderThumbprintResponse = UpdateOpenIDConnectProviderThumbprintResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOpenIDConnectProviderThumbprintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateOpenIDConnectProviderThumbprintResponse ::
  UpdateOpenIDConnectProviderThumbprintResponse
newUpdateOpenIDConnectProviderThumbprintResponse =
  UpdateOpenIDConnectProviderThumbprintResponse'

instance
  Prelude.NFData
    UpdateOpenIDConnectProviderThumbprintResponse
  where
  rnf _ = ()
