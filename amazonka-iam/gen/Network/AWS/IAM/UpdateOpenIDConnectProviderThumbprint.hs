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
-- Module      : Network.AWS.IAM.UpdateOpenIDConnectProviderThumbprint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- provider\'s certificate changes, which occurs rarely. However, if the
-- provider\'s certificate /does/ change, any attempt to assume an IAM role
-- that specifies the OIDC provider as a principal fails until the
-- certificate thumbprint is updated.
--
-- Trust for the OIDC provider is derived from the provider\'s certificate
-- and is validated by the thumbprint. Therefore, it is best to limit
-- access to the @UpdateOpenIDConnectProviderThumbprint@ operation to
-- highly privileged users.
module Network.AWS.IAM.UpdateOpenIDConnectProviderThumbprint
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

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateOpenIDConnectProviderThumbprint' smart constructor.
data UpdateOpenIDConnectProviderThumbprint = UpdateOpenIDConnectProviderThumbprint'
  { -- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource object
    -- for which you want to update the thumbprint. You can get a list of OIDC
    -- provider ARNs by using the ListOpenIDConnectProviders operation.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    openIDConnectProviderArn :: Core.Text,
    -- | A list of certificate thumbprints that are associated with the specified
    -- IAM OpenID Connect provider. For more information, see
    -- CreateOpenIDConnectProvider.
    thumbprintList :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- in the /AWS General Reference/.
--
-- 'thumbprintList', 'updateOpenIDConnectProviderThumbprint_thumbprintList' - A list of certificate thumbprints that are associated with the specified
-- IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
newUpdateOpenIDConnectProviderThumbprint ::
  -- | 'openIDConnectProviderArn'
  Core.Text ->
  UpdateOpenIDConnectProviderThumbprint
newUpdateOpenIDConnectProviderThumbprint
  pOpenIDConnectProviderArn_ =
    UpdateOpenIDConnectProviderThumbprint'
      { openIDConnectProviderArn =
          pOpenIDConnectProviderArn_,
        thumbprintList = Core.mempty
      }

-- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource object
-- for which you want to update the thumbprint. You can get a list of OIDC
-- provider ARNs by using the ListOpenIDConnectProviders operation.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
updateOpenIDConnectProviderThumbprint_openIDConnectProviderArn :: Lens.Lens' UpdateOpenIDConnectProviderThumbprint Core.Text
updateOpenIDConnectProviderThumbprint_openIDConnectProviderArn = Lens.lens (\UpdateOpenIDConnectProviderThumbprint' {openIDConnectProviderArn} -> openIDConnectProviderArn) (\s@UpdateOpenIDConnectProviderThumbprint' {} a -> s {openIDConnectProviderArn = a} :: UpdateOpenIDConnectProviderThumbprint)

-- | A list of certificate thumbprints that are associated with the specified
-- IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
updateOpenIDConnectProviderThumbprint_thumbprintList :: Lens.Lens' UpdateOpenIDConnectProviderThumbprint [Core.Text]
updateOpenIDConnectProviderThumbprint_thumbprintList = Lens.lens (\UpdateOpenIDConnectProviderThumbprint' {thumbprintList} -> thumbprintList) (\s@UpdateOpenIDConnectProviderThumbprint' {} a -> s {thumbprintList = a} :: UpdateOpenIDConnectProviderThumbprint) Core.. Lens._Coerce

instance
  Core.AWSRequest
    UpdateOpenIDConnectProviderThumbprint
  where
  type
    AWSResponse
      UpdateOpenIDConnectProviderThumbprint =
      UpdateOpenIDConnectProviderThumbprintResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UpdateOpenIDConnectProviderThumbprintResponse'

instance
  Core.Hashable
    UpdateOpenIDConnectProviderThumbprint

instance
  Core.NFData
    UpdateOpenIDConnectProviderThumbprint

instance
  Core.ToHeaders
    UpdateOpenIDConnectProviderThumbprint
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    UpdateOpenIDConnectProviderThumbprint
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    UpdateOpenIDConnectProviderThumbprint
  where
  toQuery UpdateOpenIDConnectProviderThumbprint' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "UpdateOpenIDConnectProviderThumbprint" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "OpenIDConnectProviderArn"
          Core.=: openIDConnectProviderArn,
        "ThumbprintList"
          Core.=: Core.toQueryList "member" thumbprintList
      ]

-- | /See:/ 'newUpdateOpenIDConnectProviderThumbprintResponse' smart constructor.
data UpdateOpenIDConnectProviderThumbprintResponse = UpdateOpenIDConnectProviderThumbprintResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateOpenIDConnectProviderThumbprintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateOpenIDConnectProviderThumbprintResponse ::
  UpdateOpenIDConnectProviderThumbprintResponse
newUpdateOpenIDConnectProviderThumbprintResponse =
  UpdateOpenIDConnectProviderThumbprintResponse'

instance
  Core.NFData
    UpdateOpenIDConnectProviderThumbprintResponse
