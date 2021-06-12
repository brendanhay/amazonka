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
-- Module      : Network.AWS.IAM.DeleteOpenIDConnectProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an OpenID Connect identity provider (IdP) resource object in
-- IAM.
--
-- Deleting an IAM OIDC provider resource does not update any roles that
-- reference the provider as a principal in their trust policies. Any
-- attempt to assume a role that references a deleted provider fails.
--
-- This operation is idempotent; it does not fail or return an error if you
-- call the operation for a provider that does not exist.
module Network.AWS.IAM.DeleteOpenIDConnectProvider
  ( -- * Creating a Request
    DeleteOpenIDConnectProvider (..),
    newDeleteOpenIDConnectProvider,

    -- * Request Lenses
    deleteOpenIDConnectProvider_openIDConnectProviderArn,

    -- * Destructuring the Response
    DeleteOpenIDConnectProviderResponse (..),
    newDeleteOpenIDConnectProviderResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteOpenIDConnectProvider' smart constructor.
data DeleteOpenIDConnectProvider = DeleteOpenIDConnectProvider'
  { -- | The Amazon Resource Name (ARN) of the IAM OpenID Connect provider
    -- resource object to delete. You can get a list of OpenID Connect provider
    -- resource ARNs by using the ListOpenIDConnectProviders operation.
    openIDConnectProviderArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteOpenIDConnectProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectProviderArn', 'deleteOpenIDConnectProvider_openIDConnectProviderArn' - The Amazon Resource Name (ARN) of the IAM OpenID Connect provider
-- resource object to delete. You can get a list of OpenID Connect provider
-- resource ARNs by using the ListOpenIDConnectProviders operation.
newDeleteOpenIDConnectProvider ::
  -- | 'openIDConnectProviderArn'
  Core.Text ->
  DeleteOpenIDConnectProvider
newDeleteOpenIDConnectProvider
  pOpenIDConnectProviderArn_ =
    DeleteOpenIDConnectProvider'
      { openIDConnectProviderArn =
          pOpenIDConnectProviderArn_
      }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect provider
-- resource object to delete. You can get a list of OpenID Connect provider
-- resource ARNs by using the ListOpenIDConnectProviders operation.
deleteOpenIDConnectProvider_openIDConnectProviderArn :: Lens.Lens' DeleteOpenIDConnectProvider Core.Text
deleteOpenIDConnectProvider_openIDConnectProviderArn = Lens.lens (\DeleteOpenIDConnectProvider' {openIDConnectProviderArn} -> openIDConnectProviderArn) (\s@DeleteOpenIDConnectProvider' {} a -> s {openIDConnectProviderArn = a} :: DeleteOpenIDConnectProvider)

instance Core.AWSRequest DeleteOpenIDConnectProvider where
  type
    AWSResponse DeleteOpenIDConnectProvider =
      DeleteOpenIDConnectProviderResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteOpenIDConnectProviderResponse'

instance Core.Hashable DeleteOpenIDConnectProvider

instance Core.NFData DeleteOpenIDConnectProvider

instance Core.ToHeaders DeleteOpenIDConnectProvider where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteOpenIDConnectProvider where
  toPath = Core.const "/"

instance Core.ToQuery DeleteOpenIDConnectProvider where
  toQuery DeleteOpenIDConnectProvider' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteOpenIDConnectProvider" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "OpenIDConnectProviderArn"
          Core.=: openIDConnectProviderArn
      ]

-- | /See:/ 'newDeleteOpenIDConnectProviderResponse' smart constructor.
data DeleteOpenIDConnectProviderResponse = DeleteOpenIDConnectProviderResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteOpenIDConnectProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteOpenIDConnectProviderResponse ::
  DeleteOpenIDConnectProviderResponse
newDeleteOpenIDConnectProviderResponse =
  DeleteOpenIDConnectProviderResponse'

instance
  Core.NFData
    DeleteOpenIDConnectProviderResponse
