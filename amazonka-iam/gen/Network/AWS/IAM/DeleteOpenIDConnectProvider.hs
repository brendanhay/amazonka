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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteOpenIDConnectProvider' smart constructor.
data DeleteOpenIDConnectProvider = DeleteOpenIDConnectProvider'
  { -- | The Amazon Resource Name (ARN) of the IAM OpenID Connect provider
    -- resource object to delete. You can get a list of OpenID Connect provider
    -- resource ARNs by using the ListOpenIDConnectProviders operation.
    openIDConnectProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
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
deleteOpenIDConnectProvider_openIDConnectProviderArn :: Lens.Lens' DeleteOpenIDConnectProvider Prelude.Text
deleteOpenIDConnectProvider_openIDConnectProviderArn = Lens.lens (\DeleteOpenIDConnectProvider' {openIDConnectProviderArn} -> openIDConnectProviderArn) (\s@DeleteOpenIDConnectProvider' {} a -> s {openIDConnectProviderArn = a} :: DeleteOpenIDConnectProvider)

instance
  Prelude.AWSRequest
    DeleteOpenIDConnectProvider
  where
  type
    Rs DeleteOpenIDConnectProvider =
      DeleteOpenIDConnectProviderResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteOpenIDConnectProviderResponse'

instance Prelude.Hashable DeleteOpenIDConnectProvider

instance Prelude.NFData DeleteOpenIDConnectProvider

instance
  Prelude.ToHeaders
    DeleteOpenIDConnectProvider
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteOpenIDConnectProvider where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteOpenIDConnectProvider where
  toQuery DeleteOpenIDConnectProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteOpenIDConnectProvider" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "OpenIDConnectProviderArn"
          Prelude.=: openIDConnectProviderArn
      ]

-- | /See:/ 'newDeleteOpenIDConnectProviderResponse' smart constructor.
data DeleteOpenIDConnectProviderResponse = DeleteOpenIDConnectProviderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteOpenIDConnectProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteOpenIDConnectProviderResponse ::
  DeleteOpenIDConnectProviderResponse
newDeleteOpenIDConnectProviderResponse =
  DeleteOpenIDConnectProviderResponse'

instance
  Prelude.NFData
    DeleteOpenIDConnectProviderResponse
