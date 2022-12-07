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
-- Module      : Amazonka.IAM.RemoveClientIDFromOpenIDConnectProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified client ID (also known as audience) from the list
-- of client IDs registered for the specified IAM OpenID Connect (OIDC)
-- provider resource object.
--
-- This operation is idempotent; it does not fail or return an error if you
-- try to remove a client ID that does not exist.
module Amazonka.IAM.RemoveClientIDFromOpenIDConnectProvider
  ( -- * Creating a Request
    RemoveClientIDFromOpenIDConnectProvider (..),
    newRemoveClientIDFromOpenIDConnectProvider,

    -- * Request Lenses
    removeClientIDFromOpenIDConnectProvider_openIDConnectProviderArn,
    removeClientIDFromOpenIDConnectProvider_clientID,

    -- * Destructuring the Response
    RemoveClientIDFromOpenIDConnectProviderResponse (..),
    newRemoveClientIDFromOpenIDConnectProviderResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveClientIDFromOpenIDConnectProvider' smart constructor.
data RemoveClientIDFromOpenIDConnectProvider = RemoveClientIDFromOpenIDConnectProvider'
  { -- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource to
    -- remove the client ID from. You can get a list of OIDC provider ARNs by
    -- using the ListOpenIDConnectProviders operation.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    openIDConnectProviderArn :: Prelude.Text,
    -- | The client ID (also known as audience) to remove from the IAM OIDC
    -- provider resource. For more information about client IDs, see
    -- CreateOpenIDConnectProvider.
    clientID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveClientIDFromOpenIDConnectProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectProviderArn', 'removeClientIDFromOpenIDConnectProvider_openIDConnectProviderArn' - The Amazon Resource Name (ARN) of the IAM OIDC provider resource to
-- remove the client ID from. You can get a list of OIDC provider ARNs by
-- using the ListOpenIDConnectProviders operation.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'clientID', 'removeClientIDFromOpenIDConnectProvider_clientID' - The client ID (also known as audience) to remove from the IAM OIDC
-- provider resource. For more information about client IDs, see
-- CreateOpenIDConnectProvider.
newRemoveClientIDFromOpenIDConnectProvider ::
  -- | 'openIDConnectProviderArn'
  Prelude.Text ->
  -- | 'clientID'
  Prelude.Text ->
  RemoveClientIDFromOpenIDConnectProvider
newRemoveClientIDFromOpenIDConnectProvider
  pOpenIDConnectProviderArn_
  pClientID_ =
    RemoveClientIDFromOpenIDConnectProvider'
      { openIDConnectProviderArn =
          pOpenIDConnectProviderArn_,
        clientID = pClientID_
      }

-- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource to
-- remove the client ID from. You can get a list of OIDC provider ARNs by
-- using the ListOpenIDConnectProviders operation.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
removeClientIDFromOpenIDConnectProvider_openIDConnectProviderArn :: Lens.Lens' RemoveClientIDFromOpenIDConnectProvider Prelude.Text
removeClientIDFromOpenIDConnectProvider_openIDConnectProviderArn = Lens.lens (\RemoveClientIDFromOpenIDConnectProvider' {openIDConnectProviderArn} -> openIDConnectProviderArn) (\s@RemoveClientIDFromOpenIDConnectProvider' {} a -> s {openIDConnectProviderArn = a} :: RemoveClientIDFromOpenIDConnectProvider)

-- | The client ID (also known as audience) to remove from the IAM OIDC
-- provider resource. For more information about client IDs, see
-- CreateOpenIDConnectProvider.
removeClientIDFromOpenIDConnectProvider_clientID :: Lens.Lens' RemoveClientIDFromOpenIDConnectProvider Prelude.Text
removeClientIDFromOpenIDConnectProvider_clientID = Lens.lens (\RemoveClientIDFromOpenIDConnectProvider' {clientID} -> clientID) (\s@RemoveClientIDFromOpenIDConnectProvider' {} a -> s {clientID = a} :: RemoveClientIDFromOpenIDConnectProvider)

instance
  Core.AWSRequest
    RemoveClientIDFromOpenIDConnectProvider
  where
  type
    AWSResponse
      RemoveClientIDFromOpenIDConnectProvider =
      RemoveClientIDFromOpenIDConnectProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      RemoveClientIDFromOpenIDConnectProviderResponse'

instance
  Prelude.Hashable
    RemoveClientIDFromOpenIDConnectProvider
  where
  hashWithSalt
    _salt
    RemoveClientIDFromOpenIDConnectProvider' {..} =
      _salt
        `Prelude.hashWithSalt` openIDConnectProviderArn
        `Prelude.hashWithSalt` clientID

instance
  Prelude.NFData
    RemoveClientIDFromOpenIDConnectProvider
  where
  rnf RemoveClientIDFromOpenIDConnectProvider' {..} =
    Prelude.rnf openIDConnectProviderArn
      `Prelude.seq` Prelude.rnf clientID

instance
  Data.ToHeaders
    RemoveClientIDFromOpenIDConnectProvider
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    RemoveClientIDFromOpenIDConnectProvider
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    RemoveClientIDFromOpenIDConnectProvider
  where
  toQuery RemoveClientIDFromOpenIDConnectProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RemoveClientIDFromOpenIDConnectProvider" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "OpenIDConnectProviderArn"
          Data.=: openIDConnectProviderArn,
        "ClientID" Data.=: clientID
      ]

-- | /See:/ 'newRemoveClientIDFromOpenIDConnectProviderResponse' smart constructor.
data RemoveClientIDFromOpenIDConnectProviderResponse = RemoveClientIDFromOpenIDConnectProviderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveClientIDFromOpenIDConnectProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveClientIDFromOpenIDConnectProviderResponse ::
  RemoveClientIDFromOpenIDConnectProviderResponse
newRemoveClientIDFromOpenIDConnectProviderResponse =
  RemoveClientIDFromOpenIDConnectProviderResponse'

instance
  Prelude.NFData
    RemoveClientIDFromOpenIDConnectProviderResponse
  where
  rnf _ = ()
