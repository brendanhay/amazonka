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
-- Module      : Network.AWS.IAM.AddClientIDToOpenIDConnectProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new client ID (also known as audience) to the list of client IDs
-- already registered for the specified IAM OpenID Connect (OIDC) provider
-- resource.
--
-- This operation is idempotent; it does not fail or return an error if you
-- add an existing client ID to the provider.
module Network.AWS.IAM.AddClientIDToOpenIDConnectProvider
  ( -- * Creating a Request
    AddClientIDToOpenIDConnectProvider (..),
    newAddClientIDToOpenIDConnectProvider,

    -- * Request Lenses
    addClientIDToOpenIDConnectProvider_openIDConnectProviderArn,
    addClientIDToOpenIDConnectProvider_clientID,

    -- * Destructuring the Response
    AddClientIDToOpenIDConnectProviderResponse (..),
    newAddClientIDToOpenIDConnectProviderResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddClientIDToOpenIDConnectProvider' smart constructor.
data AddClientIDToOpenIDConnectProvider = AddClientIDToOpenIDConnectProvider'
  { -- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
    -- resource to add the client ID to. You can get a list of OIDC provider
    -- ARNs by using the ListOpenIDConnectProviders operation.
    openIDConnectProviderArn :: Prelude.Text,
    -- | The client ID (also known as audience) to add to the IAM OpenID Connect
    -- provider resource.
    clientID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddClientIDToOpenIDConnectProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectProviderArn', 'addClientIDToOpenIDConnectProvider_openIDConnectProviderArn' - The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
-- resource to add the client ID to. You can get a list of OIDC provider
-- ARNs by using the ListOpenIDConnectProviders operation.
--
-- 'clientID', 'addClientIDToOpenIDConnectProvider_clientID' - The client ID (also known as audience) to add to the IAM OpenID Connect
-- provider resource.
newAddClientIDToOpenIDConnectProvider ::
  -- | 'openIDConnectProviderArn'
  Prelude.Text ->
  -- | 'clientID'
  Prelude.Text ->
  AddClientIDToOpenIDConnectProvider
newAddClientIDToOpenIDConnectProvider
  pOpenIDConnectProviderArn_
  pClientID_ =
    AddClientIDToOpenIDConnectProvider'
      { openIDConnectProviderArn =
          pOpenIDConnectProviderArn_,
        clientID = pClientID_
      }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
-- resource to add the client ID to. You can get a list of OIDC provider
-- ARNs by using the ListOpenIDConnectProviders operation.
addClientIDToOpenIDConnectProvider_openIDConnectProviderArn :: Lens.Lens' AddClientIDToOpenIDConnectProvider Prelude.Text
addClientIDToOpenIDConnectProvider_openIDConnectProviderArn = Lens.lens (\AddClientIDToOpenIDConnectProvider' {openIDConnectProviderArn} -> openIDConnectProviderArn) (\s@AddClientIDToOpenIDConnectProvider' {} a -> s {openIDConnectProviderArn = a} :: AddClientIDToOpenIDConnectProvider)

-- | The client ID (also known as audience) to add to the IAM OpenID Connect
-- provider resource.
addClientIDToOpenIDConnectProvider_clientID :: Lens.Lens' AddClientIDToOpenIDConnectProvider Prelude.Text
addClientIDToOpenIDConnectProvider_clientID = Lens.lens (\AddClientIDToOpenIDConnectProvider' {clientID} -> clientID) (\s@AddClientIDToOpenIDConnectProvider' {} a -> s {clientID = a} :: AddClientIDToOpenIDConnectProvider)

instance
  Core.AWSRequest
    AddClientIDToOpenIDConnectProvider
  where
  type
    AWSResponse AddClientIDToOpenIDConnectProvider =
      AddClientIDToOpenIDConnectProviderResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      AddClientIDToOpenIDConnectProviderResponse'

instance
  Prelude.Hashable
    AddClientIDToOpenIDConnectProvider

instance
  Prelude.NFData
    AddClientIDToOpenIDConnectProvider

instance
  Core.ToHeaders
    AddClientIDToOpenIDConnectProvider
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    AddClientIDToOpenIDConnectProvider
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    AddClientIDToOpenIDConnectProvider
  where
  toQuery AddClientIDToOpenIDConnectProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "AddClientIDToOpenIDConnectProvider" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "OpenIDConnectProviderArn"
          Core.=: openIDConnectProviderArn,
        "ClientID" Core.=: clientID
      ]

-- | /See:/ 'newAddClientIDToOpenIDConnectProviderResponse' smart constructor.
data AddClientIDToOpenIDConnectProviderResponse = AddClientIDToOpenIDConnectProviderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddClientIDToOpenIDConnectProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddClientIDToOpenIDConnectProviderResponse ::
  AddClientIDToOpenIDConnectProviderResponse
newAddClientIDToOpenIDConnectProviderResponse =
  AddClientIDToOpenIDConnectProviderResponse'

instance
  Prelude.NFData
    AddClientIDToOpenIDConnectProviderResponse
