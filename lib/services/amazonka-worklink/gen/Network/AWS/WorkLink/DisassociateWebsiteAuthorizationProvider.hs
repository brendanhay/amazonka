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
-- Module      : Network.AWS.WorkLink.DisassociateWebsiteAuthorizationProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a website authorization provider from a specified fleet.
-- After the disassociation, users can\'t load any associated websites that
-- require this authorization provider.
module Network.AWS.WorkLink.DisassociateWebsiteAuthorizationProvider
  ( -- * Creating a Request
    DisassociateWebsiteAuthorizationProvider (..),
    newDisassociateWebsiteAuthorizationProvider,

    -- * Request Lenses
    disassociateWebsiteAuthorizationProvider_fleetArn,
    disassociateWebsiteAuthorizationProvider_authorizationProviderId,

    -- * Destructuring the Response
    DisassociateWebsiteAuthorizationProviderResponse (..),
    newDisassociateWebsiteAuthorizationProviderResponse,

    -- * Response Lenses
    disassociateWebsiteAuthorizationProviderResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkLink.Types

-- | /See:/ 'newDisassociateWebsiteAuthorizationProvider' smart constructor.
data DisassociateWebsiteAuthorizationProvider = DisassociateWebsiteAuthorizationProvider'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | A unique identifier for the authorization provider.
    authorizationProviderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWebsiteAuthorizationProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'disassociateWebsiteAuthorizationProvider_fleetArn' - The ARN of the fleet.
--
-- 'authorizationProviderId', 'disassociateWebsiteAuthorizationProvider_authorizationProviderId' - A unique identifier for the authorization provider.
newDisassociateWebsiteAuthorizationProvider ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'authorizationProviderId'
  Prelude.Text ->
  DisassociateWebsiteAuthorizationProvider
newDisassociateWebsiteAuthorizationProvider
  pFleetArn_
  pAuthorizationProviderId_ =
    DisassociateWebsiteAuthorizationProvider'
      { fleetArn =
          pFleetArn_,
        authorizationProviderId =
          pAuthorizationProviderId_
      }

-- | The ARN of the fleet.
disassociateWebsiteAuthorizationProvider_fleetArn :: Lens.Lens' DisassociateWebsiteAuthorizationProvider Prelude.Text
disassociateWebsiteAuthorizationProvider_fleetArn = Lens.lens (\DisassociateWebsiteAuthorizationProvider' {fleetArn} -> fleetArn) (\s@DisassociateWebsiteAuthorizationProvider' {} a -> s {fleetArn = a} :: DisassociateWebsiteAuthorizationProvider)

-- | A unique identifier for the authorization provider.
disassociateWebsiteAuthorizationProvider_authorizationProviderId :: Lens.Lens' DisassociateWebsiteAuthorizationProvider Prelude.Text
disassociateWebsiteAuthorizationProvider_authorizationProviderId = Lens.lens (\DisassociateWebsiteAuthorizationProvider' {authorizationProviderId} -> authorizationProviderId) (\s@DisassociateWebsiteAuthorizationProvider' {} a -> s {authorizationProviderId = a} :: DisassociateWebsiteAuthorizationProvider)

instance
  Core.AWSRequest
    DisassociateWebsiteAuthorizationProvider
  where
  type
    AWSResponse
      DisassociateWebsiteAuthorizationProvider =
      DisassociateWebsiteAuthorizationProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateWebsiteAuthorizationProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateWebsiteAuthorizationProvider

instance
  Prelude.NFData
    DisassociateWebsiteAuthorizationProvider

instance
  Core.ToHeaders
    DisassociateWebsiteAuthorizationProvider
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DisassociateWebsiteAuthorizationProvider
  where
  toJSON DisassociateWebsiteAuthorizationProvider' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just
              ( "AuthorizationProviderId"
                  Core..= authorizationProviderId
              )
          ]
      )

instance
  Core.ToPath
    DisassociateWebsiteAuthorizationProvider
  where
  toPath =
    Prelude.const
      "/disassociateWebsiteAuthorizationProvider"

instance
  Core.ToQuery
    DisassociateWebsiteAuthorizationProvider
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateWebsiteAuthorizationProviderResponse' smart constructor.
data DisassociateWebsiteAuthorizationProviderResponse = DisassociateWebsiteAuthorizationProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWebsiteAuthorizationProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateWebsiteAuthorizationProviderResponse_httpStatus' - The response's http status code.
newDisassociateWebsiteAuthorizationProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateWebsiteAuthorizationProviderResponse
newDisassociateWebsiteAuthorizationProviderResponse
  pHttpStatus_ =
    DisassociateWebsiteAuthorizationProviderResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateWebsiteAuthorizationProviderResponse_httpStatus :: Lens.Lens' DisassociateWebsiteAuthorizationProviderResponse Prelude.Int
disassociateWebsiteAuthorizationProviderResponse_httpStatus = Lens.lens (\DisassociateWebsiteAuthorizationProviderResponse' {httpStatus} -> httpStatus) (\s@DisassociateWebsiteAuthorizationProviderResponse' {} a -> s {httpStatus = a} :: DisassociateWebsiteAuthorizationProviderResponse)

instance
  Prelude.NFData
    DisassociateWebsiteAuthorizationProviderResponse
