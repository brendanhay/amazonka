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
-- Module      : Network.AWS.OpsWorksCM.UpdateServerEngineAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates engine-specific attributes on a specified server. The server
-- enters the @MODIFYING@ state when this operation is in progress. Only
-- one update can occur at a time. You can use this command to reset a Chef
-- server\'s public key (@CHEF_PIVOTAL_KEY@) or a Puppet server\'s admin
-- password (@PUPPET_ADMIN_PASSWORD@).
--
-- This operation is asynchronous.
--
-- This operation can only be called for servers in @HEALTHY@ or
-- @UNHEALTHY@ states. Otherwise, an @InvalidStateException@ is raised. A
-- @ResourceNotFoundException@ is thrown when the server does not exist. A
-- @ValidationException@ is raised when parameters of the request are not
-- valid.
module Network.AWS.OpsWorksCM.UpdateServerEngineAttributes
  ( -- * Creating a Request
    UpdateServerEngineAttributes (..),
    newUpdateServerEngineAttributes,

    -- * Request Lenses
    updateServerEngineAttributes_attributeValue,
    updateServerEngineAttributes_serverName,
    updateServerEngineAttributes_attributeName,

    -- * Destructuring the Response
    UpdateServerEngineAttributesResponse (..),
    newUpdateServerEngineAttributesResponse,

    -- * Response Lenses
    updateServerEngineAttributesResponse_server,
    updateServerEngineAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateServerEngineAttributes' smart constructor.
data UpdateServerEngineAttributes = UpdateServerEngineAttributes'
  { -- | The value to set for the attribute.
    attributeValue :: Core.Maybe Core.Text,
    -- | The name of the server to update.
    serverName :: Core.Text,
    -- | The name of the engine attribute to update.
    attributeName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateServerEngineAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValue', 'updateServerEngineAttributes_attributeValue' - The value to set for the attribute.
--
-- 'serverName', 'updateServerEngineAttributes_serverName' - The name of the server to update.
--
-- 'attributeName', 'updateServerEngineAttributes_attributeName' - The name of the engine attribute to update.
newUpdateServerEngineAttributes ::
  -- | 'serverName'
  Core.Text ->
  -- | 'attributeName'
  Core.Text ->
  UpdateServerEngineAttributes
newUpdateServerEngineAttributes
  pServerName_
  pAttributeName_ =
    UpdateServerEngineAttributes'
      { attributeValue =
          Core.Nothing,
        serverName = pServerName_,
        attributeName = pAttributeName_
      }

-- | The value to set for the attribute.
updateServerEngineAttributes_attributeValue :: Lens.Lens' UpdateServerEngineAttributes (Core.Maybe Core.Text)
updateServerEngineAttributes_attributeValue = Lens.lens (\UpdateServerEngineAttributes' {attributeValue} -> attributeValue) (\s@UpdateServerEngineAttributes' {} a -> s {attributeValue = a} :: UpdateServerEngineAttributes)

-- | The name of the server to update.
updateServerEngineAttributes_serverName :: Lens.Lens' UpdateServerEngineAttributes Core.Text
updateServerEngineAttributes_serverName = Lens.lens (\UpdateServerEngineAttributes' {serverName} -> serverName) (\s@UpdateServerEngineAttributes' {} a -> s {serverName = a} :: UpdateServerEngineAttributes)

-- | The name of the engine attribute to update.
updateServerEngineAttributes_attributeName :: Lens.Lens' UpdateServerEngineAttributes Core.Text
updateServerEngineAttributes_attributeName = Lens.lens (\UpdateServerEngineAttributes' {attributeName} -> attributeName) (\s@UpdateServerEngineAttributes' {} a -> s {attributeName = a} :: UpdateServerEngineAttributes)

instance Core.AWSRequest UpdateServerEngineAttributes where
  type
    AWSResponse UpdateServerEngineAttributes =
      UpdateServerEngineAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServerEngineAttributesResponse'
            Core.<$> (x Core..?> "Server")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateServerEngineAttributes

instance Core.NFData UpdateServerEngineAttributes

instance Core.ToHeaders UpdateServerEngineAttributes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorksCM_V2016_11_01.UpdateServerEngineAttributes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateServerEngineAttributes where
  toJSON UpdateServerEngineAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AttributeValue" Core..=) Core.<$> attributeValue,
            Core.Just ("ServerName" Core..= serverName),
            Core.Just ("AttributeName" Core..= attributeName)
          ]
      )

instance Core.ToPath UpdateServerEngineAttributes where
  toPath = Core.const "/"

instance Core.ToQuery UpdateServerEngineAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateServerEngineAttributesResponse' smart constructor.
data UpdateServerEngineAttributesResponse = UpdateServerEngineAttributesResponse'
  { -- | Contains the response to an @UpdateServerEngineAttributes@ request.
    server :: Core.Maybe Server,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateServerEngineAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'server', 'updateServerEngineAttributesResponse_server' - Contains the response to an @UpdateServerEngineAttributes@ request.
--
-- 'httpStatus', 'updateServerEngineAttributesResponse_httpStatus' - The response's http status code.
newUpdateServerEngineAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateServerEngineAttributesResponse
newUpdateServerEngineAttributesResponse pHttpStatus_ =
  UpdateServerEngineAttributesResponse'
    { server =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the response to an @UpdateServerEngineAttributes@ request.
updateServerEngineAttributesResponse_server :: Lens.Lens' UpdateServerEngineAttributesResponse (Core.Maybe Server)
updateServerEngineAttributesResponse_server = Lens.lens (\UpdateServerEngineAttributesResponse' {server} -> server) (\s@UpdateServerEngineAttributesResponse' {} a -> s {server = a} :: UpdateServerEngineAttributesResponse)

-- | The response's http status code.
updateServerEngineAttributesResponse_httpStatus :: Lens.Lens' UpdateServerEngineAttributesResponse Core.Int
updateServerEngineAttributesResponse_httpStatus = Lens.lens (\UpdateServerEngineAttributesResponse' {httpStatus} -> httpStatus) (\s@UpdateServerEngineAttributesResponse' {} a -> s {httpStatus = a} :: UpdateServerEngineAttributesResponse)

instance
  Core.NFData
    UpdateServerEngineAttributesResponse
