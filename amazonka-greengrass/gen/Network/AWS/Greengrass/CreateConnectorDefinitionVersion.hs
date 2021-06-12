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
-- Module      : Network.AWS.Greengrass.CreateConnectorDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a connector definition which has already been
-- defined.
module Network.AWS.Greengrass.CreateConnectorDefinitionVersion
  ( -- * Creating a Request
    CreateConnectorDefinitionVersion (..),
    newCreateConnectorDefinitionVersion,

    -- * Request Lenses
    createConnectorDefinitionVersion_connectors,
    createConnectorDefinitionVersion_amznClientToken,
    createConnectorDefinitionVersion_connectorDefinitionId,

    -- * Destructuring the Response
    CreateConnectorDefinitionVersionResponse (..),
    newCreateConnectorDefinitionVersionResponse,

    -- * Response Lenses
    createConnectorDefinitionVersionResponse_creationTimestamp,
    createConnectorDefinitionVersionResponse_arn,
    createConnectorDefinitionVersionResponse_id,
    createConnectorDefinitionVersionResponse_version,
    createConnectorDefinitionVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateConnectorDefinitionVersion' smart constructor.
data CreateConnectorDefinitionVersion = CreateConnectorDefinitionVersion'
  { -- | A list of references to connectors in this version, with their
    -- corresponding configuration settings.
    connectors :: Core.Maybe [Connector],
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | The ID of the connector definition.
    connectorDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConnectorDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectors', 'createConnectorDefinitionVersion_connectors' - A list of references to connectors in this version, with their
-- corresponding configuration settings.
--
-- 'amznClientToken', 'createConnectorDefinitionVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'connectorDefinitionId', 'createConnectorDefinitionVersion_connectorDefinitionId' - The ID of the connector definition.
newCreateConnectorDefinitionVersion ::
  -- | 'connectorDefinitionId'
  Core.Text ->
  CreateConnectorDefinitionVersion
newCreateConnectorDefinitionVersion
  pConnectorDefinitionId_ =
    CreateConnectorDefinitionVersion'
      { connectors =
          Core.Nothing,
        amznClientToken = Core.Nothing,
        connectorDefinitionId =
          pConnectorDefinitionId_
      }

-- | A list of references to connectors in this version, with their
-- corresponding configuration settings.
createConnectorDefinitionVersion_connectors :: Lens.Lens' CreateConnectorDefinitionVersion (Core.Maybe [Connector])
createConnectorDefinitionVersion_connectors = Lens.lens (\CreateConnectorDefinitionVersion' {connectors} -> connectors) (\s@CreateConnectorDefinitionVersion' {} a -> s {connectors = a} :: CreateConnectorDefinitionVersion) Core.. Lens.mapping Lens._Coerce

-- | A client token used to correlate requests and responses.
createConnectorDefinitionVersion_amznClientToken :: Lens.Lens' CreateConnectorDefinitionVersion (Core.Maybe Core.Text)
createConnectorDefinitionVersion_amznClientToken = Lens.lens (\CreateConnectorDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateConnectorDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateConnectorDefinitionVersion)

-- | The ID of the connector definition.
createConnectorDefinitionVersion_connectorDefinitionId :: Lens.Lens' CreateConnectorDefinitionVersion Core.Text
createConnectorDefinitionVersion_connectorDefinitionId = Lens.lens (\CreateConnectorDefinitionVersion' {connectorDefinitionId} -> connectorDefinitionId) (\s@CreateConnectorDefinitionVersion' {} a -> s {connectorDefinitionId = a} :: CreateConnectorDefinitionVersion)

instance
  Core.AWSRequest
    CreateConnectorDefinitionVersion
  where
  type
    AWSResponse CreateConnectorDefinitionVersion =
      CreateConnectorDefinitionVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectorDefinitionVersionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateConnectorDefinitionVersion

instance Core.NFData CreateConnectorDefinitionVersion

instance
  Core.ToHeaders
    CreateConnectorDefinitionVersion
  where
  toHeaders CreateConnectorDefinitionVersion' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateConnectorDefinitionVersion where
  toJSON CreateConnectorDefinitionVersion' {..} =
    Core.object
      ( Core.catMaybes
          [("Connectors" Core..=) Core.<$> connectors]
      )

instance Core.ToPath CreateConnectorDefinitionVersion where
  toPath CreateConnectorDefinitionVersion' {..} =
    Core.mconcat
      [ "/greengrass/definition/connectors/",
        Core.toBS connectorDefinitionId,
        "/versions"
      ]

instance
  Core.ToQuery
    CreateConnectorDefinitionVersion
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateConnectorDefinitionVersionResponse' smart constructor.
data CreateConnectorDefinitionVersionResponse = CreateConnectorDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ARN of the version.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Core.Maybe Core.Text,
    -- | The ID of the version.
    version :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConnectorDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createConnectorDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'arn', 'createConnectorDefinitionVersionResponse_arn' - The ARN of the version.
--
-- 'id', 'createConnectorDefinitionVersionResponse_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'createConnectorDefinitionVersionResponse_version' - The ID of the version.
--
-- 'httpStatus', 'createConnectorDefinitionVersionResponse_httpStatus' - The response's http status code.
newCreateConnectorDefinitionVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateConnectorDefinitionVersionResponse
newCreateConnectorDefinitionVersionResponse
  pHttpStatus_ =
    CreateConnectorDefinitionVersionResponse'
      { creationTimestamp =
          Core.Nothing,
        arn = Core.Nothing,
        id = Core.Nothing,
        version = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time, in milliseconds since the epoch, when the version was created.
createConnectorDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
createConnectorDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateConnectorDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateConnectorDefinitionVersionResponse)

-- | The ARN of the version.
createConnectorDefinitionVersionResponse_arn :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
createConnectorDefinitionVersionResponse_arn = Lens.lens (\CreateConnectorDefinitionVersionResponse' {arn} -> arn) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {arn = a} :: CreateConnectorDefinitionVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createConnectorDefinitionVersionResponse_id :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
createConnectorDefinitionVersionResponse_id = Lens.lens (\CreateConnectorDefinitionVersionResponse' {id} -> id) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {id = a} :: CreateConnectorDefinitionVersionResponse)

-- | The ID of the version.
createConnectorDefinitionVersionResponse_version :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Core.Maybe Core.Text)
createConnectorDefinitionVersionResponse_version = Lens.lens (\CreateConnectorDefinitionVersionResponse' {version} -> version) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {version = a} :: CreateConnectorDefinitionVersionResponse)

-- | The response's http status code.
createConnectorDefinitionVersionResponse_httpStatus :: Lens.Lens' CreateConnectorDefinitionVersionResponse Core.Int
createConnectorDefinitionVersionResponse_httpStatus = Lens.lens (\CreateConnectorDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {httpStatus = a} :: CreateConnectorDefinitionVersionResponse)

instance
  Core.NFData
    CreateConnectorDefinitionVersionResponse
