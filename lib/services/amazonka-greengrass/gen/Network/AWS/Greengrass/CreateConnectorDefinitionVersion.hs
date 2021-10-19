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
    createConnectorDefinitionVersion_amznClientToken,
    createConnectorDefinitionVersion_connectors,
    createConnectorDefinitionVersion_connectorDefinitionId,

    -- * Destructuring the Response
    CreateConnectorDefinitionVersionResponse (..),
    newCreateConnectorDefinitionVersionResponse,

    -- * Response Lenses
    createConnectorDefinitionVersionResponse_arn,
    createConnectorDefinitionVersionResponse_creationTimestamp,
    createConnectorDefinitionVersionResponse_version,
    createConnectorDefinitionVersionResponse_id,
    createConnectorDefinitionVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateConnectorDefinitionVersion' smart constructor.
data CreateConnectorDefinitionVersion = CreateConnectorDefinitionVersion'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | A list of references to connectors in this version, with their
    -- corresponding configuration settings.
    connectors :: Prelude.Maybe [Connector],
    -- | The ID of the connector definition.
    connectorDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectorDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amznClientToken', 'createConnectorDefinitionVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'connectors', 'createConnectorDefinitionVersion_connectors' - A list of references to connectors in this version, with their
-- corresponding configuration settings.
--
-- 'connectorDefinitionId', 'createConnectorDefinitionVersion_connectorDefinitionId' - The ID of the connector definition.
newCreateConnectorDefinitionVersion ::
  -- | 'connectorDefinitionId'
  Prelude.Text ->
  CreateConnectorDefinitionVersion
newCreateConnectorDefinitionVersion
  pConnectorDefinitionId_ =
    CreateConnectorDefinitionVersion'
      { amznClientToken =
          Prelude.Nothing,
        connectors = Prelude.Nothing,
        connectorDefinitionId =
          pConnectorDefinitionId_
      }

-- | A client token used to correlate requests and responses.
createConnectorDefinitionVersion_amznClientToken :: Lens.Lens' CreateConnectorDefinitionVersion (Prelude.Maybe Prelude.Text)
createConnectorDefinitionVersion_amznClientToken = Lens.lens (\CreateConnectorDefinitionVersion' {amznClientToken} -> amznClientToken) (\s@CreateConnectorDefinitionVersion' {} a -> s {amznClientToken = a} :: CreateConnectorDefinitionVersion)

-- | A list of references to connectors in this version, with their
-- corresponding configuration settings.
createConnectorDefinitionVersion_connectors :: Lens.Lens' CreateConnectorDefinitionVersion (Prelude.Maybe [Connector])
createConnectorDefinitionVersion_connectors = Lens.lens (\CreateConnectorDefinitionVersion' {connectors} -> connectors) (\s@CreateConnectorDefinitionVersion' {} a -> s {connectors = a} :: CreateConnectorDefinitionVersion) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the connector definition.
createConnectorDefinitionVersion_connectorDefinitionId :: Lens.Lens' CreateConnectorDefinitionVersion Prelude.Text
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
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "Version")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateConnectorDefinitionVersion

instance
  Prelude.NFData
    CreateConnectorDefinitionVersion

instance
  Core.ToHeaders
    CreateConnectorDefinitionVersion
  where
  toHeaders CreateConnectorDefinitionVersion' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateConnectorDefinitionVersion where
  toJSON CreateConnectorDefinitionVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Connectors" Core..=) Prelude.<$> connectors]
      )

instance Core.ToPath CreateConnectorDefinitionVersion where
  toPath CreateConnectorDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/connectors/",
        Core.toBS connectorDefinitionId,
        "/versions"
      ]

instance
  Core.ToQuery
    CreateConnectorDefinitionVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectorDefinitionVersionResponse' smart constructor.
data CreateConnectorDefinitionVersionResponse = CreateConnectorDefinitionVersionResponse'
  { -- | The ARN of the version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectorDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createConnectorDefinitionVersionResponse_arn' - The ARN of the version.
--
-- 'creationTimestamp', 'createConnectorDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'version', 'createConnectorDefinitionVersionResponse_version' - The ID of the version.
--
-- 'id', 'createConnectorDefinitionVersionResponse_id' - The ID of the parent definition that the version is associated with.
--
-- 'httpStatus', 'createConnectorDefinitionVersionResponse_httpStatus' - The response's http status code.
newCreateConnectorDefinitionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConnectorDefinitionVersionResponse
newCreateConnectorDefinitionVersionResponse
  pHttpStatus_ =
    CreateConnectorDefinitionVersionResponse'
      { arn =
          Prelude.Nothing,
        creationTimestamp =
          Prelude.Nothing,
        version = Prelude.Nothing,
        id = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the version.
createConnectorDefinitionVersionResponse_arn :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionVersionResponse_arn = Lens.lens (\CreateConnectorDefinitionVersionResponse' {arn} -> arn) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {arn = a} :: CreateConnectorDefinitionVersionResponse)

-- | The time, in milliseconds since the epoch, when the version was created.
createConnectorDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateConnectorDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateConnectorDefinitionVersionResponse)

-- | The ID of the version.
createConnectorDefinitionVersionResponse_version :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionVersionResponse_version = Lens.lens (\CreateConnectorDefinitionVersionResponse' {version} -> version) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {version = a} :: CreateConnectorDefinitionVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createConnectorDefinitionVersionResponse_id :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionVersionResponse_id = Lens.lens (\CreateConnectorDefinitionVersionResponse' {id} -> id) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {id = a} :: CreateConnectorDefinitionVersionResponse)

-- | The response's http status code.
createConnectorDefinitionVersionResponse_httpStatus :: Lens.Lens' CreateConnectorDefinitionVersionResponse Prelude.Int
createConnectorDefinitionVersionResponse_httpStatus = Lens.lens (\CreateConnectorDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {httpStatus = a} :: CreateConnectorDefinitionVersionResponse)

instance
  Prelude.NFData
    CreateConnectorDefinitionVersionResponse
