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
-- Module      : Amazonka.Greengrass.CreateConnectorDefinitionVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a connector definition which has already been
-- defined.
module Amazonka.Greengrass.CreateConnectorDefinitionVersion
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
    createConnectorDefinitionVersionResponse_id,
    createConnectorDefinitionVersionResponse_version,
    createConnectorDefinitionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectorDefinitionVersionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateConnectorDefinitionVersion
  where
  hashWithSalt
    _salt
    CreateConnectorDefinitionVersion' {..} =
      _salt `Prelude.hashWithSalt` amznClientToken
        `Prelude.hashWithSalt` connectors
        `Prelude.hashWithSalt` connectorDefinitionId

instance
  Prelude.NFData
    CreateConnectorDefinitionVersion
  where
  rnf CreateConnectorDefinitionVersion' {..} =
    Prelude.rnf amznClientToken
      `Prelude.seq` Prelude.rnf connectors
      `Prelude.seq` Prelude.rnf connectorDefinitionId

instance
  Data.ToHeaders
    CreateConnectorDefinitionVersion
  where
  toHeaders CreateConnectorDefinitionVersion' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# amznClientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateConnectorDefinitionVersion where
  toJSON CreateConnectorDefinitionVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Connectors" Data..=) Prelude.<$> connectors]
      )

instance Data.ToPath CreateConnectorDefinitionVersion where
  toPath CreateConnectorDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/connectors/",
        Data.toBS connectorDefinitionId,
        "/versions"
      ]

instance
  Data.ToQuery
    CreateConnectorDefinitionVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectorDefinitionVersionResponse' smart constructor.
data CreateConnectorDefinitionVersionResponse = CreateConnectorDefinitionVersionResponse'
  { -- | The ARN of the version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version.
    version :: Prelude.Maybe Prelude.Text,
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
-- 'id', 'createConnectorDefinitionVersionResponse_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'createConnectorDefinitionVersionResponse_version' - The ID of the version.
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
        id = Prelude.Nothing,
        version = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the version.
createConnectorDefinitionVersionResponse_arn :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionVersionResponse_arn = Lens.lens (\CreateConnectorDefinitionVersionResponse' {arn} -> arn) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {arn = a} :: CreateConnectorDefinitionVersionResponse)

-- | The time, in milliseconds since the epoch, when the version was created.
createConnectorDefinitionVersionResponse_creationTimestamp :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionVersionResponse_creationTimestamp = Lens.lens (\CreateConnectorDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: CreateConnectorDefinitionVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createConnectorDefinitionVersionResponse_id :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionVersionResponse_id = Lens.lens (\CreateConnectorDefinitionVersionResponse' {id} -> id) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {id = a} :: CreateConnectorDefinitionVersionResponse)

-- | The ID of the version.
createConnectorDefinitionVersionResponse_version :: Lens.Lens' CreateConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
createConnectorDefinitionVersionResponse_version = Lens.lens (\CreateConnectorDefinitionVersionResponse' {version} -> version) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {version = a} :: CreateConnectorDefinitionVersionResponse)

-- | The response's http status code.
createConnectorDefinitionVersionResponse_httpStatus :: Lens.Lens' CreateConnectorDefinitionVersionResponse Prelude.Int
createConnectorDefinitionVersionResponse_httpStatus = Lens.lens (\CreateConnectorDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectorDefinitionVersionResponse' {} a -> s {httpStatus = a} :: CreateConnectorDefinitionVersionResponse)

instance
  Prelude.NFData
    CreateConnectorDefinitionVersionResponse
  where
  rnf CreateConnectorDefinitionVersionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
