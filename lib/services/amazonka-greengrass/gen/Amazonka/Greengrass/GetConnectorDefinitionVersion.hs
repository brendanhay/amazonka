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
-- Module      : Amazonka.Greengrass.GetConnectorDefinitionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a connector definition version, including
-- the connectors that the version contains. Connectors are prebuilt
-- modules that interact with local infrastructure, device protocols, AWS,
-- and other cloud services.
module Amazonka.Greengrass.GetConnectorDefinitionVersion
  ( -- * Creating a Request
    GetConnectorDefinitionVersion (..),
    newGetConnectorDefinitionVersion,

    -- * Request Lenses
    getConnectorDefinitionVersion_nextToken,
    getConnectorDefinitionVersion_connectorDefinitionId,
    getConnectorDefinitionVersion_connectorDefinitionVersionId,

    -- * Destructuring the Response
    GetConnectorDefinitionVersionResponse (..),
    newGetConnectorDefinitionVersionResponse,

    -- * Response Lenses
    getConnectorDefinitionVersionResponse_arn,
    getConnectorDefinitionVersionResponse_creationTimestamp,
    getConnectorDefinitionVersionResponse_definition,
    getConnectorDefinitionVersionResponse_id,
    getConnectorDefinitionVersionResponse_nextToken,
    getConnectorDefinitionVersionResponse_version,
    getConnectorDefinitionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConnectorDefinitionVersion' smart constructor.
data GetConnectorDefinitionVersion = GetConnectorDefinitionVersion'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the connector definition.
    connectorDefinitionId :: Prelude.Text,
    -- | The ID of the connector definition version. This value maps to the
    -- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
    -- object, which is returned by \'\'ListConnectorDefinitionVersions\'\'
    -- requests. If the version is the last one that was associated with a
    -- connector definition, the value also maps to the \'\'LatestVersion\'\'
    -- property of the corresponding \'\'DefinitionInformation\'\' object.
    connectorDefinitionVersionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectorDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getConnectorDefinitionVersion_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'connectorDefinitionId', 'getConnectorDefinitionVersion_connectorDefinitionId' - The ID of the connector definition.
--
-- 'connectorDefinitionVersionId', 'getConnectorDefinitionVersion_connectorDefinitionVersionId' - The ID of the connector definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListConnectorDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- connector definition, the value also maps to the \'\'LatestVersion\'\'
-- property of the corresponding \'\'DefinitionInformation\'\' object.
newGetConnectorDefinitionVersion ::
  -- | 'connectorDefinitionId'
  Prelude.Text ->
  -- | 'connectorDefinitionVersionId'
  Prelude.Text ->
  GetConnectorDefinitionVersion
newGetConnectorDefinitionVersion
  pConnectorDefinitionId_
  pConnectorDefinitionVersionId_ =
    GetConnectorDefinitionVersion'
      { nextToken =
          Prelude.Nothing,
        connectorDefinitionId =
          pConnectorDefinitionId_,
        connectorDefinitionVersionId =
          pConnectorDefinitionVersionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getConnectorDefinitionVersion_nextToken :: Lens.Lens' GetConnectorDefinitionVersion (Prelude.Maybe Prelude.Text)
getConnectorDefinitionVersion_nextToken = Lens.lens (\GetConnectorDefinitionVersion' {nextToken} -> nextToken) (\s@GetConnectorDefinitionVersion' {} a -> s {nextToken = a} :: GetConnectorDefinitionVersion)

-- | The ID of the connector definition.
getConnectorDefinitionVersion_connectorDefinitionId :: Lens.Lens' GetConnectorDefinitionVersion Prelude.Text
getConnectorDefinitionVersion_connectorDefinitionId = Lens.lens (\GetConnectorDefinitionVersion' {connectorDefinitionId} -> connectorDefinitionId) (\s@GetConnectorDefinitionVersion' {} a -> s {connectorDefinitionId = a} :: GetConnectorDefinitionVersion)

-- | The ID of the connector definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListConnectorDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- connector definition, the value also maps to the \'\'LatestVersion\'\'
-- property of the corresponding \'\'DefinitionInformation\'\' object.
getConnectorDefinitionVersion_connectorDefinitionVersionId :: Lens.Lens' GetConnectorDefinitionVersion Prelude.Text
getConnectorDefinitionVersion_connectorDefinitionVersionId = Lens.lens (\GetConnectorDefinitionVersion' {connectorDefinitionVersionId} -> connectorDefinitionVersionId) (\s@GetConnectorDefinitionVersion' {} a -> s {connectorDefinitionVersionId = a} :: GetConnectorDefinitionVersion)

instance
  Core.AWSRequest
    GetConnectorDefinitionVersion
  where
  type
    AWSResponse GetConnectorDefinitionVersion =
      GetConnectorDefinitionVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectorDefinitionVersionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Definition")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetConnectorDefinitionVersion
  where
  hashWithSalt _salt GetConnectorDefinitionVersion' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` connectorDefinitionId
      `Prelude.hashWithSalt` connectorDefinitionVersionId

instance Prelude.NFData GetConnectorDefinitionVersion where
  rnf GetConnectorDefinitionVersion' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf connectorDefinitionId `Prelude.seq`
        Prelude.rnf connectorDefinitionVersionId

instance Data.ToHeaders GetConnectorDefinitionVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetConnectorDefinitionVersion where
  toPath GetConnectorDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/connectors/",
        Data.toBS connectorDefinitionId,
        "/versions/",
        Data.toBS connectorDefinitionVersionId
      ]

instance Data.ToQuery GetConnectorDefinitionVersion where
  toQuery GetConnectorDefinitionVersion' {..} =
    Prelude.mconcat ["NextToken" Data.=: nextToken]

-- | /See:/ 'newGetConnectorDefinitionVersionResponse' smart constructor.
data GetConnectorDefinitionVersionResponse = GetConnectorDefinitionVersionResponse'
  { -- | The ARN of the connector definition version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the connector definition
    -- version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | Information about the connector definition version.
    definition :: Prelude.Maybe ConnectorDefinitionVersion,
    -- | The ID of the connector definition version.
    id :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The version of the connector definition version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectorDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getConnectorDefinitionVersionResponse_arn' - The ARN of the connector definition version.
--
-- 'creationTimestamp', 'getConnectorDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the connector definition
-- version was created.
--
-- 'definition', 'getConnectorDefinitionVersionResponse_definition' - Information about the connector definition version.
--
-- 'id', 'getConnectorDefinitionVersionResponse_id' - The ID of the connector definition version.
--
-- 'nextToken', 'getConnectorDefinitionVersionResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'version', 'getConnectorDefinitionVersionResponse_version' - The version of the connector definition version.
--
-- 'httpStatus', 'getConnectorDefinitionVersionResponse_httpStatus' - The response's http status code.
newGetConnectorDefinitionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConnectorDefinitionVersionResponse
newGetConnectorDefinitionVersionResponse pHttpStatus_ =
  GetConnectorDefinitionVersionResponse'
    { arn =
        Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      definition = Prelude.Nothing,
      id = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the connector definition version.
getConnectorDefinitionVersionResponse_arn :: Lens.Lens' GetConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionVersionResponse_arn = Lens.lens (\GetConnectorDefinitionVersionResponse' {arn} -> arn) (\s@GetConnectorDefinitionVersionResponse' {} a -> s {arn = a} :: GetConnectorDefinitionVersionResponse)

-- | The time, in milliseconds since the epoch, when the connector definition
-- version was created.
getConnectorDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetConnectorDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetConnectorDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetConnectorDefinitionVersionResponse)

-- | Information about the connector definition version.
getConnectorDefinitionVersionResponse_definition :: Lens.Lens' GetConnectorDefinitionVersionResponse (Prelude.Maybe ConnectorDefinitionVersion)
getConnectorDefinitionVersionResponse_definition = Lens.lens (\GetConnectorDefinitionVersionResponse' {definition} -> definition) (\s@GetConnectorDefinitionVersionResponse' {} a -> s {definition = a} :: GetConnectorDefinitionVersionResponse)

-- | The ID of the connector definition version.
getConnectorDefinitionVersionResponse_id :: Lens.Lens' GetConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionVersionResponse_id = Lens.lens (\GetConnectorDefinitionVersionResponse' {id} -> id) (\s@GetConnectorDefinitionVersionResponse' {} a -> s {id = a} :: GetConnectorDefinitionVersionResponse)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getConnectorDefinitionVersionResponse_nextToken :: Lens.Lens' GetConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionVersionResponse_nextToken = Lens.lens (\GetConnectorDefinitionVersionResponse' {nextToken} -> nextToken) (\s@GetConnectorDefinitionVersionResponse' {} a -> s {nextToken = a} :: GetConnectorDefinitionVersionResponse)

-- | The version of the connector definition version.
getConnectorDefinitionVersionResponse_version :: Lens.Lens' GetConnectorDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionVersionResponse_version = Lens.lens (\GetConnectorDefinitionVersionResponse' {version} -> version) (\s@GetConnectorDefinitionVersionResponse' {} a -> s {version = a} :: GetConnectorDefinitionVersionResponse)

-- | The response's http status code.
getConnectorDefinitionVersionResponse_httpStatus :: Lens.Lens' GetConnectorDefinitionVersionResponse Prelude.Int
getConnectorDefinitionVersionResponse_httpStatus = Lens.lens (\GetConnectorDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@GetConnectorDefinitionVersionResponse' {} a -> s {httpStatus = a} :: GetConnectorDefinitionVersionResponse)

instance
  Prelude.NFData
    GetConnectorDefinitionVersionResponse
  where
  rnf GetConnectorDefinitionVersionResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTimestamp `Prelude.seq`
        Prelude.rnf definition `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf nextToken `Prelude.seq`
              Prelude.rnf version `Prelude.seq`
                Prelude.rnf httpStatus
