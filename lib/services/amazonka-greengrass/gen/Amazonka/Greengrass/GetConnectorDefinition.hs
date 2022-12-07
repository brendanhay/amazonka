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
-- Module      : Amazonka.Greengrass.GetConnectorDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a connector definition.
module Amazonka.Greengrass.GetConnectorDefinition
  ( -- * Creating a Request
    GetConnectorDefinition (..),
    newGetConnectorDefinition,

    -- * Request Lenses
    getConnectorDefinition_connectorDefinitionId,

    -- * Destructuring the Response
    GetConnectorDefinitionResponse (..),
    newGetConnectorDefinitionResponse,

    -- * Response Lenses
    getConnectorDefinitionResponse_lastUpdatedTimestamp,
    getConnectorDefinitionResponse_tags,
    getConnectorDefinitionResponse_name,
    getConnectorDefinitionResponse_arn,
    getConnectorDefinitionResponse_latestVersion,
    getConnectorDefinitionResponse_id,
    getConnectorDefinitionResponse_creationTimestamp,
    getConnectorDefinitionResponse_latestVersionArn,
    getConnectorDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConnectorDefinition' smart constructor.
data GetConnectorDefinition = GetConnectorDefinition'
  { -- | The ID of the connector definition.
    connectorDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectorDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorDefinitionId', 'getConnectorDefinition_connectorDefinitionId' - The ID of the connector definition.
newGetConnectorDefinition ::
  -- | 'connectorDefinitionId'
  Prelude.Text ->
  GetConnectorDefinition
newGetConnectorDefinition pConnectorDefinitionId_ =
  GetConnectorDefinition'
    { connectorDefinitionId =
        pConnectorDefinitionId_
    }

-- | The ID of the connector definition.
getConnectorDefinition_connectorDefinitionId :: Lens.Lens' GetConnectorDefinition Prelude.Text
getConnectorDefinition_connectorDefinitionId = Lens.lens (\GetConnectorDefinition' {connectorDefinitionId} -> connectorDefinitionId) (\s@GetConnectorDefinition' {} a -> s {connectorDefinitionId = a} :: GetConnectorDefinition)

instance Core.AWSRequest GetConnectorDefinition where
  type
    AWSResponse GetConnectorDefinition =
      GetConnectorDefinitionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectorDefinitionResponse'
            Prelude.<$> (x Data..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "LatestVersion")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "LatestVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnectorDefinition where
  hashWithSalt _salt GetConnectorDefinition' {..} =
    _salt `Prelude.hashWithSalt` connectorDefinitionId

instance Prelude.NFData GetConnectorDefinition where
  rnf GetConnectorDefinition' {..} =
    Prelude.rnf connectorDefinitionId

instance Data.ToHeaders GetConnectorDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetConnectorDefinition where
  toPath GetConnectorDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/connectors/",
        Data.toBS connectorDefinitionId
      ]

instance Data.ToQuery GetConnectorDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectorDefinitionResponse' smart constructor.
data GetConnectorDefinitionResponse = GetConnectorDefinitionResponse'
  { -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectorDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimestamp', 'getConnectorDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getConnectorDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'name', 'getConnectorDefinitionResponse_name' - The name of the definition.
--
-- 'arn', 'getConnectorDefinitionResponse_arn' - The ARN of the definition.
--
-- 'latestVersion', 'getConnectorDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'id', 'getConnectorDefinitionResponse_id' - The ID of the definition.
--
-- 'creationTimestamp', 'getConnectorDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getConnectorDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'httpStatus', 'getConnectorDefinitionResponse_httpStatus' - The response's http status code.
newGetConnectorDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConnectorDefinitionResponse
newGetConnectorDefinitionResponse pHttpStatus_ =
  GetConnectorDefinitionResponse'
    { lastUpdatedTimestamp =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      id = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getConnectorDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetConnectorDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetConnectorDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetConnectorDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getConnectorDefinitionResponse_tags :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getConnectorDefinitionResponse_tags = Lens.lens (\GetConnectorDefinitionResponse' {tags} -> tags) (\s@GetConnectorDefinitionResponse' {} a -> s {tags = a} :: GetConnectorDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the definition.
getConnectorDefinitionResponse_name :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_name = Lens.lens (\GetConnectorDefinitionResponse' {name} -> name) (\s@GetConnectorDefinitionResponse' {} a -> s {name = a} :: GetConnectorDefinitionResponse)

-- | The ARN of the definition.
getConnectorDefinitionResponse_arn :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_arn = Lens.lens (\GetConnectorDefinitionResponse' {arn} -> arn) (\s@GetConnectorDefinitionResponse' {} a -> s {arn = a} :: GetConnectorDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getConnectorDefinitionResponse_latestVersion :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_latestVersion = Lens.lens (\GetConnectorDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetConnectorDefinitionResponse' {} a -> s {latestVersion = a} :: GetConnectorDefinitionResponse)

-- | The ID of the definition.
getConnectorDefinitionResponse_id :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_id = Lens.lens (\GetConnectorDefinitionResponse' {id} -> id) (\s@GetConnectorDefinitionResponse' {} a -> s {id = a} :: GetConnectorDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
getConnectorDefinitionResponse_creationTimestamp :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_creationTimestamp = Lens.lens (\GetConnectorDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetConnectorDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetConnectorDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getConnectorDefinitionResponse_latestVersionArn :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_latestVersionArn = Lens.lens (\GetConnectorDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetConnectorDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetConnectorDefinitionResponse)

-- | The response's http status code.
getConnectorDefinitionResponse_httpStatus :: Lens.Lens' GetConnectorDefinitionResponse Prelude.Int
getConnectorDefinitionResponse_httpStatus = Lens.lens (\GetConnectorDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetConnectorDefinitionResponse' {} a -> s {httpStatus = a} :: GetConnectorDefinitionResponse)

instance
  Prelude.NFData
    GetConnectorDefinitionResponse
  where
  rnf GetConnectorDefinitionResponse' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf latestVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
