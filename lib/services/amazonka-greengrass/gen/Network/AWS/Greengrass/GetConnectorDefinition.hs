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
-- Module      : Network.AWS.Greengrass.GetConnectorDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a connector definition.
module Network.AWS.Greengrass.GetConnectorDefinition
  ( -- * Creating a Request
    GetConnectorDefinition (..),
    newGetConnectorDefinition,

    -- * Request Lenses
    getConnectorDefinition_connectorDefinitionId,

    -- * Destructuring the Response
    GetConnectorDefinitionResponse (..),
    newGetConnectorDefinitionResponse,

    -- * Response Lenses
    getConnectorDefinitionResponse_creationTimestamp,
    getConnectorDefinitionResponse_latestVersionArn,
    getConnectorDefinitionResponse_latestVersion,
    getConnectorDefinitionResponse_arn,
    getConnectorDefinitionResponse_id,
    getConnectorDefinitionResponse_name,
    getConnectorDefinitionResponse_tags,
    getConnectorDefinitionResponse_lastUpdatedTimestamp,
    getConnectorDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectorDefinitionResponse'
            Prelude.<$> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "LatestVersionArn")
            Prelude.<*> (x Core..?> "LatestVersion")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "LastUpdatedTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnectorDefinition

instance Prelude.NFData GetConnectorDefinition

instance Core.ToHeaders GetConnectorDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetConnectorDefinition where
  toPath GetConnectorDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/connectors/",
        Core.toBS connectorDefinitionId
      ]

instance Core.ToQuery GetConnectorDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectorDefinitionResponse' smart constructor.
data GetConnectorDefinitionResponse = GetConnectorDefinitionResponse'
  { -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
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
-- 'creationTimestamp', 'getConnectorDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getConnectorDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'getConnectorDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'getConnectorDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'getConnectorDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'getConnectorDefinitionResponse_name' - The name of the definition.
--
-- 'tags', 'getConnectorDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'lastUpdatedTimestamp', 'getConnectorDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'httpStatus', 'getConnectorDefinitionResponse_httpStatus' - The response's http status code.
newGetConnectorDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConnectorDefinitionResponse
newGetConnectorDefinitionResponse pHttpStatus_ =
  GetConnectorDefinitionResponse'
    { creationTimestamp =
        Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
getConnectorDefinitionResponse_creationTimestamp :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_creationTimestamp = Lens.lens (\GetConnectorDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetConnectorDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetConnectorDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getConnectorDefinitionResponse_latestVersionArn :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_latestVersionArn = Lens.lens (\GetConnectorDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetConnectorDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetConnectorDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getConnectorDefinitionResponse_latestVersion :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_latestVersion = Lens.lens (\GetConnectorDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetConnectorDefinitionResponse' {} a -> s {latestVersion = a} :: GetConnectorDefinitionResponse)

-- | The ARN of the definition.
getConnectorDefinitionResponse_arn :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_arn = Lens.lens (\GetConnectorDefinitionResponse' {arn} -> arn) (\s@GetConnectorDefinitionResponse' {} a -> s {arn = a} :: GetConnectorDefinitionResponse)

-- | The ID of the definition.
getConnectorDefinitionResponse_id :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_id = Lens.lens (\GetConnectorDefinitionResponse' {id} -> id) (\s@GetConnectorDefinitionResponse' {} a -> s {id = a} :: GetConnectorDefinitionResponse)

-- | The name of the definition.
getConnectorDefinitionResponse_name :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_name = Lens.lens (\GetConnectorDefinitionResponse' {name} -> name) (\s@GetConnectorDefinitionResponse' {} a -> s {name = a} :: GetConnectorDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getConnectorDefinitionResponse_tags :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getConnectorDefinitionResponse_tags = Lens.lens (\GetConnectorDefinitionResponse' {tags} -> tags) (\s@GetConnectorDefinitionResponse' {} a -> s {tags = a} :: GetConnectorDefinitionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getConnectorDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetConnectorDefinitionResponse (Prelude.Maybe Prelude.Text)
getConnectorDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetConnectorDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetConnectorDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetConnectorDefinitionResponse)

-- | The response's http status code.
getConnectorDefinitionResponse_httpStatus :: Lens.Lens' GetConnectorDefinitionResponse Prelude.Int
getConnectorDefinitionResponse_httpStatus = Lens.lens (\GetConnectorDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetConnectorDefinitionResponse' {} a -> s {httpStatus = a} :: GetConnectorDefinitionResponse)

instance
  Prelude.NFData
    GetConnectorDefinitionResponse
