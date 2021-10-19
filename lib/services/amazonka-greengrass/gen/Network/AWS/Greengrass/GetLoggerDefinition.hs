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
-- Module      : Network.AWS.Greengrass.GetLoggerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a logger definition.
module Network.AWS.Greengrass.GetLoggerDefinition
  ( -- * Creating a Request
    GetLoggerDefinition (..),
    newGetLoggerDefinition,

    -- * Request Lenses
    getLoggerDefinition_loggerDefinitionId,

    -- * Destructuring the Response
    GetLoggerDefinitionResponse (..),
    newGetLoggerDefinitionResponse,

    -- * Response Lenses
    getLoggerDefinitionResponse_latestVersionArn,
    getLoggerDefinitionResponse_arn,
    getLoggerDefinitionResponse_name,
    getLoggerDefinitionResponse_creationTimestamp,
    getLoggerDefinitionResponse_id,
    getLoggerDefinitionResponse_latestVersion,
    getLoggerDefinitionResponse_lastUpdatedTimestamp,
    getLoggerDefinitionResponse_tags,
    getLoggerDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLoggerDefinition' smart constructor.
data GetLoggerDefinition = GetLoggerDefinition'
  { -- | The ID of the logger definition.
    loggerDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggerDefinitionId', 'getLoggerDefinition_loggerDefinitionId' - The ID of the logger definition.
newGetLoggerDefinition ::
  -- | 'loggerDefinitionId'
  Prelude.Text ->
  GetLoggerDefinition
newGetLoggerDefinition pLoggerDefinitionId_ =
  GetLoggerDefinition'
    { loggerDefinitionId =
        pLoggerDefinitionId_
    }

-- | The ID of the logger definition.
getLoggerDefinition_loggerDefinitionId :: Lens.Lens' GetLoggerDefinition Prelude.Text
getLoggerDefinition_loggerDefinitionId = Lens.lens (\GetLoggerDefinition' {loggerDefinitionId} -> loggerDefinitionId) (\s@GetLoggerDefinition' {} a -> s {loggerDefinitionId = a} :: GetLoggerDefinition)

instance Core.AWSRequest GetLoggerDefinition where
  type
    AWSResponse GetLoggerDefinition =
      GetLoggerDefinitionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggerDefinitionResponse'
            Prelude.<$> (x Core..?> "LatestVersionArn")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "LatestVersion")
            Prelude.<*> (x Core..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLoggerDefinition

instance Prelude.NFData GetLoggerDefinition

instance Core.ToHeaders GetLoggerDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetLoggerDefinition where
  toPath GetLoggerDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/loggers/",
        Core.toBS loggerDefinitionId
      ]

instance Core.ToQuery GetLoggerDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLoggerDefinitionResponse' smart constructor.
data GetLoggerDefinitionResponse = GetLoggerDefinitionResponse'
  { -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggerDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestVersionArn', 'getLoggerDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'arn', 'getLoggerDefinitionResponse_arn' - The ARN of the definition.
--
-- 'name', 'getLoggerDefinitionResponse_name' - The name of the definition.
--
-- 'creationTimestamp', 'getLoggerDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'id', 'getLoggerDefinitionResponse_id' - The ID of the definition.
--
-- 'latestVersion', 'getLoggerDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'lastUpdatedTimestamp', 'getLoggerDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getLoggerDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'httpStatus', 'getLoggerDefinitionResponse_httpStatus' - The response's http status code.
newGetLoggerDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLoggerDefinitionResponse
newGetLoggerDefinitionResponse pHttpStatus_ =
  GetLoggerDefinitionResponse'
    { latestVersionArn =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the latest version associated with the definition.
getLoggerDefinitionResponse_latestVersionArn :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_latestVersionArn = Lens.lens (\GetLoggerDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetLoggerDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetLoggerDefinitionResponse)

-- | The ARN of the definition.
getLoggerDefinitionResponse_arn :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_arn = Lens.lens (\GetLoggerDefinitionResponse' {arn} -> arn) (\s@GetLoggerDefinitionResponse' {} a -> s {arn = a} :: GetLoggerDefinitionResponse)

-- | The name of the definition.
getLoggerDefinitionResponse_name :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_name = Lens.lens (\GetLoggerDefinitionResponse' {name} -> name) (\s@GetLoggerDefinitionResponse' {} a -> s {name = a} :: GetLoggerDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
getLoggerDefinitionResponse_creationTimestamp :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_creationTimestamp = Lens.lens (\GetLoggerDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetLoggerDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetLoggerDefinitionResponse)

-- | The ID of the definition.
getLoggerDefinitionResponse_id :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_id = Lens.lens (\GetLoggerDefinitionResponse' {id} -> id) (\s@GetLoggerDefinitionResponse' {} a -> s {id = a} :: GetLoggerDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getLoggerDefinitionResponse_latestVersion :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_latestVersion = Lens.lens (\GetLoggerDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetLoggerDefinitionResponse' {} a -> s {latestVersion = a} :: GetLoggerDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getLoggerDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetLoggerDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetLoggerDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetLoggerDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getLoggerDefinitionResponse_tags :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getLoggerDefinitionResponse_tags = Lens.lens (\GetLoggerDefinitionResponse' {tags} -> tags) (\s@GetLoggerDefinitionResponse' {} a -> s {tags = a} :: GetLoggerDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLoggerDefinitionResponse_httpStatus :: Lens.Lens' GetLoggerDefinitionResponse Prelude.Int
getLoggerDefinitionResponse_httpStatus = Lens.lens (\GetLoggerDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetLoggerDefinitionResponse' {} a -> s {httpStatus = a} :: GetLoggerDefinitionResponse)

instance Prelude.NFData GetLoggerDefinitionResponse
