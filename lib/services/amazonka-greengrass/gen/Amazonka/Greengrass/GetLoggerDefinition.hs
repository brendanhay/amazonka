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
-- Module      : Amazonka.Greengrass.GetLoggerDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a logger definition.
module Amazonka.Greengrass.GetLoggerDefinition
  ( -- * Creating a Request
    GetLoggerDefinition (..),
    newGetLoggerDefinition,

    -- * Request Lenses
    getLoggerDefinition_loggerDefinitionId,

    -- * Destructuring the Response
    GetLoggerDefinitionResponse (..),
    newGetLoggerDefinitionResponse,

    -- * Response Lenses
    getLoggerDefinitionResponse_arn,
    getLoggerDefinitionResponse_creationTimestamp,
    getLoggerDefinitionResponse_id,
    getLoggerDefinitionResponse_lastUpdatedTimestamp,
    getLoggerDefinitionResponse_latestVersion,
    getLoggerDefinitionResponse_latestVersionArn,
    getLoggerDefinitionResponse_name,
    getLoggerDefinitionResponse_tags,
    getLoggerDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggerDefinitionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Data..?> "LatestVersion")
            Prelude.<*> (x Data..?> "LatestVersionArn")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLoggerDefinition where
  hashWithSalt _salt GetLoggerDefinition' {..} =
    _salt `Prelude.hashWithSalt` loggerDefinitionId

instance Prelude.NFData GetLoggerDefinition where
  rnf GetLoggerDefinition' {..} =
    Prelude.rnf loggerDefinitionId

instance Data.ToHeaders GetLoggerDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetLoggerDefinition where
  toPath GetLoggerDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/loggers/",
        Data.toBS loggerDefinitionId
      ]

instance Data.ToQuery GetLoggerDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLoggerDefinitionResponse' smart constructor.
data GetLoggerDefinitionResponse = GetLoggerDefinitionResponse'
  { -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
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
-- 'arn', 'getLoggerDefinitionResponse_arn' - The ARN of the definition.
--
-- 'creationTimestamp', 'getLoggerDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'id', 'getLoggerDefinitionResponse_id' - The ID of the definition.
--
-- 'lastUpdatedTimestamp', 'getLoggerDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'latestVersion', 'getLoggerDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'latestVersionArn', 'getLoggerDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'name', 'getLoggerDefinitionResponse_name' - The name of the definition.
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
    { arn = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the definition.
getLoggerDefinitionResponse_arn :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_arn = Lens.lens (\GetLoggerDefinitionResponse' {arn} -> arn) (\s@GetLoggerDefinitionResponse' {} a -> s {arn = a} :: GetLoggerDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
getLoggerDefinitionResponse_creationTimestamp :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_creationTimestamp = Lens.lens (\GetLoggerDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetLoggerDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetLoggerDefinitionResponse)

-- | The ID of the definition.
getLoggerDefinitionResponse_id :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_id = Lens.lens (\GetLoggerDefinitionResponse' {id} -> id) (\s@GetLoggerDefinitionResponse' {} a -> s {id = a} :: GetLoggerDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getLoggerDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetLoggerDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetLoggerDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetLoggerDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getLoggerDefinitionResponse_latestVersion :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_latestVersion = Lens.lens (\GetLoggerDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetLoggerDefinitionResponse' {} a -> s {latestVersion = a} :: GetLoggerDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getLoggerDefinitionResponse_latestVersionArn :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_latestVersionArn = Lens.lens (\GetLoggerDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetLoggerDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetLoggerDefinitionResponse)

-- | The name of the definition.
getLoggerDefinitionResponse_name :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionResponse_name = Lens.lens (\GetLoggerDefinitionResponse' {name} -> name) (\s@GetLoggerDefinitionResponse' {} a -> s {name = a} :: GetLoggerDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getLoggerDefinitionResponse_tags :: Lens.Lens' GetLoggerDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getLoggerDefinitionResponse_tags = Lens.lens (\GetLoggerDefinitionResponse' {tags} -> tags) (\s@GetLoggerDefinitionResponse' {} a -> s {tags = a} :: GetLoggerDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLoggerDefinitionResponse_httpStatus :: Lens.Lens' GetLoggerDefinitionResponse Prelude.Int
getLoggerDefinitionResponse_httpStatus = Lens.lens (\GetLoggerDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetLoggerDefinitionResponse' {} a -> s {httpStatus = a} :: GetLoggerDefinitionResponse)

instance Prelude.NFData GetLoggerDefinitionResponse where
  rnf GetLoggerDefinitionResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTimestamp `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf lastUpdatedTimestamp `Prelude.seq`
            Prelude.rnf latestVersion `Prelude.seq`
              Prelude.rnf latestVersionArn `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf tags `Prelude.seq`
                    Prelude.rnf httpStatus
