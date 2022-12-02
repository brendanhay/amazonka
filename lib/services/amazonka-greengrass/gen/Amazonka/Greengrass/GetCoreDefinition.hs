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
-- Module      : Amazonka.Greengrass.GetCoreDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a core definition version.
module Amazonka.Greengrass.GetCoreDefinition
  ( -- * Creating a Request
    GetCoreDefinition (..),
    newGetCoreDefinition,

    -- * Request Lenses
    getCoreDefinition_coreDefinitionId,

    -- * Destructuring the Response
    GetCoreDefinitionResponse (..),
    newGetCoreDefinitionResponse,

    -- * Response Lenses
    getCoreDefinitionResponse_lastUpdatedTimestamp,
    getCoreDefinitionResponse_tags,
    getCoreDefinitionResponse_name,
    getCoreDefinitionResponse_arn,
    getCoreDefinitionResponse_latestVersion,
    getCoreDefinitionResponse_id,
    getCoreDefinitionResponse_creationTimestamp,
    getCoreDefinitionResponse_latestVersionArn,
    getCoreDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCoreDefinition' smart constructor.
data GetCoreDefinition = GetCoreDefinition'
  { -- | The ID of the core definition.
    coreDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoreDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreDefinitionId', 'getCoreDefinition_coreDefinitionId' - The ID of the core definition.
newGetCoreDefinition ::
  -- | 'coreDefinitionId'
  Prelude.Text ->
  GetCoreDefinition
newGetCoreDefinition pCoreDefinitionId_ =
  GetCoreDefinition'
    { coreDefinitionId =
        pCoreDefinitionId_
    }

-- | The ID of the core definition.
getCoreDefinition_coreDefinitionId :: Lens.Lens' GetCoreDefinition Prelude.Text
getCoreDefinition_coreDefinitionId = Lens.lens (\GetCoreDefinition' {coreDefinitionId} -> coreDefinitionId) (\s@GetCoreDefinition' {} a -> s {coreDefinitionId = a} :: GetCoreDefinition)

instance Core.AWSRequest GetCoreDefinition where
  type
    AWSResponse GetCoreDefinition =
      GetCoreDefinitionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoreDefinitionResponse'
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

instance Prelude.Hashable GetCoreDefinition where
  hashWithSalt _salt GetCoreDefinition' {..} =
    _salt `Prelude.hashWithSalt` coreDefinitionId

instance Prelude.NFData GetCoreDefinition where
  rnf GetCoreDefinition' {..} =
    Prelude.rnf coreDefinitionId

instance Data.ToHeaders GetCoreDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCoreDefinition where
  toPath GetCoreDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/cores/",
        Data.toBS coreDefinitionId
      ]

instance Data.ToQuery GetCoreDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCoreDefinitionResponse' smart constructor.
data GetCoreDefinitionResponse = GetCoreDefinitionResponse'
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
-- Create a value of 'GetCoreDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimestamp', 'getCoreDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getCoreDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'name', 'getCoreDefinitionResponse_name' - The name of the definition.
--
-- 'arn', 'getCoreDefinitionResponse_arn' - The ARN of the definition.
--
-- 'latestVersion', 'getCoreDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'id', 'getCoreDefinitionResponse_id' - The ID of the definition.
--
-- 'creationTimestamp', 'getCoreDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getCoreDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'httpStatus', 'getCoreDefinitionResponse_httpStatus' - The response's http status code.
newGetCoreDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCoreDefinitionResponse
newGetCoreDefinitionResponse pHttpStatus_ =
  GetCoreDefinitionResponse'
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
getCoreDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetCoreDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetCoreDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetCoreDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getCoreDefinitionResponse_tags :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getCoreDefinitionResponse_tags = Lens.lens (\GetCoreDefinitionResponse' {tags} -> tags) (\s@GetCoreDefinitionResponse' {} a -> s {tags = a} :: GetCoreDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the definition.
getCoreDefinitionResponse_name :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_name = Lens.lens (\GetCoreDefinitionResponse' {name} -> name) (\s@GetCoreDefinitionResponse' {} a -> s {name = a} :: GetCoreDefinitionResponse)

-- | The ARN of the definition.
getCoreDefinitionResponse_arn :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_arn = Lens.lens (\GetCoreDefinitionResponse' {arn} -> arn) (\s@GetCoreDefinitionResponse' {} a -> s {arn = a} :: GetCoreDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getCoreDefinitionResponse_latestVersion :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_latestVersion = Lens.lens (\GetCoreDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetCoreDefinitionResponse' {} a -> s {latestVersion = a} :: GetCoreDefinitionResponse)

-- | The ID of the definition.
getCoreDefinitionResponse_id :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_id = Lens.lens (\GetCoreDefinitionResponse' {id} -> id) (\s@GetCoreDefinitionResponse' {} a -> s {id = a} :: GetCoreDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
getCoreDefinitionResponse_creationTimestamp :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_creationTimestamp = Lens.lens (\GetCoreDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetCoreDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetCoreDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getCoreDefinitionResponse_latestVersionArn :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_latestVersionArn = Lens.lens (\GetCoreDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetCoreDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetCoreDefinitionResponse)

-- | The response's http status code.
getCoreDefinitionResponse_httpStatus :: Lens.Lens' GetCoreDefinitionResponse Prelude.Int
getCoreDefinitionResponse_httpStatus = Lens.lens (\GetCoreDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetCoreDefinitionResponse' {} a -> s {httpStatus = a} :: GetCoreDefinitionResponse)

instance Prelude.NFData GetCoreDefinitionResponse where
  rnf GetCoreDefinitionResponse' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf latestVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
