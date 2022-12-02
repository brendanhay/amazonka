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
-- Module      : Amazonka.Greengrass.GetResourceDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a resource definition, including its
-- creation time and latest version.
module Amazonka.Greengrass.GetResourceDefinition
  ( -- * Creating a Request
    GetResourceDefinition (..),
    newGetResourceDefinition,

    -- * Request Lenses
    getResourceDefinition_resourceDefinitionId,

    -- * Destructuring the Response
    GetResourceDefinitionResponse (..),
    newGetResourceDefinitionResponse,

    -- * Response Lenses
    getResourceDefinitionResponse_lastUpdatedTimestamp,
    getResourceDefinitionResponse_tags,
    getResourceDefinitionResponse_name,
    getResourceDefinitionResponse_arn,
    getResourceDefinitionResponse_latestVersion,
    getResourceDefinitionResponse_id,
    getResourceDefinitionResponse_creationTimestamp,
    getResourceDefinitionResponse_latestVersionArn,
    getResourceDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceDefinition' smart constructor.
data GetResourceDefinition = GetResourceDefinition'
  { -- | The ID of the resource definition.
    resourceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceDefinitionId', 'getResourceDefinition_resourceDefinitionId' - The ID of the resource definition.
newGetResourceDefinition ::
  -- | 'resourceDefinitionId'
  Prelude.Text ->
  GetResourceDefinition
newGetResourceDefinition pResourceDefinitionId_ =
  GetResourceDefinition'
    { resourceDefinitionId =
        pResourceDefinitionId_
    }

-- | The ID of the resource definition.
getResourceDefinition_resourceDefinitionId :: Lens.Lens' GetResourceDefinition Prelude.Text
getResourceDefinition_resourceDefinitionId = Lens.lens (\GetResourceDefinition' {resourceDefinitionId} -> resourceDefinitionId) (\s@GetResourceDefinition' {} a -> s {resourceDefinitionId = a} :: GetResourceDefinition)

instance Core.AWSRequest GetResourceDefinition where
  type
    AWSResponse GetResourceDefinition =
      GetResourceDefinitionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceDefinitionResponse'
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

instance Prelude.Hashable GetResourceDefinition where
  hashWithSalt _salt GetResourceDefinition' {..} =
    _salt `Prelude.hashWithSalt` resourceDefinitionId

instance Prelude.NFData GetResourceDefinition where
  rnf GetResourceDefinition' {..} =
    Prelude.rnf resourceDefinitionId

instance Data.ToHeaders GetResourceDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetResourceDefinition where
  toPath GetResourceDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/resources/",
        Data.toBS resourceDefinitionId
      ]

instance Data.ToQuery GetResourceDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceDefinitionResponse' smart constructor.
data GetResourceDefinitionResponse = GetResourceDefinitionResponse'
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
-- Create a value of 'GetResourceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimestamp', 'getResourceDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getResourceDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'name', 'getResourceDefinitionResponse_name' - The name of the definition.
--
-- 'arn', 'getResourceDefinitionResponse_arn' - The ARN of the definition.
--
-- 'latestVersion', 'getResourceDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'id', 'getResourceDefinitionResponse_id' - The ID of the definition.
--
-- 'creationTimestamp', 'getResourceDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getResourceDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'httpStatus', 'getResourceDefinitionResponse_httpStatus' - The response's http status code.
newGetResourceDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceDefinitionResponse
newGetResourceDefinitionResponse pHttpStatus_ =
  GetResourceDefinitionResponse'
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
getResourceDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetResourceDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetResourceDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetResourceDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getResourceDefinitionResponse_tags :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getResourceDefinitionResponse_tags = Lens.lens (\GetResourceDefinitionResponse' {tags} -> tags) (\s@GetResourceDefinitionResponse' {} a -> s {tags = a} :: GetResourceDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the definition.
getResourceDefinitionResponse_name :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_name = Lens.lens (\GetResourceDefinitionResponse' {name} -> name) (\s@GetResourceDefinitionResponse' {} a -> s {name = a} :: GetResourceDefinitionResponse)

-- | The ARN of the definition.
getResourceDefinitionResponse_arn :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_arn = Lens.lens (\GetResourceDefinitionResponse' {arn} -> arn) (\s@GetResourceDefinitionResponse' {} a -> s {arn = a} :: GetResourceDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getResourceDefinitionResponse_latestVersion :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_latestVersion = Lens.lens (\GetResourceDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetResourceDefinitionResponse' {} a -> s {latestVersion = a} :: GetResourceDefinitionResponse)

-- | The ID of the definition.
getResourceDefinitionResponse_id :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_id = Lens.lens (\GetResourceDefinitionResponse' {id} -> id) (\s@GetResourceDefinitionResponse' {} a -> s {id = a} :: GetResourceDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
getResourceDefinitionResponse_creationTimestamp :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_creationTimestamp = Lens.lens (\GetResourceDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetResourceDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetResourceDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getResourceDefinitionResponse_latestVersionArn :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_latestVersionArn = Lens.lens (\GetResourceDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetResourceDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetResourceDefinitionResponse)

-- | The response's http status code.
getResourceDefinitionResponse_httpStatus :: Lens.Lens' GetResourceDefinitionResponse Prelude.Int
getResourceDefinitionResponse_httpStatus = Lens.lens (\GetResourceDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetResourceDefinitionResponse' {} a -> s {httpStatus = a} :: GetResourceDefinitionResponse)

instance Prelude.NFData GetResourceDefinitionResponse where
  rnf GetResourceDefinitionResponse' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf latestVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
