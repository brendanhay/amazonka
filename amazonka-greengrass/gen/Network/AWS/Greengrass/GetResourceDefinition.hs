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
-- Module      : Network.AWS.Greengrass.GetResourceDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a resource definition, including its
-- creation time and latest version.
module Network.AWS.Greengrass.GetResourceDefinition
  ( -- * Creating a Request
    GetResourceDefinition (..),
    newGetResourceDefinition,

    -- * Request Lenses
    getResourceDefinition_resourceDefinitionId,

    -- * Destructuring the Response
    GetResourceDefinitionResponse (..),
    newGetResourceDefinitionResponse,

    -- * Response Lenses
    getResourceDefinitionResponse_creationTimestamp,
    getResourceDefinitionResponse_latestVersionArn,
    getResourceDefinitionResponse_latestVersion,
    getResourceDefinitionResponse_arn,
    getResourceDefinitionResponse_id,
    getResourceDefinitionResponse_name,
    getResourceDefinitionResponse_lastUpdatedTimestamp,
    getResourceDefinitionResponse_tags,
    getResourceDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceDefinitionResponse'
            Prelude.<$> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "LatestVersionArn")
            Prelude.<*> (x Core..?> "LatestVersion")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceDefinition

instance Prelude.NFData GetResourceDefinition

instance Core.ToHeaders GetResourceDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetResourceDefinition where
  toPath GetResourceDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/resources/",
        Core.toBS resourceDefinitionId
      ]

instance Core.ToQuery GetResourceDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceDefinitionResponse' smart constructor.
data GetResourceDefinitionResponse = GetResourceDefinitionResponse'
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
-- Create a value of 'GetResourceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getResourceDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getResourceDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'getResourceDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'getResourceDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'getResourceDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'getResourceDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'getResourceDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getResourceDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'httpStatus', 'getResourceDefinitionResponse_httpStatus' - The response's http status code.
newGetResourceDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceDefinitionResponse
newGetResourceDefinitionResponse pHttpStatus_ =
  GetResourceDefinitionResponse'
    { creationTimestamp =
        Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
getResourceDefinitionResponse_creationTimestamp :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_creationTimestamp = Lens.lens (\GetResourceDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetResourceDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetResourceDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getResourceDefinitionResponse_latestVersionArn :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_latestVersionArn = Lens.lens (\GetResourceDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetResourceDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetResourceDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getResourceDefinitionResponse_latestVersion :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_latestVersion = Lens.lens (\GetResourceDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetResourceDefinitionResponse' {} a -> s {latestVersion = a} :: GetResourceDefinitionResponse)

-- | The ARN of the definition.
getResourceDefinitionResponse_arn :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_arn = Lens.lens (\GetResourceDefinitionResponse' {arn} -> arn) (\s@GetResourceDefinitionResponse' {} a -> s {arn = a} :: GetResourceDefinitionResponse)

-- | The ID of the definition.
getResourceDefinitionResponse_id :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_id = Lens.lens (\GetResourceDefinitionResponse' {id} -> id) (\s@GetResourceDefinitionResponse' {} a -> s {id = a} :: GetResourceDefinitionResponse)

-- | The name of the definition.
getResourceDefinitionResponse_name :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_name = Lens.lens (\GetResourceDefinitionResponse' {name} -> name) (\s@GetResourceDefinitionResponse' {} a -> s {name = a} :: GetResourceDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getResourceDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetResourceDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetResourceDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetResourceDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getResourceDefinitionResponse_tags :: Lens.Lens' GetResourceDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getResourceDefinitionResponse_tags = Lens.lens (\GetResourceDefinitionResponse' {tags} -> tags) (\s@GetResourceDefinitionResponse' {} a -> s {tags = a} :: GetResourceDefinitionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getResourceDefinitionResponse_httpStatus :: Lens.Lens' GetResourceDefinitionResponse Prelude.Int
getResourceDefinitionResponse_httpStatus = Lens.lens (\GetResourceDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetResourceDefinitionResponse' {} a -> s {httpStatus = a} :: GetResourceDefinitionResponse)

instance Prelude.NFData GetResourceDefinitionResponse
