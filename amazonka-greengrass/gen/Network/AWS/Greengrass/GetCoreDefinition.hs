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
-- Module      : Network.AWS.Greengrass.GetCoreDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a core definition version.
module Network.AWS.Greengrass.GetCoreDefinition
  ( -- * Creating a Request
    GetCoreDefinition (..),
    newGetCoreDefinition,

    -- * Request Lenses
    getCoreDefinition_coreDefinitionId,

    -- * Destructuring the Response
    GetCoreDefinitionResponse (..),
    newGetCoreDefinitionResponse,

    -- * Response Lenses
    getCoreDefinitionResponse_creationTimestamp,
    getCoreDefinitionResponse_latestVersionArn,
    getCoreDefinitionResponse_latestVersion,
    getCoreDefinitionResponse_arn,
    getCoreDefinitionResponse_id,
    getCoreDefinitionResponse_name,
    getCoreDefinitionResponse_lastUpdatedTimestamp,
    getCoreDefinitionResponse_tags,
    getCoreDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoreDefinitionResponse'
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

instance Prelude.Hashable GetCoreDefinition

instance Prelude.NFData GetCoreDefinition

instance Core.ToHeaders GetCoreDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetCoreDefinition where
  toPath GetCoreDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/cores/",
        Core.toBS coreDefinitionId
      ]

instance Core.ToQuery GetCoreDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCoreDefinitionResponse' smart constructor.
data GetCoreDefinitionResponse = GetCoreDefinitionResponse'
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
-- Create a value of 'GetCoreDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getCoreDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getCoreDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'getCoreDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'getCoreDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'getCoreDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'getCoreDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'getCoreDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getCoreDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'httpStatus', 'getCoreDefinitionResponse_httpStatus' - The response's http status code.
newGetCoreDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCoreDefinitionResponse
newGetCoreDefinitionResponse pHttpStatus_ =
  GetCoreDefinitionResponse'
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
getCoreDefinitionResponse_creationTimestamp :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_creationTimestamp = Lens.lens (\GetCoreDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetCoreDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetCoreDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getCoreDefinitionResponse_latestVersionArn :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_latestVersionArn = Lens.lens (\GetCoreDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetCoreDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetCoreDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getCoreDefinitionResponse_latestVersion :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_latestVersion = Lens.lens (\GetCoreDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetCoreDefinitionResponse' {} a -> s {latestVersion = a} :: GetCoreDefinitionResponse)

-- | The ARN of the definition.
getCoreDefinitionResponse_arn :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_arn = Lens.lens (\GetCoreDefinitionResponse' {arn} -> arn) (\s@GetCoreDefinitionResponse' {} a -> s {arn = a} :: GetCoreDefinitionResponse)

-- | The ID of the definition.
getCoreDefinitionResponse_id :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_id = Lens.lens (\GetCoreDefinitionResponse' {id} -> id) (\s@GetCoreDefinitionResponse' {} a -> s {id = a} :: GetCoreDefinitionResponse)

-- | The name of the definition.
getCoreDefinitionResponse_name :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_name = Lens.lens (\GetCoreDefinitionResponse' {name} -> name) (\s@GetCoreDefinitionResponse' {} a -> s {name = a} :: GetCoreDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getCoreDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetCoreDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetCoreDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetCoreDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getCoreDefinitionResponse_tags :: Lens.Lens' GetCoreDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getCoreDefinitionResponse_tags = Lens.lens (\GetCoreDefinitionResponse' {tags} -> tags) (\s@GetCoreDefinitionResponse' {} a -> s {tags = a} :: GetCoreDefinitionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getCoreDefinitionResponse_httpStatus :: Lens.Lens' GetCoreDefinitionResponse Prelude.Int
getCoreDefinitionResponse_httpStatus = Lens.lens (\GetCoreDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetCoreDefinitionResponse' {} a -> s {httpStatus = a} :: GetCoreDefinitionResponse)

instance Prelude.NFData GetCoreDefinitionResponse
