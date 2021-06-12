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
-- Module      : Network.AWS.Config.PutResourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records the configuration state for the resource provided in the
-- request. The configuration state of a resource is represented in AWS
-- Config as Configuration Items. Once this API records the configuration
-- item, you can retrieve the list of configuration items for the custom
-- resource type using existing AWS Config APIs.
--
-- The custom resource type must be registered with AWS CloudFormation.
-- This API accepts the configuration item registered with AWS
-- CloudFormation.
--
-- When you call this API, AWS Config only stores configuration state of
-- the resource provided in the request. This API does not change or
-- remediate the configuration of the resource.
--
-- Write-only schema properites are not recorded as part of the published
-- configuration item.
module Network.AWS.Config.PutResourceConfig
  ( -- * Creating a Request
    PutResourceConfig (..),
    newPutResourceConfig,

    -- * Request Lenses
    putResourceConfig_tags,
    putResourceConfig_resourceName,
    putResourceConfig_resourceType,
    putResourceConfig_schemaVersionId,
    putResourceConfig_resourceId,
    putResourceConfig_configuration,

    -- * Destructuring the Response
    PutResourceConfigResponse (..),
    newPutResourceConfigResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutResourceConfig' smart constructor.
data PutResourceConfig = PutResourceConfig'
  { -- | Tags associated with the resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Name of the resource.
    resourceName :: Core.Maybe Core.Text,
    -- | The type of the resource. The custom resource type must be registered
    -- with AWS CloudFormation.
    --
    -- You cannot use the organization names “aws”, “amzn”, “amazon”, “alexa”,
    -- “custom” with custom resource types. It is the first part of the
    -- ResourceType up to the first ::.
    resourceType :: Core.Text,
    -- | Version of the schema registered for the ResourceType in AWS
    -- CloudFormation.
    schemaVersionId :: Core.Text,
    -- | Unique identifier of the resource.
    resourceId :: Core.Text,
    -- | The configuration object of the resource in valid JSON format. It must
    -- match the schema registered with AWS CloudFormation.
    --
    -- The configuration JSON must not exceed 64 KB.
    configuration :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'putResourceConfig_tags' - Tags associated with the resource.
--
-- 'resourceName', 'putResourceConfig_resourceName' - Name of the resource.
--
-- 'resourceType', 'putResourceConfig_resourceType' - The type of the resource. The custom resource type must be registered
-- with AWS CloudFormation.
--
-- You cannot use the organization names “aws”, “amzn”, “amazon”, “alexa”,
-- “custom” with custom resource types. It is the first part of the
-- ResourceType up to the first ::.
--
-- 'schemaVersionId', 'putResourceConfig_schemaVersionId' - Version of the schema registered for the ResourceType in AWS
-- CloudFormation.
--
-- 'resourceId', 'putResourceConfig_resourceId' - Unique identifier of the resource.
--
-- 'configuration', 'putResourceConfig_configuration' - The configuration object of the resource in valid JSON format. It must
-- match the schema registered with AWS CloudFormation.
--
-- The configuration JSON must not exceed 64 KB.
newPutResourceConfig ::
  -- | 'resourceType'
  Core.Text ->
  -- | 'schemaVersionId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  -- | 'configuration'
  Core.Text ->
  PutResourceConfig
newPutResourceConfig
  pResourceType_
  pSchemaVersionId_
  pResourceId_
  pConfiguration_ =
    PutResourceConfig'
      { tags = Core.Nothing,
        resourceName = Core.Nothing,
        resourceType = pResourceType_,
        schemaVersionId = pSchemaVersionId_,
        resourceId = pResourceId_,
        configuration = pConfiguration_
      }

-- | Tags associated with the resource.
putResourceConfig_tags :: Lens.Lens' PutResourceConfig (Core.Maybe (Core.HashMap Core.Text Core.Text))
putResourceConfig_tags = Lens.lens (\PutResourceConfig' {tags} -> tags) (\s@PutResourceConfig' {} a -> s {tags = a} :: PutResourceConfig) Core.. Lens.mapping Lens._Coerce

-- | Name of the resource.
putResourceConfig_resourceName :: Lens.Lens' PutResourceConfig (Core.Maybe Core.Text)
putResourceConfig_resourceName = Lens.lens (\PutResourceConfig' {resourceName} -> resourceName) (\s@PutResourceConfig' {} a -> s {resourceName = a} :: PutResourceConfig)

-- | The type of the resource. The custom resource type must be registered
-- with AWS CloudFormation.
--
-- You cannot use the organization names “aws”, “amzn”, “amazon”, “alexa”,
-- “custom” with custom resource types. It is the first part of the
-- ResourceType up to the first ::.
putResourceConfig_resourceType :: Lens.Lens' PutResourceConfig Core.Text
putResourceConfig_resourceType = Lens.lens (\PutResourceConfig' {resourceType} -> resourceType) (\s@PutResourceConfig' {} a -> s {resourceType = a} :: PutResourceConfig)

-- | Version of the schema registered for the ResourceType in AWS
-- CloudFormation.
putResourceConfig_schemaVersionId :: Lens.Lens' PutResourceConfig Core.Text
putResourceConfig_schemaVersionId = Lens.lens (\PutResourceConfig' {schemaVersionId} -> schemaVersionId) (\s@PutResourceConfig' {} a -> s {schemaVersionId = a} :: PutResourceConfig)

-- | Unique identifier of the resource.
putResourceConfig_resourceId :: Lens.Lens' PutResourceConfig Core.Text
putResourceConfig_resourceId = Lens.lens (\PutResourceConfig' {resourceId} -> resourceId) (\s@PutResourceConfig' {} a -> s {resourceId = a} :: PutResourceConfig)

-- | The configuration object of the resource in valid JSON format. It must
-- match the schema registered with AWS CloudFormation.
--
-- The configuration JSON must not exceed 64 KB.
putResourceConfig_configuration :: Lens.Lens' PutResourceConfig Core.Text
putResourceConfig_configuration = Lens.lens (\PutResourceConfig' {configuration} -> configuration) (\s@PutResourceConfig' {} a -> s {configuration = a} :: PutResourceConfig)

instance Core.AWSRequest PutResourceConfig where
  type
    AWSResponse PutResourceConfig =
      PutResourceConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull PutResourceConfigResponse'

instance Core.Hashable PutResourceConfig

instance Core.NFData PutResourceConfig

instance Core.ToHeaders PutResourceConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.PutResourceConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutResourceConfig where
  toJSON PutResourceConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("ResourceName" Core..=) Core.<$> resourceName,
            Core.Just ("ResourceType" Core..= resourceType),
            Core.Just
              ("SchemaVersionId" Core..= schemaVersionId),
            Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("Configuration" Core..= configuration)
          ]
      )

instance Core.ToPath PutResourceConfig where
  toPath = Core.const "/"

instance Core.ToQuery PutResourceConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutResourceConfigResponse' smart constructor.
data PutResourceConfigResponse = PutResourceConfigResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutResourceConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutResourceConfigResponse ::
  PutResourceConfigResponse
newPutResourceConfigResponse =
  PutResourceConfigResponse'

instance Core.NFData PutResourceConfigResponse
