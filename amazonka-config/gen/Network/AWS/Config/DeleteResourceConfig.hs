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
-- Module      : Network.AWS.Config.DeleteResourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records the configuration state for a custom resource that has been
-- deleted. This API records a new ConfigurationItem with a ResourceDeleted
-- status. You can retrieve the ConfigurationItems recorded for this
-- resource in your AWS Config History.
module Network.AWS.Config.DeleteResourceConfig
  ( -- * Creating a Request
    DeleteResourceConfig (..),
    newDeleteResourceConfig,

    -- * Request Lenses
    deleteResourceConfig_resourceType,
    deleteResourceConfig_resourceId,

    -- * Destructuring the Response
    DeleteResourceConfigResponse (..),
    newDeleteResourceConfigResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteResourceConfig' smart constructor.
data DeleteResourceConfig = DeleteResourceConfig'
  { -- | The type of the resource.
    resourceType :: Core.Text,
    -- | Unique identifier of the resource.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'deleteResourceConfig_resourceType' - The type of the resource.
--
-- 'resourceId', 'deleteResourceConfig_resourceId' - Unique identifier of the resource.
newDeleteResourceConfig ::
  -- | 'resourceType'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  DeleteResourceConfig
newDeleteResourceConfig pResourceType_ pResourceId_ =
  DeleteResourceConfig'
    { resourceType =
        pResourceType_,
      resourceId = pResourceId_
    }

-- | The type of the resource.
deleteResourceConfig_resourceType :: Lens.Lens' DeleteResourceConfig Core.Text
deleteResourceConfig_resourceType = Lens.lens (\DeleteResourceConfig' {resourceType} -> resourceType) (\s@DeleteResourceConfig' {} a -> s {resourceType = a} :: DeleteResourceConfig)

-- | Unique identifier of the resource.
deleteResourceConfig_resourceId :: Lens.Lens' DeleteResourceConfig Core.Text
deleteResourceConfig_resourceId = Lens.lens (\DeleteResourceConfig' {resourceId} -> resourceId) (\s@DeleteResourceConfig' {} a -> s {resourceId = a} :: DeleteResourceConfig)

instance Core.AWSRequest DeleteResourceConfig where
  type
    AWSResponse DeleteResourceConfig =
      DeleteResourceConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteResourceConfigResponse'

instance Core.Hashable DeleteResourceConfig

instance Core.NFData DeleteResourceConfig

instance Core.ToHeaders DeleteResourceConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeleteResourceConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteResourceConfig where
  toJSON DeleteResourceConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceType" Core..= resourceType),
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath DeleteResourceConfig where
  toPath = Core.const "/"

instance Core.ToQuery DeleteResourceConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteResourceConfigResponse' smart constructor.
data DeleteResourceConfigResponse = DeleteResourceConfigResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourceConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteResourceConfigResponse ::
  DeleteResourceConfigResponse
newDeleteResourceConfigResponse =
  DeleteResourceConfigResponse'

instance Core.NFData DeleteResourceConfigResponse
