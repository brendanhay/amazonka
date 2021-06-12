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
-- Module      : Network.AWS.Greengrass.GetResourceDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a resource definition version, including
-- which resources are included in the version.
module Network.AWS.Greengrass.GetResourceDefinitionVersion
  ( -- * Creating a Request
    GetResourceDefinitionVersion (..),
    newGetResourceDefinitionVersion,

    -- * Request Lenses
    getResourceDefinitionVersion_resourceDefinitionVersionId,
    getResourceDefinitionVersion_resourceDefinitionId,

    -- * Destructuring the Response
    GetResourceDefinitionVersionResponse (..),
    newGetResourceDefinitionVersionResponse,

    -- * Response Lenses
    getResourceDefinitionVersionResponse_creationTimestamp,
    getResourceDefinitionVersionResponse_arn,
    getResourceDefinitionVersionResponse_id,
    getResourceDefinitionVersionResponse_version,
    getResourceDefinitionVersionResponse_definition,
    getResourceDefinitionVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetResourceDefinitionVersion' smart constructor.
data GetResourceDefinitionVersion = GetResourceDefinitionVersion'
  { -- | The ID of the resource definition version. This value maps to the
    -- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
    -- object, which is returned by \'\'ListResourceDefinitionVersions\'\'
    -- requests. If the version is the last one that was associated with a
    -- resource definition, the value also maps to the \'\'LatestVersion\'\'
    -- property of the corresponding \'\'DefinitionInformation\'\' object.
    resourceDefinitionVersionId :: Core.Text,
    -- | The ID of the resource definition.
    resourceDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetResourceDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceDefinitionVersionId', 'getResourceDefinitionVersion_resourceDefinitionVersionId' - The ID of the resource definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListResourceDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- resource definition, the value also maps to the \'\'LatestVersion\'\'
-- property of the corresponding \'\'DefinitionInformation\'\' object.
--
-- 'resourceDefinitionId', 'getResourceDefinitionVersion_resourceDefinitionId' - The ID of the resource definition.
newGetResourceDefinitionVersion ::
  -- | 'resourceDefinitionVersionId'
  Core.Text ->
  -- | 'resourceDefinitionId'
  Core.Text ->
  GetResourceDefinitionVersion
newGetResourceDefinitionVersion
  pResourceDefinitionVersionId_
  pResourceDefinitionId_ =
    GetResourceDefinitionVersion'
      { resourceDefinitionVersionId =
          pResourceDefinitionVersionId_,
        resourceDefinitionId = pResourceDefinitionId_
      }

-- | The ID of the resource definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListResourceDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- resource definition, the value also maps to the \'\'LatestVersion\'\'
-- property of the corresponding \'\'DefinitionInformation\'\' object.
getResourceDefinitionVersion_resourceDefinitionVersionId :: Lens.Lens' GetResourceDefinitionVersion Core.Text
getResourceDefinitionVersion_resourceDefinitionVersionId = Lens.lens (\GetResourceDefinitionVersion' {resourceDefinitionVersionId} -> resourceDefinitionVersionId) (\s@GetResourceDefinitionVersion' {} a -> s {resourceDefinitionVersionId = a} :: GetResourceDefinitionVersion)

-- | The ID of the resource definition.
getResourceDefinitionVersion_resourceDefinitionId :: Lens.Lens' GetResourceDefinitionVersion Core.Text
getResourceDefinitionVersion_resourceDefinitionId = Lens.lens (\GetResourceDefinitionVersion' {resourceDefinitionId} -> resourceDefinitionId) (\s@GetResourceDefinitionVersion' {} a -> s {resourceDefinitionId = a} :: GetResourceDefinitionVersion)

instance Core.AWSRequest GetResourceDefinitionVersion where
  type
    AWSResponse GetResourceDefinitionVersion =
      GetResourceDefinitionVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceDefinitionVersionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Version")
            Core.<*> (x Core..?> "Definition")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetResourceDefinitionVersion

instance Core.NFData GetResourceDefinitionVersion

instance Core.ToHeaders GetResourceDefinitionVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetResourceDefinitionVersion where
  toPath GetResourceDefinitionVersion' {..} =
    Core.mconcat
      [ "/greengrass/definition/resources/",
        Core.toBS resourceDefinitionId,
        "/versions/",
        Core.toBS resourceDefinitionVersionId
      ]

instance Core.ToQuery GetResourceDefinitionVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetResourceDefinitionVersionResponse' smart constructor.
data GetResourceDefinitionVersionResponse = GetResourceDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the resource definition
    -- version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | Arn of the resource definition version.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the resource definition version.
    id :: Core.Maybe Core.Text,
    -- | The version of the resource definition version.
    version :: Core.Maybe Core.Text,
    -- | Information about the definition.
    definition :: Core.Maybe ResourceDefinitionVersion,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetResourceDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getResourceDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the resource definition
-- version was created.
--
-- 'arn', 'getResourceDefinitionVersionResponse_arn' - Arn of the resource definition version.
--
-- 'id', 'getResourceDefinitionVersionResponse_id' - The ID of the resource definition version.
--
-- 'version', 'getResourceDefinitionVersionResponse_version' - The version of the resource definition version.
--
-- 'definition', 'getResourceDefinitionVersionResponse_definition' - Information about the definition.
--
-- 'httpStatus', 'getResourceDefinitionVersionResponse_httpStatus' - The response's http status code.
newGetResourceDefinitionVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetResourceDefinitionVersionResponse
newGetResourceDefinitionVersionResponse pHttpStatus_ =
  GetResourceDefinitionVersionResponse'
    { creationTimestamp =
        Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      definition = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the resource definition
-- version was created.
getResourceDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetResourceDefinitionVersionResponse (Core.Maybe Core.Text)
getResourceDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetResourceDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetResourceDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetResourceDefinitionVersionResponse)

-- | Arn of the resource definition version.
getResourceDefinitionVersionResponse_arn :: Lens.Lens' GetResourceDefinitionVersionResponse (Core.Maybe Core.Text)
getResourceDefinitionVersionResponse_arn = Lens.lens (\GetResourceDefinitionVersionResponse' {arn} -> arn) (\s@GetResourceDefinitionVersionResponse' {} a -> s {arn = a} :: GetResourceDefinitionVersionResponse)

-- | The ID of the resource definition version.
getResourceDefinitionVersionResponse_id :: Lens.Lens' GetResourceDefinitionVersionResponse (Core.Maybe Core.Text)
getResourceDefinitionVersionResponse_id = Lens.lens (\GetResourceDefinitionVersionResponse' {id} -> id) (\s@GetResourceDefinitionVersionResponse' {} a -> s {id = a} :: GetResourceDefinitionVersionResponse)

-- | The version of the resource definition version.
getResourceDefinitionVersionResponse_version :: Lens.Lens' GetResourceDefinitionVersionResponse (Core.Maybe Core.Text)
getResourceDefinitionVersionResponse_version = Lens.lens (\GetResourceDefinitionVersionResponse' {version} -> version) (\s@GetResourceDefinitionVersionResponse' {} a -> s {version = a} :: GetResourceDefinitionVersionResponse)

-- | Information about the definition.
getResourceDefinitionVersionResponse_definition :: Lens.Lens' GetResourceDefinitionVersionResponse (Core.Maybe ResourceDefinitionVersion)
getResourceDefinitionVersionResponse_definition = Lens.lens (\GetResourceDefinitionVersionResponse' {definition} -> definition) (\s@GetResourceDefinitionVersionResponse' {} a -> s {definition = a} :: GetResourceDefinitionVersionResponse)

-- | The response's http status code.
getResourceDefinitionVersionResponse_httpStatus :: Lens.Lens' GetResourceDefinitionVersionResponse Core.Int
getResourceDefinitionVersionResponse_httpStatus = Lens.lens (\GetResourceDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@GetResourceDefinitionVersionResponse' {} a -> s {httpStatus = a} :: GetResourceDefinitionVersionResponse)

instance
  Core.NFData
    GetResourceDefinitionVersionResponse
