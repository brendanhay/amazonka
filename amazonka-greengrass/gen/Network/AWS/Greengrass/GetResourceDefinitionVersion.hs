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
import qualified Network.AWS.Prelude as Prelude
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
    resourceDefinitionVersionId :: Prelude.Text,
    -- | The ID of the resource definition.
    resourceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'resourceDefinitionId'
  Prelude.Text ->
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
getResourceDefinitionVersion_resourceDefinitionVersionId :: Lens.Lens' GetResourceDefinitionVersion Prelude.Text
getResourceDefinitionVersion_resourceDefinitionVersionId = Lens.lens (\GetResourceDefinitionVersion' {resourceDefinitionVersionId} -> resourceDefinitionVersionId) (\s@GetResourceDefinitionVersion' {} a -> s {resourceDefinitionVersionId = a} :: GetResourceDefinitionVersion)

-- | The ID of the resource definition.
getResourceDefinitionVersion_resourceDefinitionId :: Lens.Lens' GetResourceDefinitionVersion Prelude.Text
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
            Prelude.<$> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Version")
            Prelude.<*> (x Core..?> "Definition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetResourceDefinitionVersion

instance Prelude.NFData GetResourceDefinitionVersion

instance Core.ToHeaders GetResourceDefinitionVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetResourceDefinitionVersion where
  toPath GetResourceDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/resources/",
        Core.toBS resourceDefinitionId,
        "/versions/",
        Core.toBS resourceDefinitionVersionId
      ]

instance Core.ToQuery GetResourceDefinitionVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceDefinitionVersionResponse' smart constructor.
data GetResourceDefinitionVersionResponse = GetResourceDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the resource definition
    -- version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | Arn of the resource definition version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource definition version.
    id :: Prelude.Maybe Prelude.Text,
    -- | The version of the resource definition version.
    version :: Prelude.Maybe Prelude.Text,
    -- | Information about the definition.
    definition :: Prelude.Maybe ResourceDefinitionVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetResourceDefinitionVersionResponse
newGetResourceDefinitionVersionResponse pHttpStatus_ =
  GetResourceDefinitionVersionResponse'
    { creationTimestamp =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      version = Prelude.Nothing,
      definition = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the resource definition
-- version was created.
getResourceDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetResourceDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetResourceDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetResourceDefinitionVersionResponse)

-- | Arn of the resource definition version.
getResourceDefinitionVersionResponse_arn :: Lens.Lens' GetResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionVersionResponse_arn = Lens.lens (\GetResourceDefinitionVersionResponse' {arn} -> arn) (\s@GetResourceDefinitionVersionResponse' {} a -> s {arn = a} :: GetResourceDefinitionVersionResponse)

-- | The ID of the resource definition version.
getResourceDefinitionVersionResponse_id :: Lens.Lens' GetResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionVersionResponse_id = Lens.lens (\GetResourceDefinitionVersionResponse' {id} -> id) (\s@GetResourceDefinitionVersionResponse' {} a -> s {id = a} :: GetResourceDefinitionVersionResponse)

-- | The version of the resource definition version.
getResourceDefinitionVersionResponse_version :: Lens.Lens' GetResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionVersionResponse_version = Lens.lens (\GetResourceDefinitionVersionResponse' {version} -> version) (\s@GetResourceDefinitionVersionResponse' {} a -> s {version = a} :: GetResourceDefinitionVersionResponse)

-- | Information about the definition.
getResourceDefinitionVersionResponse_definition :: Lens.Lens' GetResourceDefinitionVersionResponse (Prelude.Maybe ResourceDefinitionVersion)
getResourceDefinitionVersionResponse_definition = Lens.lens (\GetResourceDefinitionVersionResponse' {definition} -> definition) (\s@GetResourceDefinitionVersionResponse' {} a -> s {definition = a} :: GetResourceDefinitionVersionResponse)

-- | The response's http status code.
getResourceDefinitionVersionResponse_httpStatus :: Lens.Lens' GetResourceDefinitionVersionResponse Prelude.Int
getResourceDefinitionVersionResponse_httpStatus = Lens.lens (\GetResourceDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@GetResourceDefinitionVersionResponse' {} a -> s {httpStatus = a} :: GetResourceDefinitionVersionResponse)

instance
  Prelude.NFData
    GetResourceDefinitionVersionResponse
