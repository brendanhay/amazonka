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
-- Module      : Network.AWS.Greengrass.GetCoreDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a core definition version.
module Network.AWS.Greengrass.GetCoreDefinitionVersion
  ( -- * Creating a Request
    GetCoreDefinitionVersion (..),
    newGetCoreDefinitionVersion,

    -- * Request Lenses
    getCoreDefinitionVersion_coreDefinitionId,
    getCoreDefinitionVersion_coreDefinitionVersionId,

    -- * Destructuring the Response
    GetCoreDefinitionVersionResponse (..),
    newGetCoreDefinitionVersionResponse,

    -- * Response Lenses
    getCoreDefinitionVersionResponse_creationTimestamp,
    getCoreDefinitionVersionResponse_nextToken,
    getCoreDefinitionVersionResponse_arn,
    getCoreDefinitionVersionResponse_id,
    getCoreDefinitionVersionResponse_version,
    getCoreDefinitionVersionResponse_definition,
    getCoreDefinitionVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCoreDefinitionVersion' smart constructor.
data GetCoreDefinitionVersion = GetCoreDefinitionVersion'
  { -- | The ID of the core definition.
    coreDefinitionId :: Core.Text,
    -- | The ID of the core definition version. This value maps to the
    -- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
    -- object, which is returned by \'\'ListCoreDefinitionVersions\'\'
    -- requests. If the version is the last one that was associated with a core
    -- definition, the value also maps to the \'\'LatestVersion\'\' property of
    -- the corresponding \'\'DefinitionInformation\'\' object.
    coreDefinitionVersionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCoreDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreDefinitionId', 'getCoreDefinitionVersion_coreDefinitionId' - The ID of the core definition.
--
-- 'coreDefinitionVersionId', 'getCoreDefinitionVersion_coreDefinitionVersionId' - The ID of the core definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListCoreDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a core
-- definition, the value also maps to the \'\'LatestVersion\'\' property of
-- the corresponding \'\'DefinitionInformation\'\' object.
newGetCoreDefinitionVersion ::
  -- | 'coreDefinitionId'
  Core.Text ->
  -- | 'coreDefinitionVersionId'
  Core.Text ->
  GetCoreDefinitionVersion
newGetCoreDefinitionVersion
  pCoreDefinitionId_
  pCoreDefinitionVersionId_ =
    GetCoreDefinitionVersion'
      { coreDefinitionId =
          pCoreDefinitionId_,
        coreDefinitionVersionId =
          pCoreDefinitionVersionId_
      }

-- | The ID of the core definition.
getCoreDefinitionVersion_coreDefinitionId :: Lens.Lens' GetCoreDefinitionVersion Core.Text
getCoreDefinitionVersion_coreDefinitionId = Lens.lens (\GetCoreDefinitionVersion' {coreDefinitionId} -> coreDefinitionId) (\s@GetCoreDefinitionVersion' {} a -> s {coreDefinitionId = a} :: GetCoreDefinitionVersion)

-- | The ID of the core definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListCoreDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a core
-- definition, the value also maps to the \'\'LatestVersion\'\' property of
-- the corresponding \'\'DefinitionInformation\'\' object.
getCoreDefinitionVersion_coreDefinitionVersionId :: Lens.Lens' GetCoreDefinitionVersion Core.Text
getCoreDefinitionVersion_coreDefinitionVersionId = Lens.lens (\GetCoreDefinitionVersion' {coreDefinitionVersionId} -> coreDefinitionVersionId) (\s@GetCoreDefinitionVersion' {} a -> s {coreDefinitionVersionId = a} :: GetCoreDefinitionVersion)

instance Core.AWSRequest GetCoreDefinitionVersion where
  type
    AWSResponse GetCoreDefinitionVersion =
      GetCoreDefinitionVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoreDefinitionVersionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Version")
            Core.<*> (x Core..?> "Definition")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetCoreDefinitionVersion

instance Core.NFData GetCoreDefinitionVersion

instance Core.ToHeaders GetCoreDefinitionVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetCoreDefinitionVersion where
  toPath GetCoreDefinitionVersion' {..} =
    Core.mconcat
      [ "/greengrass/definition/cores/",
        Core.toBS coreDefinitionId,
        "/versions/",
        Core.toBS coreDefinitionVersionId
      ]

instance Core.ToQuery GetCoreDefinitionVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCoreDefinitionVersionResponse' smart constructor.
data GetCoreDefinitionVersionResponse = GetCoreDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the core definition
    -- version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The ARN of the core definition version.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the core definition version.
    id :: Core.Maybe Core.Text,
    -- | The version of the core definition version.
    version :: Core.Maybe Core.Text,
    -- | Information about the core definition version.
    definition :: Core.Maybe CoreDefinitionVersion,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCoreDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getCoreDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the core definition
-- version was created.
--
-- 'nextToken', 'getCoreDefinitionVersionResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'arn', 'getCoreDefinitionVersionResponse_arn' - The ARN of the core definition version.
--
-- 'id', 'getCoreDefinitionVersionResponse_id' - The ID of the core definition version.
--
-- 'version', 'getCoreDefinitionVersionResponse_version' - The version of the core definition version.
--
-- 'definition', 'getCoreDefinitionVersionResponse_definition' - Information about the core definition version.
--
-- 'httpStatus', 'getCoreDefinitionVersionResponse_httpStatus' - The response's http status code.
newGetCoreDefinitionVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCoreDefinitionVersionResponse
newGetCoreDefinitionVersionResponse pHttpStatus_ =
  GetCoreDefinitionVersionResponse'
    { creationTimestamp =
        Core.Nothing,
      nextToken = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      definition = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the core definition
-- version was created.
getCoreDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetCoreDefinitionVersionResponse (Core.Maybe Core.Text)
getCoreDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetCoreDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetCoreDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetCoreDefinitionVersionResponse)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getCoreDefinitionVersionResponse_nextToken :: Lens.Lens' GetCoreDefinitionVersionResponse (Core.Maybe Core.Text)
getCoreDefinitionVersionResponse_nextToken = Lens.lens (\GetCoreDefinitionVersionResponse' {nextToken} -> nextToken) (\s@GetCoreDefinitionVersionResponse' {} a -> s {nextToken = a} :: GetCoreDefinitionVersionResponse)

-- | The ARN of the core definition version.
getCoreDefinitionVersionResponse_arn :: Lens.Lens' GetCoreDefinitionVersionResponse (Core.Maybe Core.Text)
getCoreDefinitionVersionResponse_arn = Lens.lens (\GetCoreDefinitionVersionResponse' {arn} -> arn) (\s@GetCoreDefinitionVersionResponse' {} a -> s {arn = a} :: GetCoreDefinitionVersionResponse)

-- | The ID of the core definition version.
getCoreDefinitionVersionResponse_id :: Lens.Lens' GetCoreDefinitionVersionResponse (Core.Maybe Core.Text)
getCoreDefinitionVersionResponse_id = Lens.lens (\GetCoreDefinitionVersionResponse' {id} -> id) (\s@GetCoreDefinitionVersionResponse' {} a -> s {id = a} :: GetCoreDefinitionVersionResponse)

-- | The version of the core definition version.
getCoreDefinitionVersionResponse_version :: Lens.Lens' GetCoreDefinitionVersionResponse (Core.Maybe Core.Text)
getCoreDefinitionVersionResponse_version = Lens.lens (\GetCoreDefinitionVersionResponse' {version} -> version) (\s@GetCoreDefinitionVersionResponse' {} a -> s {version = a} :: GetCoreDefinitionVersionResponse)

-- | Information about the core definition version.
getCoreDefinitionVersionResponse_definition :: Lens.Lens' GetCoreDefinitionVersionResponse (Core.Maybe CoreDefinitionVersion)
getCoreDefinitionVersionResponse_definition = Lens.lens (\GetCoreDefinitionVersionResponse' {definition} -> definition) (\s@GetCoreDefinitionVersionResponse' {} a -> s {definition = a} :: GetCoreDefinitionVersionResponse)

-- | The response's http status code.
getCoreDefinitionVersionResponse_httpStatus :: Lens.Lens' GetCoreDefinitionVersionResponse Core.Int
getCoreDefinitionVersionResponse_httpStatus = Lens.lens (\GetCoreDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@GetCoreDefinitionVersionResponse' {} a -> s {httpStatus = a} :: GetCoreDefinitionVersionResponse)

instance Core.NFData GetCoreDefinitionVersionResponse
