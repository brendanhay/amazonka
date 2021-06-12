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
-- Module      : Network.AWS.Greengrass.GetLoggerDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a logger definition version.
module Network.AWS.Greengrass.GetLoggerDefinitionVersion
  ( -- * Creating a Request
    GetLoggerDefinitionVersion (..),
    newGetLoggerDefinitionVersion,

    -- * Request Lenses
    getLoggerDefinitionVersion_nextToken,
    getLoggerDefinitionVersion_loggerDefinitionVersionId,
    getLoggerDefinitionVersion_loggerDefinitionId,

    -- * Destructuring the Response
    GetLoggerDefinitionVersionResponse (..),
    newGetLoggerDefinitionVersionResponse,

    -- * Response Lenses
    getLoggerDefinitionVersionResponse_creationTimestamp,
    getLoggerDefinitionVersionResponse_arn,
    getLoggerDefinitionVersionResponse_id,
    getLoggerDefinitionVersionResponse_version,
    getLoggerDefinitionVersionResponse_definition,
    getLoggerDefinitionVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLoggerDefinitionVersion' smart constructor.
data GetLoggerDefinitionVersion = GetLoggerDefinitionVersion'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the logger definition version. This value maps to the
    -- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
    -- object, which is returned by \'\'ListLoggerDefinitionVersions\'\'
    -- requests. If the version is the last one that was associated with a
    -- logger definition, the value also maps to the \'\'LatestVersion\'\'
    -- property of the corresponding \'\'DefinitionInformation\'\' object.
    loggerDefinitionVersionId :: Core.Text,
    -- | The ID of the logger definition.
    loggerDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLoggerDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getLoggerDefinitionVersion_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'loggerDefinitionVersionId', 'getLoggerDefinitionVersion_loggerDefinitionVersionId' - The ID of the logger definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListLoggerDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- logger definition, the value also maps to the \'\'LatestVersion\'\'
-- property of the corresponding \'\'DefinitionInformation\'\' object.
--
-- 'loggerDefinitionId', 'getLoggerDefinitionVersion_loggerDefinitionId' - The ID of the logger definition.
newGetLoggerDefinitionVersion ::
  -- | 'loggerDefinitionVersionId'
  Core.Text ->
  -- | 'loggerDefinitionId'
  Core.Text ->
  GetLoggerDefinitionVersion
newGetLoggerDefinitionVersion
  pLoggerDefinitionVersionId_
  pLoggerDefinitionId_ =
    GetLoggerDefinitionVersion'
      { nextToken =
          Core.Nothing,
        loggerDefinitionVersionId =
          pLoggerDefinitionVersionId_,
        loggerDefinitionId = pLoggerDefinitionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getLoggerDefinitionVersion_nextToken :: Lens.Lens' GetLoggerDefinitionVersion (Core.Maybe Core.Text)
getLoggerDefinitionVersion_nextToken = Lens.lens (\GetLoggerDefinitionVersion' {nextToken} -> nextToken) (\s@GetLoggerDefinitionVersion' {} a -> s {nextToken = a} :: GetLoggerDefinitionVersion)

-- | The ID of the logger definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListLoggerDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- logger definition, the value also maps to the \'\'LatestVersion\'\'
-- property of the corresponding \'\'DefinitionInformation\'\' object.
getLoggerDefinitionVersion_loggerDefinitionVersionId :: Lens.Lens' GetLoggerDefinitionVersion Core.Text
getLoggerDefinitionVersion_loggerDefinitionVersionId = Lens.lens (\GetLoggerDefinitionVersion' {loggerDefinitionVersionId} -> loggerDefinitionVersionId) (\s@GetLoggerDefinitionVersion' {} a -> s {loggerDefinitionVersionId = a} :: GetLoggerDefinitionVersion)

-- | The ID of the logger definition.
getLoggerDefinitionVersion_loggerDefinitionId :: Lens.Lens' GetLoggerDefinitionVersion Core.Text
getLoggerDefinitionVersion_loggerDefinitionId = Lens.lens (\GetLoggerDefinitionVersion' {loggerDefinitionId} -> loggerDefinitionId) (\s@GetLoggerDefinitionVersion' {} a -> s {loggerDefinitionId = a} :: GetLoggerDefinitionVersion)

instance Core.AWSRequest GetLoggerDefinitionVersion where
  type
    AWSResponse GetLoggerDefinitionVersion =
      GetLoggerDefinitionVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggerDefinitionVersionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Version")
            Core.<*> (x Core..?> "Definition")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetLoggerDefinitionVersion

instance Core.NFData GetLoggerDefinitionVersion

instance Core.ToHeaders GetLoggerDefinitionVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetLoggerDefinitionVersion where
  toPath GetLoggerDefinitionVersion' {..} =
    Core.mconcat
      [ "/greengrass/definition/loggers/",
        Core.toBS loggerDefinitionId,
        "/versions/",
        Core.toBS loggerDefinitionVersionId
      ]

instance Core.ToQuery GetLoggerDefinitionVersion where
  toQuery GetLoggerDefinitionVersion' {..} =
    Core.mconcat ["NextToken" Core.=: nextToken]

-- | /See:/ 'newGetLoggerDefinitionVersionResponse' smart constructor.
data GetLoggerDefinitionVersionResponse = GetLoggerDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the logger definition
    -- version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ARN of the logger definition version.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the logger definition version.
    id :: Core.Maybe Core.Text,
    -- | The version of the logger definition version.
    version :: Core.Maybe Core.Text,
    -- | Information about the logger definition version.
    definition :: Core.Maybe LoggerDefinitionVersion,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLoggerDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getLoggerDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the logger definition
-- version was created.
--
-- 'arn', 'getLoggerDefinitionVersionResponse_arn' - The ARN of the logger definition version.
--
-- 'id', 'getLoggerDefinitionVersionResponse_id' - The ID of the logger definition version.
--
-- 'version', 'getLoggerDefinitionVersionResponse_version' - The version of the logger definition version.
--
-- 'definition', 'getLoggerDefinitionVersionResponse_definition' - Information about the logger definition version.
--
-- 'httpStatus', 'getLoggerDefinitionVersionResponse_httpStatus' - The response's http status code.
newGetLoggerDefinitionVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetLoggerDefinitionVersionResponse
newGetLoggerDefinitionVersionResponse pHttpStatus_ =
  GetLoggerDefinitionVersionResponse'
    { creationTimestamp =
        Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      definition = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the logger definition
-- version was created.
getLoggerDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
getLoggerDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetLoggerDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetLoggerDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetLoggerDefinitionVersionResponse)

-- | The ARN of the logger definition version.
getLoggerDefinitionVersionResponse_arn :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
getLoggerDefinitionVersionResponse_arn = Lens.lens (\GetLoggerDefinitionVersionResponse' {arn} -> arn) (\s@GetLoggerDefinitionVersionResponse' {} a -> s {arn = a} :: GetLoggerDefinitionVersionResponse)

-- | The ID of the logger definition version.
getLoggerDefinitionVersionResponse_id :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
getLoggerDefinitionVersionResponse_id = Lens.lens (\GetLoggerDefinitionVersionResponse' {id} -> id) (\s@GetLoggerDefinitionVersionResponse' {} a -> s {id = a} :: GetLoggerDefinitionVersionResponse)

-- | The version of the logger definition version.
getLoggerDefinitionVersionResponse_version :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
getLoggerDefinitionVersionResponse_version = Lens.lens (\GetLoggerDefinitionVersionResponse' {version} -> version) (\s@GetLoggerDefinitionVersionResponse' {} a -> s {version = a} :: GetLoggerDefinitionVersionResponse)

-- | Information about the logger definition version.
getLoggerDefinitionVersionResponse_definition :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe LoggerDefinitionVersion)
getLoggerDefinitionVersionResponse_definition = Lens.lens (\GetLoggerDefinitionVersionResponse' {definition} -> definition) (\s@GetLoggerDefinitionVersionResponse' {} a -> s {definition = a} :: GetLoggerDefinitionVersionResponse)

-- | The response's http status code.
getLoggerDefinitionVersionResponse_httpStatus :: Lens.Lens' GetLoggerDefinitionVersionResponse Core.Int
getLoggerDefinitionVersionResponse_httpStatus = Lens.lens (\GetLoggerDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@GetLoggerDefinitionVersionResponse' {} a -> s {httpStatus = a} :: GetLoggerDefinitionVersionResponse)

instance
  Core.NFData
    GetLoggerDefinitionVersionResponse
