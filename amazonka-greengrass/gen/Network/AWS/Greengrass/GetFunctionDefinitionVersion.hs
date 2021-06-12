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
-- Module      : Network.AWS.Greengrass.GetFunctionDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a Lambda function definition version,
-- including which Lambda functions are included in the version and their
-- configurations.
module Network.AWS.Greengrass.GetFunctionDefinitionVersion
  ( -- * Creating a Request
    GetFunctionDefinitionVersion (..),
    newGetFunctionDefinitionVersion,

    -- * Request Lenses
    getFunctionDefinitionVersion_nextToken,
    getFunctionDefinitionVersion_functionDefinitionId,
    getFunctionDefinitionVersion_functionDefinitionVersionId,

    -- * Destructuring the Response
    GetFunctionDefinitionVersionResponse (..),
    newGetFunctionDefinitionVersionResponse,

    -- * Response Lenses
    getFunctionDefinitionVersionResponse_creationTimestamp,
    getFunctionDefinitionVersionResponse_nextToken,
    getFunctionDefinitionVersionResponse_arn,
    getFunctionDefinitionVersionResponse_id,
    getFunctionDefinitionVersionResponse_version,
    getFunctionDefinitionVersionResponse_definition,
    getFunctionDefinitionVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFunctionDefinitionVersion' smart constructor.
data GetFunctionDefinitionVersion = GetFunctionDefinitionVersion'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the Lambda function definition.
    functionDefinitionId :: Core.Text,
    -- | The ID of the function definition version. This value maps to the
    -- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
    -- object, which is returned by \'\'ListFunctionDefinitionVersions\'\'
    -- requests. If the version is the last one that was associated with a
    -- function definition, the value also maps to the \'\'LatestVersion\'\'
    -- property of the corresponding \'\'DefinitionInformation\'\' object.
    functionDefinitionVersionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetFunctionDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getFunctionDefinitionVersion_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'functionDefinitionId', 'getFunctionDefinitionVersion_functionDefinitionId' - The ID of the Lambda function definition.
--
-- 'functionDefinitionVersionId', 'getFunctionDefinitionVersion_functionDefinitionVersionId' - The ID of the function definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListFunctionDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- function definition, the value also maps to the \'\'LatestVersion\'\'
-- property of the corresponding \'\'DefinitionInformation\'\' object.
newGetFunctionDefinitionVersion ::
  -- | 'functionDefinitionId'
  Core.Text ->
  -- | 'functionDefinitionVersionId'
  Core.Text ->
  GetFunctionDefinitionVersion
newGetFunctionDefinitionVersion
  pFunctionDefinitionId_
  pFunctionDefinitionVersionId_ =
    GetFunctionDefinitionVersion'
      { nextToken =
          Core.Nothing,
        functionDefinitionId = pFunctionDefinitionId_,
        functionDefinitionVersionId =
          pFunctionDefinitionVersionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getFunctionDefinitionVersion_nextToken :: Lens.Lens' GetFunctionDefinitionVersion (Core.Maybe Core.Text)
getFunctionDefinitionVersion_nextToken = Lens.lens (\GetFunctionDefinitionVersion' {nextToken} -> nextToken) (\s@GetFunctionDefinitionVersion' {} a -> s {nextToken = a} :: GetFunctionDefinitionVersion)

-- | The ID of the Lambda function definition.
getFunctionDefinitionVersion_functionDefinitionId :: Lens.Lens' GetFunctionDefinitionVersion Core.Text
getFunctionDefinitionVersion_functionDefinitionId = Lens.lens (\GetFunctionDefinitionVersion' {functionDefinitionId} -> functionDefinitionId) (\s@GetFunctionDefinitionVersion' {} a -> s {functionDefinitionId = a} :: GetFunctionDefinitionVersion)

-- | The ID of the function definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListFunctionDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- function definition, the value also maps to the \'\'LatestVersion\'\'
-- property of the corresponding \'\'DefinitionInformation\'\' object.
getFunctionDefinitionVersion_functionDefinitionVersionId :: Lens.Lens' GetFunctionDefinitionVersion Core.Text
getFunctionDefinitionVersion_functionDefinitionVersionId = Lens.lens (\GetFunctionDefinitionVersion' {functionDefinitionVersionId} -> functionDefinitionVersionId) (\s@GetFunctionDefinitionVersion' {} a -> s {functionDefinitionVersionId = a} :: GetFunctionDefinitionVersion)

instance Core.AWSRequest GetFunctionDefinitionVersion where
  type
    AWSResponse GetFunctionDefinitionVersion =
      GetFunctionDefinitionVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFunctionDefinitionVersionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Version")
            Core.<*> (x Core..?> "Definition")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetFunctionDefinitionVersion

instance Core.NFData GetFunctionDefinitionVersion

instance Core.ToHeaders GetFunctionDefinitionVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetFunctionDefinitionVersion where
  toPath GetFunctionDefinitionVersion' {..} =
    Core.mconcat
      [ "/greengrass/definition/functions/",
        Core.toBS functionDefinitionId,
        "/versions/",
        Core.toBS functionDefinitionVersionId
      ]

instance Core.ToQuery GetFunctionDefinitionVersion where
  toQuery GetFunctionDefinitionVersion' {..} =
    Core.mconcat ["NextToken" Core.=: nextToken]

-- | /See:/ 'newGetFunctionDefinitionVersionResponse' smart constructor.
data GetFunctionDefinitionVersionResponse = GetFunctionDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the function definition
    -- version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The ARN of the function definition version.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the function definition version.
    id :: Core.Maybe Core.Text,
    -- | The version of the function definition version.
    version :: Core.Maybe Core.Text,
    -- | Information on the definition.
    definition :: Core.Maybe FunctionDefinitionVersion,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetFunctionDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getFunctionDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the function definition
-- version was created.
--
-- 'nextToken', 'getFunctionDefinitionVersionResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'arn', 'getFunctionDefinitionVersionResponse_arn' - The ARN of the function definition version.
--
-- 'id', 'getFunctionDefinitionVersionResponse_id' - The ID of the function definition version.
--
-- 'version', 'getFunctionDefinitionVersionResponse_version' - The version of the function definition version.
--
-- 'definition', 'getFunctionDefinitionVersionResponse_definition' - Information on the definition.
--
-- 'httpStatus', 'getFunctionDefinitionVersionResponse_httpStatus' - The response's http status code.
newGetFunctionDefinitionVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetFunctionDefinitionVersionResponse
newGetFunctionDefinitionVersionResponse pHttpStatus_ =
  GetFunctionDefinitionVersionResponse'
    { creationTimestamp =
        Core.Nothing,
      nextToken = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      definition = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the function definition
-- version was created.
getFunctionDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetFunctionDefinitionVersionResponse (Core.Maybe Core.Text)
getFunctionDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetFunctionDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetFunctionDefinitionVersionResponse)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getFunctionDefinitionVersionResponse_nextToken :: Lens.Lens' GetFunctionDefinitionVersionResponse (Core.Maybe Core.Text)
getFunctionDefinitionVersionResponse_nextToken = Lens.lens (\GetFunctionDefinitionVersionResponse' {nextToken} -> nextToken) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {nextToken = a} :: GetFunctionDefinitionVersionResponse)

-- | The ARN of the function definition version.
getFunctionDefinitionVersionResponse_arn :: Lens.Lens' GetFunctionDefinitionVersionResponse (Core.Maybe Core.Text)
getFunctionDefinitionVersionResponse_arn = Lens.lens (\GetFunctionDefinitionVersionResponse' {arn} -> arn) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {arn = a} :: GetFunctionDefinitionVersionResponse)

-- | The ID of the function definition version.
getFunctionDefinitionVersionResponse_id :: Lens.Lens' GetFunctionDefinitionVersionResponse (Core.Maybe Core.Text)
getFunctionDefinitionVersionResponse_id = Lens.lens (\GetFunctionDefinitionVersionResponse' {id} -> id) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {id = a} :: GetFunctionDefinitionVersionResponse)

-- | The version of the function definition version.
getFunctionDefinitionVersionResponse_version :: Lens.Lens' GetFunctionDefinitionVersionResponse (Core.Maybe Core.Text)
getFunctionDefinitionVersionResponse_version = Lens.lens (\GetFunctionDefinitionVersionResponse' {version} -> version) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {version = a} :: GetFunctionDefinitionVersionResponse)

-- | Information on the definition.
getFunctionDefinitionVersionResponse_definition :: Lens.Lens' GetFunctionDefinitionVersionResponse (Core.Maybe FunctionDefinitionVersion)
getFunctionDefinitionVersionResponse_definition = Lens.lens (\GetFunctionDefinitionVersionResponse' {definition} -> definition) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {definition = a} :: GetFunctionDefinitionVersionResponse)

-- | The response's http status code.
getFunctionDefinitionVersionResponse_httpStatus :: Lens.Lens' GetFunctionDefinitionVersionResponse Core.Int
getFunctionDefinitionVersionResponse_httpStatus = Lens.lens (\GetFunctionDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {httpStatus = a} :: GetFunctionDefinitionVersionResponse)

instance
  Core.NFData
    GetFunctionDefinitionVersionResponse
