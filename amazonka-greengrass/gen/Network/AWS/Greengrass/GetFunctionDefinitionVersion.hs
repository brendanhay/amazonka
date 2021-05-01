{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFunctionDefinitionVersion' smart constructor.
data GetFunctionDefinitionVersion = GetFunctionDefinitionVersion'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Lambda function definition.
    functionDefinitionId :: Prelude.Text,
    -- | The ID of the function definition version. This value maps to the
    -- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
    -- object, which is returned by \'\'ListFunctionDefinitionVersions\'\'
    -- requests. If the version is the last one that was associated with a
    -- function definition, the value also maps to the \'\'LatestVersion\'\'
    -- property of the corresponding \'\'DefinitionInformation\'\' object.
    functionDefinitionVersionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'functionDefinitionVersionId'
  Prelude.Text ->
  GetFunctionDefinitionVersion
newGetFunctionDefinitionVersion
  pFunctionDefinitionId_
  pFunctionDefinitionVersionId_ =
    GetFunctionDefinitionVersion'
      { nextToken =
          Prelude.Nothing,
        functionDefinitionId = pFunctionDefinitionId_,
        functionDefinitionVersionId =
          pFunctionDefinitionVersionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getFunctionDefinitionVersion_nextToken :: Lens.Lens' GetFunctionDefinitionVersion (Prelude.Maybe Prelude.Text)
getFunctionDefinitionVersion_nextToken = Lens.lens (\GetFunctionDefinitionVersion' {nextToken} -> nextToken) (\s@GetFunctionDefinitionVersion' {} a -> s {nextToken = a} :: GetFunctionDefinitionVersion)

-- | The ID of the Lambda function definition.
getFunctionDefinitionVersion_functionDefinitionId :: Lens.Lens' GetFunctionDefinitionVersion Prelude.Text
getFunctionDefinitionVersion_functionDefinitionId = Lens.lens (\GetFunctionDefinitionVersion' {functionDefinitionId} -> functionDefinitionId) (\s@GetFunctionDefinitionVersion' {} a -> s {functionDefinitionId = a} :: GetFunctionDefinitionVersion)

-- | The ID of the function definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListFunctionDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- function definition, the value also maps to the \'\'LatestVersion\'\'
-- property of the corresponding \'\'DefinitionInformation\'\' object.
getFunctionDefinitionVersion_functionDefinitionVersionId :: Lens.Lens' GetFunctionDefinitionVersion Prelude.Text
getFunctionDefinitionVersion_functionDefinitionVersionId = Lens.lens (\GetFunctionDefinitionVersion' {functionDefinitionVersionId} -> functionDefinitionVersionId) (\s@GetFunctionDefinitionVersion' {} a -> s {functionDefinitionVersionId = a} :: GetFunctionDefinitionVersion)

instance
  Prelude.AWSRequest
    GetFunctionDefinitionVersion
  where
  type
    Rs GetFunctionDefinitionVersion =
      GetFunctionDefinitionVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFunctionDefinitionVersionResponse'
            Prelude.<$> (x Prelude..?> "CreationTimestamp")
            Prelude.<*> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Arn")
            Prelude.<*> (x Prelude..?> "Id")
            Prelude.<*> (x Prelude..?> "Version")
            Prelude.<*> (x Prelude..?> "Definition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetFunctionDefinitionVersion

instance Prelude.NFData GetFunctionDefinitionVersion

instance
  Prelude.ToHeaders
    GetFunctionDefinitionVersion
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetFunctionDefinitionVersion where
  toPath GetFunctionDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/functions/",
        Prelude.toBS functionDefinitionId,
        "/versions/",
        Prelude.toBS functionDefinitionVersionId
      ]

instance Prelude.ToQuery GetFunctionDefinitionVersion where
  toQuery GetFunctionDefinitionVersion' {..} =
    Prelude.mconcat ["NextToken" Prelude.=: nextToken]

-- | /See:/ 'newGetFunctionDefinitionVersionResponse' smart constructor.
data GetFunctionDefinitionVersionResponse = GetFunctionDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the function definition
    -- version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the function definition version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the function definition version.
    id :: Prelude.Maybe Prelude.Text,
    -- | The version of the function definition version.
    version :: Prelude.Maybe Prelude.Text,
    -- | Information on the definition.
    definition :: Prelude.Maybe FunctionDefinitionVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetFunctionDefinitionVersionResponse
newGetFunctionDefinitionVersionResponse pHttpStatus_ =
  GetFunctionDefinitionVersionResponse'
    { creationTimestamp =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      version = Prelude.Nothing,
      definition = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the function definition
-- version was created.
getFunctionDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetFunctionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetFunctionDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetFunctionDefinitionVersionResponse)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getFunctionDefinitionVersionResponse_nextToken :: Lens.Lens' GetFunctionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionVersionResponse_nextToken = Lens.lens (\GetFunctionDefinitionVersionResponse' {nextToken} -> nextToken) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {nextToken = a} :: GetFunctionDefinitionVersionResponse)

-- | The ARN of the function definition version.
getFunctionDefinitionVersionResponse_arn :: Lens.Lens' GetFunctionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionVersionResponse_arn = Lens.lens (\GetFunctionDefinitionVersionResponse' {arn} -> arn) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {arn = a} :: GetFunctionDefinitionVersionResponse)

-- | The ID of the function definition version.
getFunctionDefinitionVersionResponse_id :: Lens.Lens' GetFunctionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionVersionResponse_id = Lens.lens (\GetFunctionDefinitionVersionResponse' {id} -> id) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {id = a} :: GetFunctionDefinitionVersionResponse)

-- | The version of the function definition version.
getFunctionDefinitionVersionResponse_version :: Lens.Lens' GetFunctionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionVersionResponse_version = Lens.lens (\GetFunctionDefinitionVersionResponse' {version} -> version) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {version = a} :: GetFunctionDefinitionVersionResponse)

-- | Information on the definition.
getFunctionDefinitionVersionResponse_definition :: Lens.Lens' GetFunctionDefinitionVersionResponse (Prelude.Maybe FunctionDefinitionVersion)
getFunctionDefinitionVersionResponse_definition = Lens.lens (\GetFunctionDefinitionVersionResponse' {definition} -> definition) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {definition = a} :: GetFunctionDefinitionVersionResponse)

-- | The response's http status code.
getFunctionDefinitionVersionResponse_httpStatus :: Lens.Lens' GetFunctionDefinitionVersionResponse Prelude.Int
getFunctionDefinitionVersionResponse_httpStatus = Lens.lens (\GetFunctionDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {httpStatus = a} :: GetFunctionDefinitionVersionResponse)

instance
  Prelude.NFData
    GetFunctionDefinitionVersionResponse
