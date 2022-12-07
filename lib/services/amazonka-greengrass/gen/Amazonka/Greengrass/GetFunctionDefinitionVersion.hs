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
-- Module      : Amazonka.Greengrass.GetFunctionDefinitionVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a Lambda function definition version,
-- including which Lambda functions are included in the version and their
-- configurations.
module Amazonka.Greengrass.GetFunctionDefinitionVersion
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
    getFunctionDefinitionVersionResponse_nextToken,
    getFunctionDefinitionVersionResponse_arn,
    getFunctionDefinitionVersionResponse_id,
    getFunctionDefinitionVersionResponse_creationTimestamp,
    getFunctionDefinitionVersionResponse_version,
    getFunctionDefinitionVersionResponse_definition,
    getFunctionDefinitionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetFunctionDefinitionVersion where
  type
    AWSResponse GetFunctionDefinitionVersion =
      GetFunctionDefinitionVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFunctionDefinitionVersionResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (x Data..?> "Definition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetFunctionDefinitionVersion
  where
  hashWithSalt _salt GetFunctionDefinitionVersion' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` functionDefinitionId
      `Prelude.hashWithSalt` functionDefinitionVersionId

instance Prelude.NFData GetFunctionDefinitionVersion where
  rnf GetFunctionDefinitionVersion' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf functionDefinitionId
      `Prelude.seq` Prelude.rnf functionDefinitionVersionId

instance Data.ToHeaders GetFunctionDefinitionVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetFunctionDefinitionVersion where
  toPath GetFunctionDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/functions/",
        Data.toBS functionDefinitionId,
        "/versions/",
        Data.toBS functionDefinitionVersionId
      ]

instance Data.ToQuery GetFunctionDefinitionVersion where
  toQuery GetFunctionDefinitionVersion' {..} =
    Prelude.mconcat ["NextToken" Data.=: nextToken]

-- | /See:/ 'newGetFunctionDefinitionVersionResponse' smart constructor.
data GetFunctionDefinitionVersionResponse = GetFunctionDefinitionVersionResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the function definition version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the function definition version.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the function definition
    -- version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The version of the function definition version.
    version :: Prelude.Maybe Prelude.Text,
    -- | Information on the definition.
    definition :: Prelude.Maybe FunctionDefinitionVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFunctionDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getFunctionDefinitionVersionResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'arn', 'getFunctionDefinitionVersionResponse_arn' - The ARN of the function definition version.
--
-- 'id', 'getFunctionDefinitionVersionResponse_id' - The ID of the function definition version.
--
-- 'creationTimestamp', 'getFunctionDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the function definition
-- version was created.
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
    { nextToken =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      version = Prelude.Nothing,
      definition = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

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

-- | The time, in milliseconds since the epoch, when the function definition
-- version was created.
getFunctionDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetFunctionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getFunctionDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetFunctionDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetFunctionDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetFunctionDefinitionVersionResponse)

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
  where
  rnf GetFunctionDefinitionVersionResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf httpStatus
