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
-- Module      : Amazonka.Greengrass.GetLoggerDefinitionVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a logger definition version.
module Amazonka.Greengrass.GetLoggerDefinitionVersion
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
    getLoggerDefinitionVersionResponse_arn,
    getLoggerDefinitionVersionResponse_id,
    getLoggerDefinitionVersionResponse_creationTimestamp,
    getLoggerDefinitionVersionResponse_version,
    getLoggerDefinitionVersionResponse_definition,
    getLoggerDefinitionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLoggerDefinitionVersion' smart constructor.
data GetLoggerDefinitionVersion = GetLoggerDefinitionVersion'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the logger definition version. This value maps to the
    -- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
    -- object, which is returned by \'\'ListLoggerDefinitionVersions\'\'
    -- requests. If the version is the last one that was associated with a
    -- logger definition, the value also maps to the \'\'LatestVersion\'\'
    -- property of the corresponding \'\'DefinitionInformation\'\' object.
    loggerDefinitionVersionId :: Prelude.Text,
    -- | The ID of the logger definition.
    loggerDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'loggerDefinitionId'
  Prelude.Text ->
  GetLoggerDefinitionVersion
newGetLoggerDefinitionVersion
  pLoggerDefinitionVersionId_
  pLoggerDefinitionId_ =
    GetLoggerDefinitionVersion'
      { nextToken =
          Prelude.Nothing,
        loggerDefinitionVersionId =
          pLoggerDefinitionVersionId_,
        loggerDefinitionId = pLoggerDefinitionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getLoggerDefinitionVersion_nextToken :: Lens.Lens' GetLoggerDefinitionVersion (Prelude.Maybe Prelude.Text)
getLoggerDefinitionVersion_nextToken = Lens.lens (\GetLoggerDefinitionVersion' {nextToken} -> nextToken) (\s@GetLoggerDefinitionVersion' {} a -> s {nextToken = a} :: GetLoggerDefinitionVersion)

-- | The ID of the logger definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListLoggerDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- logger definition, the value also maps to the \'\'LatestVersion\'\'
-- property of the corresponding \'\'DefinitionInformation\'\' object.
getLoggerDefinitionVersion_loggerDefinitionVersionId :: Lens.Lens' GetLoggerDefinitionVersion Prelude.Text
getLoggerDefinitionVersion_loggerDefinitionVersionId = Lens.lens (\GetLoggerDefinitionVersion' {loggerDefinitionVersionId} -> loggerDefinitionVersionId) (\s@GetLoggerDefinitionVersion' {} a -> s {loggerDefinitionVersionId = a} :: GetLoggerDefinitionVersion)

-- | The ID of the logger definition.
getLoggerDefinitionVersion_loggerDefinitionId :: Lens.Lens' GetLoggerDefinitionVersion Prelude.Text
getLoggerDefinitionVersion_loggerDefinitionId = Lens.lens (\GetLoggerDefinitionVersion' {loggerDefinitionId} -> loggerDefinitionId) (\s@GetLoggerDefinitionVersion' {} a -> s {loggerDefinitionId = a} :: GetLoggerDefinitionVersion)

instance Core.AWSRequest GetLoggerDefinitionVersion where
  type
    AWSResponse GetLoggerDefinitionVersion =
      GetLoggerDefinitionVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggerDefinitionVersionResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "Version")
            Prelude.<*> (x Core..?> "Definition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLoggerDefinitionVersion where
  hashWithSalt _salt GetLoggerDefinitionVersion' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` loggerDefinitionVersionId
      `Prelude.hashWithSalt` loggerDefinitionId

instance Prelude.NFData GetLoggerDefinitionVersion where
  rnf GetLoggerDefinitionVersion' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf loggerDefinitionVersionId
      `Prelude.seq` Prelude.rnf loggerDefinitionId

instance Core.ToHeaders GetLoggerDefinitionVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetLoggerDefinitionVersion where
  toPath GetLoggerDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/loggers/",
        Core.toBS loggerDefinitionId,
        "/versions/",
        Core.toBS loggerDefinitionVersionId
      ]

instance Core.ToQuery GetLoggerDefinitionVersion where
  toQuery GetLoggerDefinitionVersion' {..} =
    Prelude.mconcat ["NextToken" Core.=: nextToken]

-- | /See:/ 'newGetLoggerDefinitionVersionResponse' smart constructor.
data GetLoggerDefinitionVersionResponse = GetLoggerDefinitionVersionResponse'
  { -- | The ARN of the logger definition version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the logger definition version.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the logger definition
    -- version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The version of the logger definition version.
    version :: Prelude.Maybe Prelude.Text,
    -- | Information about the logger definition version.
    definition :: Prelude.Maybe LoggerDefinitionVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggerDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getLoggerDefinitionVersionResponse_arn' - The ARN of the logger definition version.
--
-- 'id', 'getLoggerDefinitionVersionResponse_id' - The ID of the logger definition version.
--
-- 'creationTimestamp', 'getLoggerDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the logger definition
-- version was created.
--
-- 'version', 'getLoggerDefinitionVersionResponse_version' - The version of the logger definition version.
--
-- 'definition', 'getLoggerDefinitionVersionResponse_definition' - Information about the logger definition version.
--
-- 'httpStatus', 'getLoggerDefinitionVersionResponse_httpStatus' - The response's http status code.
newGetLoggerDefinitionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLoggerDefinitionVersionResponse
newGetLoggerDefinitionVersionResponse pHttpStatus_ =
  GetLoggerDefinitionVersionResponse'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      version = Prelude.Nothing,
      definition = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the logger definition version.
getLoggerDefinitionVersionResponse_arn :: Lens.Lens' GetLoggerDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionVersionResponse_arn = Lens.lens (\GetLoggerDefinitionVersionResponse' {arn} -> arn) (\s@GetLoggerDefinitionVersionResponse' {} a -> s {arn = a} :: GetLoggerDefinitionVersionResponse)

-- | The ID of the logger definition version.
getLoggerDefinitionVersionResponse_id :: Lens.Lens' GetLoggerDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionVersionResponse_id = Lens.lens (\GetLoggerDefinitionVersionResponse' {id} -> id) (\s@GetLoggerDefinitionVersionResponse' {} a -> s {id = a} :: GetLoggerDefinitionVersionResponse)

-- | The time, in milliseconds since the epoch, when the logger definition
-- version was created.
getLoggerDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetLoggerDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetLoggerDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetLoggerDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetLoggerDefinitionVersionResponse)

-- | The version of the logger definition version.
getLoggerDefinitionVersionResponse_version :: Lens.Lens' GetLoggerDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getLoggerDefinitionVersionResponse_version = Lens.lens (\GetLoggerDefinitionVersionResponse' {version} -> version) (\s@GetLoggerDefinitionVersionResponse' {} a -> s {version = a} :: GetLoggerDefinitionVersionResponse)

-- | Information about the logger definition version.
getLoggerDefinitionVersionResponse_definition :: Lens.Lens' GetLoggerDefinitionVersionResponse (Prelude.Maybe LoggerDefinitionVersion)
getLoggerDefinitionVersionResponse_definition = Lens.lens (\GetLoggerDefinitionVersionResponse' {definition} -> definition) (\s@GetLoggerDefinitionVersionResponse' {} a -> s {definition = a} :: GetLoggerDefinitionVersionResponse)

-- | The response's http status code.
getLoggerDefinitionVersionResponse_httpStatus :: Lens.Lens' GetLoggerDefinitionVersionResponse Prelude.Int
getLoggerDefinitionVersionResponse_httpStatus = Lens.lens (\GetLoggerDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@GetLoggerDefinitionVersionResponse' {} a -> s {httpStatus = a} :: GetLoggerDefinitionVersionResponse)

instance
  Prelude.NFData
    GetLoggerDefinitionVersionResponse
  where
  rnf GetLoggerDefinitionVersionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf httpStatus
