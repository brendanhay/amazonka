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
-- Module      : Amazonka.Greengrass.GetCoreDefinitionVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a core definition version.
module Amazonka.Greengrass.GetCoreDefinitionVersion
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
    getCoreDefinitionVersionResponse_nextToken,
    getCoreDefinitionVersionResponse_arn,
    getCoreDefinitionVersionResponse_id,
    getCoreDefinitionVersionResponse_creationTimestamp,
    getCoreDefinitionVersionResponse_version,
    getCoreDefinitionVersionResponse_definition,
    getCoreDefinitionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCoreDefinitionVersion' smart constructor.
data GetCoreDefinitionVersion = GetCoreDefinitionVersion'
  { -- | The ID of the core definition.
    coreDefinitionId :: Prelude.Text,
    -- | The ID of the core definition version. This value maps to the
    -- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
    -- object, which is returned by \'\'ListCoreDefinitionVersions\'\'
    -- requests. If the version is the last one that was associated with a core
    -- definition, the value also maps to the \'\'LatestVersion\'\' property of
    -- the corresponding \'\'DefinitionInformation\'\' object.
    coreDefinitionVersionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'coreDefinitionVersionId'
  Prelude.Text ->
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
getCoreDefinitionVersion_coreDefinitionId :: Lens.Lens' GetCoreDefinitionVersion Prelude.Text
getCoreDefinitionVersion_coreDefinitionId = Lens.lens (\GetCoreDefinitionVersion' {coreDefinitionId} -> coreDefinitionId) (\s@GetCoreDefinitionVersion' {} a -> s {coreDefinitionId = a} :: GetCoreDefinitionVersion)

-- | The ID of the core definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListCoreDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a core
-- definition, the value also maps to the \'\'LatestVersion\'\' property of
-- the corresponding \'\'DefinitionInformation\'\' object.
getCoreDefinitionVersion_coreDefinitionVersionId :: Lens.Lens' GetCoreDefinitionVersion Prelude.Text
getCoreDefinitionVersion_coreDefinitionVersionId = Lens.lens (\GetCoreDefinitionVersion' {coreDefinitionVersionId} -> coreDefinitionVersionId) (\s@GetCoreDefinitionVersion' {} a -> s {coreDefinitionVersionId = a} :: GetCoreDefinitionVersion)

instance Core.AWSRequest GetCoreDefinitionVersion where
  type
    AWSResponse GetCoreDefinitionVersion =
      GetCoreDefinitionVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoreDefinitionVersionResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (x Data..?> "Definition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCoreDefinitionVersion where
  hashWithSalt _salt GetCoreDefinitionVersion' {..} =
    _salt `Prelude.hashWithSalt` coreDefinitionId
      `Prelude.hashWithSalt` coreDefinitionVersionId

instance Prelude.NFData GetCoreDefinitionVersion where
  rnf GetCoreDefinitionVersion' {..} =
    Prelude.rnf coreDefinitionId
      `Prelude.seq` Prelude.rnf coreDefinitionVersionId

instance Data.ToHeaders GetCoreDefinitionVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCoreDefinitionVersion where
  toPath GetCoreDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/cores/",
        Data.toBS coreDefinitionId,
        "/versions/",
        Data.toBS coreDefinitionVersionId
      ]

instance Data.ToQuery GetCoreDefinitionVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCoreDefinitionVersionResponse' smart constructor.
data GetCoreDefinitionVersionResponse = GetCoreDefinitionVersionResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the core definition version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the core definition version.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the core definition
    -- version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The version of the core definition version.
    version :: Prelude.Maybe Prelude.Text,
    -- | Information about the core definition version.
    definition :: Prelude.Maybe CoreDefinitionVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoreDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCoreDefinitionVersionResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'arn', 'getCoreDefinitionVersionResponse_arn' - The ARN of the core definition version.
--
-- 'id', 'getCoreDefinitionVersionResponse_id' - The ID of the core definition version.
--
-- 'creationTimestamp', 'getCoreDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the core definition
-- version was created.
--
-- 'version', 'getCoreDefinitionVersionResponse_version' - The version of the core definition version.
--
-- 'definition', 'getCoreDefinitionVersionResponse_definition' - Information about the core definition version.
--
-- 'httpStatus', 'getCoreDefinitionVersionResponse_httpStatus' - The response's http status code.
newGetCoreDefinitionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCoreDefinitionVersionResponse
newGetCoreDefinitionVersionResponse pHttpStatus_ =
  GetCoreDefinitionVersionResponse'
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
getCoreDefinitionVersionResponse_nextToken :: Lens.Lens' GetCoreDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionVersionResponse_nextToken = Lens.lens (\GetCoreDefinitionVersionResponse' {nextToken} -> nextToken) (\s@GetCoreDefinitionVersionResponse' {} a -> s {nextToken = a} :: GetCoreDefinitionVersionResponse)

-- | The ARN of the core definition version.
getCoreDefinitionVersionResponse_arn :: Lens.Lens' GetCoreDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionVersionResponse_arn = Lens.lens (\GetCoreDefinitionVersionResponse' {arn} -> arn) (\s@GetCoreDefinitionVersionResponse' {} a -> s {arn = a} :: GetCoreDefinitionVersionResponse)

-- | The ID of the core definition version.
getCoreDefinitionVersionResponse_id :: Lens.Lens' GetCoreDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionVersionResponse_id = Lens.lens (\GetCoreDefinitionVersionResponse' {id} -> id) (\s@GetCoreDefinitionVersionResponse' {} a -> s {id = a} :: GetCoreDefinitionVersionResponse)

-- | The time, in milliseconds since the epoch, when the core definition
-- version was created.
getCoreDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetCoreDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetCoreDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetCoreDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetCoreDefinitionVersionResponse)

-- | The version of the core definition version.
getCoreDefinitionVersionResponse_version :: Lens.Lens' GetCoreDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getCoreDefinitionVersionResponse_version = Lens.lens (\GetCoreDefinitionVersionResponse' {version} -> version) (\s@GetCoreDefinitionVersionResponse' {} a -> s {version = a} :: GetCoreDefinitionVersionResponse)

-- | Information about the core definition version.
getCoreDefinitionVersionResponse_definition :: Lens.Lens' GetCoreDefinitionVersionResponse (Prelude.Maybe CoreDefinitionVersion)
getCoreDefinitionVersionResponse_definition = Lens.lens (\GetCoreDefinitionVersionResponse' {definition} -> definition) (\s@GetCoreDefinitionVersionResponse' {} a -> s {definition = a} :: GetCoreDefinitionVersionResponse)

-- | The response's http status code.
getCoreDefinitionVersionResponse_httpStatus :: Lens.Lens' GetCoreDefinitionVersionResponse Prelude.Int
getCoreDefinitionVersionResponse_httpStatus = Lens.lens (\GetCoreDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@GetCoreDefinitionVersionResponse' {} a -> s {httpStatus = a} :: GetCoreDefinitionVersionResponse)

instance
  Prelude.NFData
    GetCoreDefinitionVersionResponse
  where
  rnf GetCoreDefinitionVersionResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf httpStatus
