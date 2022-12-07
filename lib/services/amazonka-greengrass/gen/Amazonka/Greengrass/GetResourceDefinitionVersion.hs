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
-- Module      : Amazonka.Greengrass.GetResourceDefinitionVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a resource definition version, including
-- which resources are included in the version.
module Amazonka.Greengrass.GetResourceDefinitionVersion
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
    getResourceDefinitionVersionResponse_arn,
    getResourceDefinitionVersionResponse_id,
    getResourceDefinitionVersionResponse_creationTimestamp,
    getResourceDefinitionVersionResponse_version,
    getResourceDefinitionVersionResponse_definition,
    getResourceDefinitionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceDefinitionVersionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (x Data..?> "Definition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetResourceDefinitionVersion
  where
  hashWithSalt _salt GetResourceDefinitionVersion' {..} =
    _salt
      `Prelude.hashWithSalt` resourceDefinitionVersionId
      `Prelude.hashWithSalt` resourceDefinitionId

instance Prelude.NFData GetResourceDefinitionVersion where
  rnf GetResourceDefinitionVersion' {..} =
    Prelude.rnf resourceDefinitionVersionId
      `Prelude.seq` Prelude.rnf resourceDefinitionId

instance Data.ToHeaders GetResourceDefinitionVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetResourceDefinitionVersion where
  toPath GetResourceDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/resources/",
        Data.toBS resourceDefinitionId,
        "/versions/",
        Data.toBS resourceDefinitionVersionId
      ]

instance Data.ToQuery GetResourceDefinitionVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceDefinitionVersionResponse' smart constructor.
data GetResourceDefinitionVersionResponse = GetResourceDefinitionVersionResponse'
  { -- | Arn of the resource definition version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource definition version.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the resource definition
    -- version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
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
-- 'arn', 'getResourceDefinitionVersionResponse_arn' - Arn of the resource definition version.
--
-- 'id', 'getResourceDefinitionVersionResponse_id' - The ID of the resource definition version.
--
-- 'creationTimestamp', 'getResourceDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the resource definition
-- version was created.
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
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      version = Prelude.Nothing,
      definition = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Arn of the resource definition version.
getResourceDefinitionVersionResponse_arn :: Lens.Lens' GetResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionVersionResponse_arn = Lens.lens (\GetResourceDefinitionVersionResponse' {arn} -> arn) (\s@GetResourceDefinitionVersionResponse' {} a -> s {arn = a} :: GetResourceDefinitionVersionResponse)

-- | The ID of the resource definition version.
getResourceDefinitionVersionResponse_id :: Lens.Lens' GetResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionVersionResponse_id = Lens.lens (\GetResourceDefinitionVersionResponse' {id} -> id) (\s@GetResourceDefinitionVersionResponse' {} a -> s {id = a} :: GetResourceDefinitionVersionResponse)

-- | The time, in milliseconds since the epoch, when the resource definition
-- version was created.
getResourceDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetResourceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getResourceDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetResourceDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetResourceDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetResourceDefinitionVersionResponse)

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
  where
  rnf GetResourceDefinitionVersionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf httpStatus
