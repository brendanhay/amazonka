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
-- Module      : Amazonka.Greengrass.GetGroupVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a group version.
module Amazonka.Greengrass.GetGroupVersion
  ( -- * Creating a Request
    GetGroupVersion (..),
    newGetGroupVersion,

    -- * Request Lenses
    getGroupVersion_groupVersionId,
    getGroupVersion_groupId,

    -- * Destructuring the Response
    GetGroupVersionResponse (..),
    newGetGroupVersionResponse,

    -- * Response Lenses
    getGroupVersionResponse_arn,
    getGroupVersionResponse_id,
    getGroupVersionResponse_creationTimestamp,
    getGroupVersionResponse_version,
    getGroupVersionResponse_definition,
    getGroupVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGroupVersion' smart constructor.
data GetGroupVersion = GetGroupVersion'
  { -- | The ID of the group version. This value maps to the \'\'Version\'\'
    -- property of the corresponding \'\'VersionInformation\'\' object, which
    -- is returned by \'\'ListGroupVersions\'\' requests. If the version is the
    -- last one that was associated with a group, the value also maps to the
    -- \'\'LatestVersion\'\' property of the corresponding
    -- \'\'GroupInformation\'\' object.
    groupVersionId :: Prelude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroupVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupVersionId', 'getGroupVersion_groupVersionId' - The ID of the group version. This value maps to the \'\'Version\'\'
-- property of the corresponding \'\'VersionInformation\'\' object, which
-- is returned by \'\'ListGroupVersions\'\' requests. If the version is the
-- last one that was associated with a group, the value also maps to the
-- \'\'LatestVersion\'\' property of the corresponding
-- \'\'GroupInformation\'\' object.
--
-- 'groupId', 'getGroupVersion_groupId' - The ID of the Greengrass group.
newGetGroupVersion ::
  -- | 'groupVersionId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  GetGroupVersion
newGetGroupVersion pGroupVersionId_ pGroupId_ =
  GetGroupVersion'
    { groupVersionId = pGroupVersionId_,
      groupId = pGroupId_
    }

-- | The ID of the group version. This value maps to the \'\'Version\'\'
-- property of the corresponding \'\'VersionInformation\'\' object, which
-- is returned by \'\'ListGroupVersions\'\' requests. If the version is the
-- last one that was associated with a group, the value also maps to the
-- \'\'LatestVersion\'\' property of the corresponding
-- \'\'GroupInformation\'\' object.
getGroupVersion_groupVersionId :: Lens.Lens' GetGroupVersion Prelude.Text
getGroupVersion_groupVersionId = Lens.lens (\GetGroupVersion' {groupVersionId} -> groupVersionId) (\s@GetGroupVersion' {} a -> s {groupVersionId = a} :: GetGroupVersion)

-- | The ID of the Greengrass group.
getGroupVersion_groupId :: Lens.Lens' GetGroupVersion Prelude.Text
getGroupVersion_groupId = Lens.lens (\GetGroupVersion' {groupId} -> groupId) (\s@GetGroupVersion' {} a -> s {groupId = a} :: GetGroupVersion)

instance Core.AWSRequest GetGroupVersion where
  type
    AWSResponse GetGroupVersion =
      GetGroupVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupVersionResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "Version")
            Prelude.<*> (x Core..?> "Definition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGroupVersion where
  hashWithSalt _salt GetGroupVersion' {..} =
    _salt `Prelude.hashWithSalt` groupVersionId
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData GetGroupVersion where
  rnf GetGroupVersion' {..} =
    Prelude.rnf groupVersionId
      `Prelude.seq` Prelude.rnf groupId

instance Core.ToHeaders GetGroupVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetGroupVersion where
  toPath GetGroupVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Core.toBS groupId,
        "/versions/",
        Core.toBS groupVersionId
      ]

instance Core.ToQuery GetGroupVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGroupVersionResponse' smart constructor.
data GetGroupVersionResponse = GetGroupVersionResponse'
  { -- | The ARN of the group version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the group that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the group version was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the group version.
    version :: Prelude.Maybe Prelude.Text,
    -- | Information about the group version definition.
    definition :: Prelude.Maybe GroupVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroupVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getGroupVersionResponse_arn' - The ARN of the group version.
--
-- 'id', 'getGroupVersionResponse_id' - The ID of the group that the version is associated with.
--
-- 'creationTimestamp', 'getGroupVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the group version was
-- created.
--
-- 'version', 'getGroupVersionResponse_version' - The ID of the group version.
--
-- 'definition', 'getGroupVersionResponse_definition' - Information about the group version definition.
--
-- 'httpStatus', 'getGroupVersionResponse_httpStatus' - The response's http status code.
newGetGroupVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGroupVersionResponse
newGetGroupVersionResponse pHttpStatus_ =
  GetGroupVersionResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      version = Prelude.Nothing,
      definition = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the group version.
getGroupVersionResponse_arn :: Lens.Lens' GetGroupVersionResponse (Prelude.Maybe Prelude.Text)
getGroupVersionResponse_arn = Lens.lens (\GetGroupVersionResponse' {arn} -> arn) (\s@GetGroupVersionResponse' {} a -> s {arn = a} :: GetGroupVersionResponse)

-- | The ID of the group that the version is associated with.
getGroupVersionResponse_id :: Lens.Lens' GetGroupVersionResponse (Prelude.Maybe Prelude.Text)
getGroupVersionResponse_id = Lens.lens (\GetGroupVersionResponse' {id} -> id) (\s@GetGroupVersionResponse' {} a -> s {id = a} :: GetGroupVersionResponse)

-- | The time, in milliseconds since the epoch, when the group version was
-- created.
getGroupVersionResponse_creationTimestamp :: Lens.Lens' GetGroupVersionResponse (Prelude.Maybe Prelude.Text)
getGroupVersionResponse_creationTimestamp = Lens.lens (\GetGroupVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetGroupVersionResponse' {} a -> s {creationTimestamp = a} :: GetGroupVersionResponse)

-- | The ID of the group version.
getGroupVersionResponse_version :: Lens.Lens' GetGroupVersionResponse (Prelude.Maybe Prelude.Text)
getGroupVersionResponse_version = Lens.lens (\GetGroupVersionResponse' {version} -> version) (\s@GetGroupVersionResponse' {} a -> s {version = a} :: GetGroupVersionResponse)

-- | Information about the group version definition.
getGroupVersionResponse_definition :: Lens.Lens' GetGroupVersionResponse (Prelude.Maybe GroupVersion)
getGroupVersionResponse_definition = Lens.lens (\GetGroupVersionResponse' {definition} -> definition) (\s@GetGroupVersionResponse' {} a -> s {definition = a} :: GetGroupVersionResponse)

-- | The response's http status code.
getGroupVersionResponse_httpStatus :: Lens.Lens' GetGroupVersionResponse Prelude.Int
getGroupVersionResponse_httpStatus = Lens.lens (\GetGroupVersionResponse' {httpStatus} -> httpStatus) (\s@GetGroupVersionResponse' {} a -> s {httpStatus = a} :: GetGroupVersionResponse)

instance Prelude.NFData GetGroupVersionResponse where
  rnf GetGroupVersionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf httpStatus
