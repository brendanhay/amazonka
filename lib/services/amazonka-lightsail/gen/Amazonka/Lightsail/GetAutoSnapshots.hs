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
-- Module      : Amazonka.Lightsail.GetAutoSnapshots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the available automatic snapshots for an instance or disk. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.GetAutoSnapshots
  ( -- * Creating a Request
    GetAutoSnapshots (..),
    newGetAutoSnapshots,

    -- * Request Lenses
    getAutoSnapshots_resourceName,

    -- * Destructuring the Response
    GetAutoSnapshotsResponse (..),
    newGetAutoSnapshotsResponse,

    -- * Response Lenses
    getAutoSnapshotsResponse_autoSnapshots,
    getAutoSnapshotsResponse_resourceName,
    getAutoSnapshotsResponse_resourceType,
    getAutoSnapshotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAutoSnapshots' smart constructor.
data GetAutoSnapshots = GetAutoSnapshots'
  { -- | The name of the source instance or disk from which to get automatic
    -- snapshot information.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAutoSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'getAutoSnapshots_resourceName' - The name of the source instance or disk from which to get automatic
-- snapshot information.
newGetAutoSnapshots ::
  -- | 'resourceName'
  Prelude.Text ->
  GetAutoSnapshots
newGetAutoSnapshots pResourceName_ =
  GetAutoSnapshots' {resourceName = pResourceName_}

-- | The name of the source instance or disk from which to get automatic
-- snapshot information.
getAutoSnapshots_resourceName :: Lens.Lens' GetAutoSnapshots Prelude.Text
getAutoSnapshots_resourceName = Lens.lens (\GetAutoSnapshots' {resourceName} -> resourceName) (\s@GetAutoSnapshots' {} a -> s {resourceName = a} :: GetAutoSnapshots)

instance Core.AWSRequest GetAutoSnapshots where
  type
    AWSResponse GetAutoSnapshots =
      GetAutoSnapshotsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAutoSnapshotsResponse'
            Prelude.<$> (x Data..?> "autoSnapshots" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "resourceName")
            Prelude.<*> (x Data..?> "resourceType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAutoSnapshots where
  hashWithSalt _salt GetAutoSnapshots' {..} =
    _salt `Prelude.hashWithSalt` resourceName

instance Prelude.NFData GetAutoSnapshots where
  rnf GetAutoSnapshots' {..} = Prelude.rnf resourceName

instance Data.ToHeaders GetAutoSnapshots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetAutoSnapshots" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAutoSnapshots where
  toJSON GetAutoSnapshots' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("resourceName" Data..= resourceName)]
      )

instance Data.ToPath GetAutoSnapshots where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAutoSnapshots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAutoSnapshotsResponse' smart constructor.
data GetAutoSnapshotsResponse = GetAutoSnapshotsResponse'
  { -- | An array of objects that describe the automatic snapshots that are
    -- available for the specified source instance or disk.
    autoSnapshots :: Prelude.Maybe [AutoSnapshotDetails],
    -- | The name of the source instance or disk for the automatic snapshots.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The resource type (e.g., @Instance@ or @Disk@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAutoSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoSnapshots', 'getAutoSnapshotsResponse_autoSnapshots' - An array of objects that describe the automatic snapshots that are
-- available for the specified source instance or disk.
--
-- 'resourceName', 'getAutoSnapshotsResponse_resourceName' - The name of the source instance or disk for the automatic snapshots.
--
-- 'resourceType', 'getAutoSnapshotsResponse_resourceType' - The resource type (e.g., @Instance@ or @Disk@).
--
-- 'httpStatus', 'getAutoSnapshotsResponse_httpStatus' - The response's http status code.
newGetAutoSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAutoSnapshotsResponse
newGetAutoSnapshotsResponse pHttpStatus_ =
  GetAutoSnapshotsResponse'
    { autoSnapshots =
        Prelude.Nothing,
      resourceName = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the automatic snapshots that are
-- available for the specified source instance or disk.
getAutoSnapshotsResponse_autoSnapshots :: Lens.Lens' GetAutoSnapshotsResponse (Prelude.Maybe [AutoSnapshotDetails])
getAutoSnapshotsResponse_autoSnapshots = Lens.lens (\GetAutoSnapshotsResponse' {autoSnapshots} -> autoSnapshots) (\s@GetAutoSnapshotsResponse' {} a -> s {autoSnapshots = a} :: GetAutoSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the source instance or disk for the automatic snapshots.
getAutoSnapshotsResponse_resourceName :: Lens.Lens' GetAutoSnapshotsResponse (Prelude.Maybe Prelude.Text)
getAutoSnapshotsResponse_resourceName = Lens.lens (\GetAutoSnapshotsResponse' {resourceName} -> resourceName) (\s@GetAutoSnapshotsResponse' {} a -> s {resourceName = a} :: GetAutoSnapshotsResponse)

-- | The resource type (e.g., @Instance@ or @Disk@).
getAutoSnapshotsResponse_resourceType :: Lens.Lens' GetAutoSnapshotsResponse (Prelude.Maybe ResourceType)
getAutoSnapshotsResponse_resourceType = Lens.lens (\GetAutoSnapshotsResponse' {resourceType} -> resourceType) (\s@GetAutoSnapshotsResponse' {} a -> s {resourceType = a} :: GetAutoSnapshotsResponse)

-- | The response's http status code.
getAutoSnapshotsResponse_httpStatus :: Lens.Lens' GetAutoSnapshotsResponse Prelude.Int
getAutoSnapshotsResponse_httpStatus = Lens.lens (\GetAutoSnapshotsResponse' {httpStatus} -> httpStatus) (\s@GetAutoSnapshotsResponse' {} a -> s {httpStatus = a} :: GetAutoSnapshotsResponse)

instance Prelude.NFData GetAutoSnapshotsResponse where
  rnf GetAutoSnapshotsResponse' {..} =
    Prelude.rnf autoSnapshots
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf httpStatus
