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
-- Module      : Amazonka.KinesisAnalyticsV2.CreateApplicationSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of the application\'s state data.
module Amazonka.KinesisAnalyticsV2.CreateApplicationSnapshot
  ( -- * Creating a Request
    CreateApplicationSnapshot (..),
    newCreateApplicationSnapshot,

    -- * Request Lenses
    createApplicationSnapshot_applicationName,
    createApplicationSnapshot_snapshotName,

    -- * Destructuring the Response
    CreateApplicationSnapshotResponse (..),
    newCreateApplicationSnapshotResponse,

    -- * Response Lenses
    createApplicationSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApplicationSnapshot' smart constructor.
data CreateApplicationSnapshot = CreateApplicationSnapshot'
  { -- | The name of an existing application
    applicationName :: Prelude.Text,
    -- | An identifier for the application snapshot.
    snapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'createApplicationSnapshot_applicationName' - The name of an existing application
--
-- 'snapshotName', 'createApplicationSnapshot_snapshotName' - An identifier for the application snapshot.
newCreateApplicationSnapshot ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'snapshotName'
  Prelude.Text ->
  CreateApplicationSnapshot
newCreateApplicationSnapshot
  pApplicationName_
  pSnapshotName_ =
    CreateApplicationSnapshot'
      { applicationName =
          pApplicationName_,
        snapshotName = pSnapshotName_
      }

-- | The name of an existing application
createApplicationSnapshot_applicationName :: Lens.Lens' CreateApplicationSnapshot Prelude.Text
createApplicationSnapshot_applicationName = Lens.lens (\CreateApplicationSnapshot' {applicationName} -> applicationName) (\s@CreateApplicationSnapshot' {} a -> s {applicationName = a} :: CreateApplicationSnapshot)

-- | An identifier for the application snapshot.
createApplicationSnapshot_snapshotName :: Lens.Lens' CreateApplicationSnapshot Prelude.Text
createApplicationSnapshot_snapshotName = Lens.lens (\CreateApplicationSnapshot' {snapshotName} -> snapshotName) (\s@CreateApplicationSnapshot' {} a -> s {snapshotName = a} :: CreateApplicationSnapshot)

instance Core.AWSRequest CreateApplicationSnapshot where
  type
    AWSResponse CreateApplicationSnapshot =
      CreateApplicationSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateApplicationSnapshotResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApplicationSnapshot where
  hashWithSalt _salt CreateApplicationSnapshot' {..} =
    _salt `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` snapshotName

instance Prelude.NFData CreateApplicationSnapshot where
  rnf CreateApplicationSnapshot' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf snapshotName

instance Core.ToHeaders CreateApplicationSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20180523.CreateApplicationSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateApplicationSnapshot where
  toJSON CreateApplicationSnapshot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Core..= applicationName),
            Prelude.Just ("SnapshotName" Core..= snapshotName)
          ]
      )

instance Core.ToPath CreateApplicationSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateApplicationSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApplicationSnapshotResponse' smart constructor.
data CreateApplicationSnapshotResponse = CreateApplicationSnapshotResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createApplicationSnapshotResponse_httpStatus' - The response's http status code.
newCreateApplicationSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateApplicationSnapshotResponse
newCreateApplicationSnapshotResponse pHttpStatus_ =
  CreateApplicationSnapshotResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createApplicationSnapshotResponse_httpStatus :: Lens.Lens' CreateApplicationSnapshotResponse Prelude.Int
createApplicationSnapshotResponse_httpStatus = Lens.lens (\CreateApplicationSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationSnapshotResponse' {} a -> s {httpStatus = a} :: CreateApplicationSnapshotResponse)

instance
  Prelude.NFData
    CreateApplicationSnapshotResponse
  where
  rnf CreateApplicationSnapshotResponse' {..} =
    Prelude.rnf httpStatus
