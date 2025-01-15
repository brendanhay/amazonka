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
-- Module      : Amazonka.KinesisAnalyticsV2.DescribeApplicationSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a snapshot of application state data.
module Amazonka.KinesisAnalyticsV2.DescribeApplicationSnapshot
  ( -- * Creating a Request
    DescribeApplicationSnapshot (..),
    newDescribeApplicationSnapshot,

    -- * Request Lenses
    describeApplicationSnapshot_applicationName,
    describeApplicationSnapshot_snapshotName,

    -- * Destructuring the Response
    DescribeApplicationSnapshotResponse (..),
    newDescribeApplicationSnapshotResponse,

    -- * Response Lenses
    describeApplicationSnapshotResponse_httpStatus,
    describeApplicationSnapshotResponse_snapshotDetails,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeApplicationSnapshot' smart constructor.
data DescribeApplicationSnapshot = DescribeApplicationSnapshot'
  { -- | The name of an existing application.
    applicationName :: Prelude.Text,
    -- | The identifier of an application snapshot. You can retrieve this value
    -- using .
    snapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'describeApplicationSnapshot_applicationName' - The name of an existing application.
--
-- 'snapshotName', 'describeApplicationSnapshot_snapshotName' - The identifier of an application snapshot. You can retrieve this value
-- using .
newDescribeApplicationSnapshot ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'snapshotName'
  Prelude.Text ->
  DescribeApplicationSnapshot
newDescribeApplicationSnapshot
  pApplicationName_
  pSnapshotName_ =
    DescribeApplicationSnapshot'
      { applicationName =
          pApplicationName_,
        snapshotName = pSnapshotName_
      }

-- | The name of an existing application.
describeApplicationSnapshot_applicationName :: Lens.Lens' DescribeApplicationSnapshot Prelude.Text
describeApplicationSnapshot_applicationName = Lens.lens (\DescribeApplicationSnapshot' {applicationName} -> applicationName) (\s@DescribeApplicationSnapshot' {} a -> s {applicationName = a} :: DescribeApplicationSnapshot)

-- | The identifier of an application snapshot. You can retrieve this value
-- using .
describeApplicationSnapshot_snapshotName :: Lens.Lens' DescribeApplicationSnapshot Prelude.Text
describeApplicationSnapshot_snapshotName = Lens.lens (\DescribeApplicationSnapshot' {snapshotName} -> snapshotName) (\s@DescribeApplicationSnapshot' {} a -> s {snapshotName = a} :: DescribeApplicationSnapshot)

instance Core.AWSRequest DescribeApplicationSnapshot where
  type
    AWSResponse DescribeApplicationSnapshot =
      DescribeApplicationSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApplicationSnapshotResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "SnapshotDetails")
      )

instance Prelude.Hashable DescribeApplicationSnapshot where
  hashWithSalt _salt DescribeApplicationSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` snapshotName

instance Prelude.NFData DescribeApplicationSnapshot where
  rnf DescribeApplicationSnapshot' {..} =
    Prelude.rnf applicationName `Prelude.seq`
      Prelude.rnf snapshotName

instance Data.ToHeaders DescribeApplicationSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.DescribeApplicationSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeApplicationSnapshot where
  toJSON DescribeApplicationSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just ("SnapshotName" Data..= snapshotName)
          ]
      )

instance Data.ToPath DescribeApplicationSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeApplicationSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeApplicationSnapshotResponse' smart constructor.
data DescribeApplicationSnapshotResponse = DescribeApplicationSnapshotResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object containing information about the application snapshot.
    snapshotDetails :: SnapshotDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeApplicationSnapshotResponse_httpStatus' - The response's http status code.
--
-- 'snapshotDetails', 'describeApplicationSnapshotResponse_snapshotDetails' - An object containing information about the application snapshot.
newDescribeApplicationSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'snapshotDetails'
  SnapshotDetails ->
  DescribeApplicationSnapshotResponse
newDescribeApplicationSnapshotResponse
  pHttpStatus_
  pSnapshotDetails_ =
    DescribeApplicationSnapshotResponse'
      { httpStatus =
          pHttpStatus_,
        snapshotDetails = pSnapshotDetails_
      }

-- | The response's http status code.
describeApplicationSnapshotResponse_httpStatus :: Lens.Lens' DescribeApplicationSnapshotResponse Prelude.Int
describeApplicationSnapshotResponse_httpStatus = Lens.lens (\DescribeApplicationSnapshotResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationSnapshotResponse' {} a -> s {httpStatus = a} :: DescribeApplicationSnapshotResponse)

-- | An object containing information about the application snapshot.
describeApplicationSnapshotResponse_snapshotDetails :: Lens.Lens' DescribeApplicationSnapshotResponse SnapshotDetails
describeApplicationSnapshotResponse_snapshotDetails = Lens.lens (\DescribeApplicationSnapshotResponse' {snapshotDetails} -> snapshotDetails) (\s@DescribeApplicationSnapshotResponse' {} a -> s {snapshotDetails = a} :: DescribeApplicationSnapshotResponse)

instance
  Prelude.NFData
    DescribeApplicationSnapshotResponse
  where
  rnf DescribeApplicationSnapshotResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf snapshotDetails
