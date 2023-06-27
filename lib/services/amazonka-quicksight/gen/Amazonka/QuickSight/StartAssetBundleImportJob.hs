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
-- Module      : Amazonka.QuickSight.StartAssetBundleImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Asset Bundle import job.
--
-- An Asset Bundle import job imports specified Amazon QuickSight assets
-- into an Amazon QuickSight account. You can also choose to import a
-- naming prefix and specified configuration overrides. The assets that are
-- contained in the bundle file that you provide are used to create or
-- update a new or existing asset in your Amazon QuickSight account. Each
-- Amazon QuickSight account can run up to 10 import jobs concurrently.
--
-- The API caller must have the necessary @\"create\"@, @\"describe\"@, and
-- @\"update\"@ permissions in their IAM role to access each resource type
-- that is contained in the bundle file before the resources can be
-- imported.
module Amazonka.QuickSight.StartAssetBundleImportJob
  ( -- * Creating a Request
    StartAssetBundleImportJob (..),
    newStartAssetBundleImportJob,

    -- * Request Lenses
    startAssetBundleImportJob_failureAction,
    startAssetBundleImportJob_overrideParameters,
    startAssetBundleImportJob_awsAccountId,
    startAssetBundleImportJob_assetBundleImportJobId,
    startAssetBundleImportJob_assetBundleImportSource,

    -- * Destructuring the Response
    StartAssetBundleImportJobResponse (..),
    newStartAssetBundleImportJobResponse,

    -- * Response Lenses
    startAssetBundleImportJobResponse_arn,
    startAssetBundleImportJobResponse_assetBundleImportJobId,
    startAssetBundleImportJobResponse_requestId,
    startAssetBundleImportJobResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartAssetBundleImportJob' smart constructor.
data StartAssetBundleImportJob = StartAssetBundleImportJob'
  { -- | The failure action for the import job.
    --
    -- If you choose @ROLLBACK@, failed import jobs will attempt to undo any
    -- asset changes caused by the failed job.
    --
    -- If you choose @DO_NOTHING@, failed import jobs will not attempt to roll
    -- back any asset changes caused by the failed job, possibly keeping the
    -- Amazon QuickSight account in an inconsistent state.
    failureAction :: Prelude.Maybe AssetBundleImportFailureAction,
    -- | Optional overrides to be applied to the resource configuration before
    -- import.
    overrideParameters :: Prelude.Maybe AssetBundleImportJobOverrideParameters,
    -- | The ID of the Amazon Web Services account to import assets into.
    awsAccountId :: Prelude.Text,
    -- | The ID of the job. This ID is unique while the job is running. After the
    -- job is completed, you can reuse this ID for another job.
    assetBundleImportJobId :: Prelude.Text,
    -- | The source of the asset bundle zip file that contains the data that you
    -- want to import.
    assetBundleImportSource :: AssetBundleImportSource
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAssetBundleImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureAction', 'startAssetBundleImportJob_failureAction' - The failure action for the import job.
--
-- If you choose @ROLLBACK@, failed import jobs will attempt to undo any
-- asset changes caused by the failed job.
--
-- If you choose @DO_NOTHING@, failed import jobs will not attempt to roll
-- back any asset changes caused by the failed job, possibly keeping the
-- Amazon QuickSight account in an inconsistent state.
--
-- 'overrideParameters', 'startAssetBundleImportJob_overrideParameters' - Optional overrides to be applied to the resource configuration before
-- import.
--
-- 'awsAccountId', 'startAssetBundleImportJob_awsAccountId' - The ID of the Amazon Web Services account to import assets into.
--
-- 'assetBundleImportJobId', 'startAssetBundleImportJob_assetBundleImportJobId' - The ID of the job. This ID is unique while the job is running. After the
-- job is completed, you can reuse this ID for another job.
--
-- 'assetBundleImportSource', 'startAssetBundleImportJob_assetBundleImportSource' - The source of the asset bundle zip file that contains the data that you
-- want to import.
newStartAssetBundleImportJob ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'assetBundleImportJobId'
  Prelude.Text ->
  -- | 'assetBundleImportSource'
  AssetBundleImportSource ->
  StartAssetBundleImportJob
newStartAssetBundleImportJob
  pAwsAccountId_
  pAssetBundleImportJobId_
  pAssetBundleImportSource_ =
    StartAssetBundleImportJob'
      { failureAction =
          Prelude.Nothing,
        overrideParameters = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        assetBundleImportJobId =
          pAssetBundleImportJobId_,
        assetBundleImportSource =
          pAssetBundleImportSource_
      }

-- | The failure action for the import job.
--
-- If you choose @ROLLBACK@, failed import jobs will attempt to undo any
-- asset changes caused by the failed job.
--
-- If you choose @DO_NOTHING@, failed import jobs will not attempt to roll
-- back any asset changes caused by the failed job, possibly keeping the
-- Amazon QuickSight account in an inconsistent state.
startAssetBundleImportJob_failureAction :: Lens.Lens' StartAssetBundleImportJob (Prelude.Maybe AssetBundleImportFailureAction)
startAssetBundleImportJob_failureAction = Lens.lens (\StartAssetBundleImportJob' {failureAction} -> failureAction) (\s@StartAssetBundleImportJob' {} a -> s {failureAction = a} :: StartAssetBundleImportJob)

-- | Optional overrides to be applied to the resource configuration before
-- import.
startAssetBundleImportJob_overrideParameters :: Lens.Lens' StartAssetBundleImportJob (Prelude.Maybe AssetBundleImportJobOverrideParameters)
startAssetBundleImportJob_overrideParameters = Lens.lens (\StartAssetBundleImportJob' {overrideParameters} -> overrideParameters) (\s@StartAssetBundleImportJob' {} a -> s {overrideParameters = a} :: StartAssetBundleImportJob)

-- | The ID of the Amazon Web Services account to import assets into.
startAssetBundleImportJob_awsAccountId :: Lens.Lens' StartAssetBundleImportJob Prelude.Text
startAssetBundleImportJob_awsAccountId = Lens.lens (\StartAssetBundleImportJob' {awsAccountId} -> awsAccountId) (\s@StartAssetBundleImportJob' {} a -> s {awsAccountId = a} :: StartAssetBundleImportJob)

-- | The ID of the job. This ID is unique while the job is running. After the
-- job is completed, you can reuse this ID for another job.
startAssetBundleImportJob_assetBundleImportJobId :: Lens.Lens' StartAssetBundleImportJob Prelude.Text
startAssetBundleImportJob_assetBundleImportJobId = Lens.lens (\StartAssetBundleImportJob' {assetBundleImportJobId} -> assetBundleImportJobId) (\s@StartAssetBundleImportJob' {} a -> s {assetBundleImportJobId = a} :: StartAssetBundleImportJob)

-- | The source of the asset bundle zip file that contains the data that you
-- want to import.
startAssetBundleImportJob_assetBundleImportSource :: Lens.Lens' StartAssetBundleImportJob AssetBundleImportSource
startAssetBundleImportJob_assetBundleImportSource = Lens.lens (\StartAssetBundleImportJob' {assetBundleImportSource} -> assetBundleImportSource) (\s@StartAssetBundleImportJob' {} a -> s {assetBundleImportSource = a} :: StartAssetBundleImportJob)

instance Core.AWSRequest StartAssetBundleImportJob where
  type
    AWSResponse StartAssetBundleImportJob =
      StartAssetBundleImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAssetBundleImportJobResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "AssetBundleImportJobId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartAssetBundleImportJob where
  hashWithSalt _salt StartAssetBundleImportJob' {..} =
    _salt
      `Prelude.hashWithSalt` failureAction
      `Prelude.hashWithSalt` overrideParameters
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` assetBundleImportJobId
      `Prelude.hashWithSalt` assetBundleImportSource

instance Prelude.NFData StartAssetBundleImportJob where
  rnf StartAssetBundleImportJob' {..} =
    Prelude.rnf failureAction
      `Prelude.seq` Prelude.rnf overrideParameters
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf assetBundleImportJobId
      `Prelude.seq` Prelude.rnf assetBundleImportSource

instance Data.ToHeaders StartAssetBundleImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartAssetBundleImportJob where
  toJSON StartAssetBundleImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FailureAction" Data..=) Prelude.<$> failureAction,
            ("OverrideParameters" Data..=)
              Prelude.<$> overrideParameters,
            Prelude.Just
              ( "AssetBundleImportJobId"
                  Data..= assetBundleImportJobId
              ),
            Prelude.Just
              ( "AssetBundleImportSource"
                  Data..= assetBundleImportSource
              )
          ]
      )

instance Data.ToPath StartAssetBundleImportJob where
  toPath StartAssetBundleImportJob' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/asset-bundle-import-jobs/import"
      ]

instance Data.ToQuery StartAssetBundleImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAssetBundleImportJobResponse' smart constructor.
data StartAssetBundleImportJobResponse = StartAssetBundleImportJobResponse'
  { -- | The Amazon Resource Name (ARN) for the import job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the job. This ID is unique while the job is running. After the
    -- job is completed, you can reuse this ID for another job.
    assetBundleImportJobId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services response ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the response.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAssetBundleImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'startAssetBundleImportJobResponse_arn' - The Amazon Resource Name (ARN) for the import job.
--
-- 'assetBundleImportJobId', 'startAssetBundleImportJobResponse_assetBundleImportJobId' - The ID of the job. This ID is unique while the job is running. After the
-- job is completed, you can reuse this ID for another job.
--
-- 'requestId', 'startAssetBundleImportJobResponse_requestId' - The Amazon Web Services response ID for this operation.
--
-- 'status', 'startAssetBundleImportJobResponse_status' - The HTTP status of the response.
newStartAssetBundleImportJobResponse ::
  -- | 'status'
  Prelude.Int ->
  StartAssetBundleImportJobResponse
newStartAssetBundleImportJobResponse pStatus_ =
  StartAssetBundleImportJobResponse'
    { arn =
        Prelude.Nothing,
      assetBundleImportJobId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the import job.
startAssetBundleImportJobResponse_arn :: Lens.Lens' StartAssetBundleImportJobResponse (Prelude.Maybe Prelude.Text)
startAssetBundleImportJobResponse_arn = Lens.lens (\StartAssetBundleImportJobResponse' {arn} -> arn) (\s@StartAssetBundleImportJobResponse' {} a -> s {arn = a} :: StartAssetBundleImportJobResponse)

-- | The ID of the job. This ID is unique while the job is running. After the
-- job is completed, you can reuse this ID for another job.
startAssetBundleImportJobResponse_assetBundleImportJobId :: Lens.Lens' StartAssetBundleImportJobResponse (Prelude.Maybe Prelude.Text)
startAssetBundleImportJobResponse_assetBundleImportJobId = Lens.lens (\StartAssetBundleImportJobResponse' {assetBundleImportJobId} -> assetBundleImportJobId) (\s@StartAssetBundleImportJobResponse' {} a -> s {assetBundleImportJobId = a} :: StartAssetBundleImportJobResponse)

-- | The Amazon Web Services response ID for this operation.
startAssetBundleImportJobResponse_requestId :: Lens.Lens' StartAssetBundleImportJobResponse (Prelude.Maybe Prelude.Text)
startAssetBundleImportJobResponse_requestId = Lens.lens (\StartAssetBundleImportJobResponse' {requestId} -> requestId) (\s@StartAssetBundleImportJobResponse' {} a -> s {requestId = a} :: StartAssetBundleImportJobResponse)

-- | The HTTP status of the response.
startAssetBundleImportJobResponse_status :: Lens.Lens' StartAssetBundleImportJobResponse Prelude.Int
startAssetBundleImportJobResponse_status = Lens.lens (\StartAssetBundleImportJobResponse' {status} -> status) (\s@StartAssetBundleImportJobResponse' {} a -> s {status = a} :: StartAssetBundleImportJobResponse)

instance
  Prelude.NFData
    StartAssetBundleImportJobResponse
  where
  rnf StartAssetBundleImportJobResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf assetBundleImportJobId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
