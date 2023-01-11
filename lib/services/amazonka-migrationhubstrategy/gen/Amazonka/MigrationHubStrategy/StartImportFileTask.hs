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
-- Module      : Amazonka.MigrationHubStrategy.StartImportFileTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a file import.
module Amazonka.MigrationHubStrategy.StartImportFileTask
  ( -- * Creating a Request
    StartImportFileTask (..),
    newStartImportFileTask,

    -- * Request Lenses
    startImportFileTask_dataSourceType,
    startImportFileTask_groupId,
    startImportFileTask_s3bucketForReportData,
    startImportFileTask_s3Bucket,
    startImportFileTask_name,
    startImportFileTask_s3key,

    -- * Destructuring the Response
    StartImportFileTaskResponse (..),
    newStartImportFileTaskResponse,

    -- * Response Lenses
    startImportFileTaskResponse_id,
    startImportFileTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartImportFileTask' smart constructor.
data StartImportFileTask = StartImportFileTask'
  { -- | Specifies the source that the servers are coming from. By default,
    -- Strategy Recommendations assumes that the servers specified in the
    -- import file are available in AWS Application Discovery Service.
    dataSourceType :: Prelude.Maybe DataSourceType,
    -- | Groups the resources in the import file together with a unique name.
    -- This ID can be as filter in @ListApplicationComponents@ and
    -- @ListServers@.
    groupId :: Prelude.Maybe [Group],
    -- | The S3 bucket where Strategy Recommendations uploads import results. The
    -- bucket name is required to begin with migrationhub-strategy-.
    s3bucketForReportData :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket where the import file is located. The bucket name is
    -- required to begin with @migrationhub-strategy-@.
    s3Bucket :: Prelude.Text,
    -- | A descriptive name for the request.
    name :: Prelude.Text,
    -- | The Amazon S3 key name of the import file.
    s3key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImportFileTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceType', 'startImportFileTask_dataSourceType' - Specifies the source that the servers are coming from. By default,
-- Strategy Recommendations assumes that the servers specified in the
-- import file are available in AWS Application Discovery Service.
--
-- 'groupId', 'startImportFileTask_groupId' - Groups the resources in the import file together with a unique name.
-- This ID can be as filter in @ListApplicationComponents@ and
-- @ListServers@.
--
-- 's3bucketForReportData', 'startImportFileTask_s3bucketForReportData' - The S3 bucket where Strategy Recommendations uploads import results. The
-- bucket name is required to begin with migrationhub-strategy-.
--
-- 's3Bucket', 'startImportFileTask_s3Bucket' - The S3 bucket where the import file is located. The bucket name is
-- required to begin with @migrationhub-strategy-@.
--
-- 'name', 'startImportFileTask_name' - A descriptive name for the request.
--
-- 's3key', 'startImportFileTask_s3key' - The Amazon S3 key name of the import file.
newStartImportFileTask ::
  -- | 's3Bucket'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 's3key'
  Prelude.Text ->
  StartImportFileTask
newStartImportFileTask pS3Bucket_ pName_ pS3key_ =
  StartImportFileTask'
    { dataSourceType =
        Prelude.Nothing,
      groupId = Prelude.Nothing,
      s3bucketForReportData = Prelude.Nothing,
      s3Bucket = pS3Bucket_,
      name = pName_,
      s3key = pS3key_
    }

-- | Specifies the source that the servers are coming from. By default,
-- Strategy Recommendations assumes that the servers specified in the
-- import file are available in AWS Application Discovery Service.
startImportFileTask_dataSourceType :: Lens.Lens' StartImportFileTask (Prelude.Maybe DataSourceType)
startImportFileTask_dataSourceType = Lens.lens (\StartImportFileTask' {dataSourceType} -> dataSourceType) (\s@StartImportFileTask' {} a -> s {dataSourceType = a} :: StartImportFileTask)

-- | Groups the resources in the import file together with a unique name.
-- This ID can be as filter in @ListApplicationComponents@ and
-- @ListServers@.
startImportFileTask_groupId :: Lens.Lens' StartImportFileTask (Prelude.Maybe [Group])
startImportFileTask_groupId = Lens.lens (\StartImportFileTask' {groupId} -> groupId) (\s@StartImportFileTask' {} a -> s {groupId = a} :: StartImportFileTask) Prelude.. Lens.mapping Lens.coerced

-- | The S3 bucket where Strategy Recommendations uploads import results. The
-- bucket name is required to begin with migrationhub-strategy-.
startImportFileTask_s3bucketForReportData :: Lens.Lens' StartImportFileTask (Prelude.Maybe Prelude.Text)
startImportFileTask_s3bucketForReportData = Lens.lens (\StartImportFileTask' {s3bucketForReportData} -> s3bucketForReportData) (\s@StartImportFileTask' {} a -> s {s3bucketForReportData = a} :: StartImportFileTask)

-- | The S3 bucket where the import file is located. The bucket name is
-- required to begin with @migrationhub-strategy-@.
startImportFileTask_s3Bucket :: Lens.Lens' StartImportFileTask Prelude.Text
startImportFileTask_s3Bucket = Lens.lens (\StartImportFileTask' {s3Bucket} -> s3Bucket) (\s@StartImportFileTask' {} a -> s {s3Bucket = a} :: StartImportFileTask)

-- | A descriptive name for the request.
startImportFileTask_name :: Lens.Lens' StartImportFileTask Prelude.Text
startImportFileTask_name = Lens.lens (\StartImportFileTask' {name} -> name) (\s@StartImportFileTask' {} a -> s {name = a} :: StartImportFileTask)

-- | The Amazon S3 key name of the import file.
startImportFileTask_s3key :: Lens.Lens' StartImportFileTask Prelude.Text
startImportFileTask_s3key = Lens.lens (\StartImportFileTask' {s3key} -> s3key) (\s@StartImportFileTask' {} a -> s {s3key = a} :: StartImportFileTask)

instance Core.AWSRequest StartImportFileTask where
  type
    AWSResponse StartImportFileTask =
      StartImportFileTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImportFileTaskResponse'
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartImportFileTask where
  hashWithSalt _salt StartImportFileTask' {..} =
    _salt `Prelude.hashWithSalt` dataSourceType
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` s3bucketForReportData
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` s3key

instance Prelude.NFData StartImportFileTask where
  rnf StartImportFileTask' {..} =
    Prelude.rnf dataSourceType
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf s3bucketForReportData
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf s3key

instance Data.ToHeaders StartImportFileTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartImportFileTask where
  toJSON StartImportFileTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataSourceType" Data..=)
              Prelude.<$> dataSourceType,
            ("groupId" Data..=) Prelude.<$> groupId,
            ("s3bucketForReportData" Data..=)
              Prelude.<$> s3bucketForReportData,
            Prelude.Just ("S3Bucket" Data..= s3Bucket),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("s3key" Data..= s3key)
          ]
      )

instance Data.ToPath StartImportFileTask where
  toPath = Prelude.const "/start-import-file-task"

instance Data.ToQuery StartImportFileTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartImportFileTaskResponse' smart constructor.
data StartImportFileTaskResponse = StartImportFileTaskResponse'
  { -- | The ID for a specific import task. The ID is unique within an AWS
    -- account.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImportFileTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'startImportFileTaskResponse_id' - The ID for a specific import task. The ID is unique within an AWS
-- account.
--
-- 'httpStatus', 'startImportFileTaskResponse_httpStatus' - The response's http status code.
newStartImportFileTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartImportFileTaskResponse
newStartImportFileTaskResponse pHttpStatus_ =
  StartImportFileTaskResponse'
    { id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID for a specific import task. The ID is unique within an AWS
-- account.
startImportFileTaskResponse_id :: Lens.Lens' StartImportFileTaskResponse (Prelude.Maybe Prelude.Text)
startImportFileTaskResponse_id = Lens.lens (\StartImportFileTaskResponse' {id} -> id) (\s@StartImportFileTaskResponse' {} a -> s {id = a} :: StartImportFileTaskResponse)

-- | The response's http status code.
startImportFileTaskResponse_httpStatus :: Lens.Lens' StartImportFileTaskResponse Prelude.Int
startImportFileTaskResponse_httpStatus = Lens.lens (\StartImportFileTaskResponse' {httpStatus} -> httpStatus) (\s@StartImportFileTaskResponse' {} a -> s {httpStatus = a} :: StartImportFileTaskResponse)

instance Prelude.NFData StartImportFileTaskResponse where
  rnf StartImportFileTaskResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus
