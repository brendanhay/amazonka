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
-- Module      : Amazonka.FSx.CreateDataRepositoryTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon FSx for Lustre data repository task. You use data
-- repository tasks to perform bulk operations between your Amazon FSx file
-- system and its linked data repositories. An example of a data repository
-- task is exporting any data and metadata changes, including POSIX
-- metadata, to files, directories, and symbolic links (symlinks) from your
-- FSx file system to a linked data repository. A
-- @CreateDataRepositoryTask@ operation will fail if a data repository is
-- not linked to the FSx file system. To learn more about data repository
-- tasks, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/data-repository-tasks.html Data Repository Tasks>.
-- To learn more about linking a data repository to your file system, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/create-dra-linked-data-repo.html Linking your file system to an S3 bucket>.
module Amazonka.FSx.CreateDataRepositoryTask
  ( -- * Creating a Request
    CreateDataRepositoryTask (..),
    newCreateDataRepositoryTask,

    -- * Request Lenses
    createDataRepositoryTask_tags,
    createDataRepositoryTask_clientRequestToken,
    createDataRepositoryTask_capacityToRelease,
    createDataRepositoryTask_paths,
    createDataRepositoryTask_type,
    createDataRepositoryTask_fileSystemId,
    createDataRepositoryTask_report,

    -- * Destructuring the Response
    CreateDataRepositoryTaskResponse (..),
    newCreateDataRepositoryTaskResponse,

    -- * Response Lenses
    createDataRepositoryTaskResponse_dataRepositoryTask,
    createDataRepositoryTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataRepositoryTask' smart constructor.
data CreateDataRepositoryTask = CreateDataRepositoryTask'
  { tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the amount of data to release, in GiB, by an Amazon File Cache
    -- @AUTO_RELEASE_DATA@ task that automatically releases files from the
    -- cache.
    capacityToRelease :: Prelude.Maybe Prelude.Natural,
    -- | A list of paths for the data repository task to use when the task is
    -- processed. If a path that you provide isn\'t valid, the task fails.
    --
    -- -   For export tasks, the list contains paths on the Amazon FSx file
    --     system from which the files are exported to the Amazon S3 bucket.
    --     The default path is the file system root directory. The paths you
    --     provide need to be relative to the mount point of the file system.
    --     If the mount point is @\/mnt\/fsx@ and @\/mnt\/fsx\/path1@ is a
    --     directory or file on the file system you want to export, then the
    --     path to provide is @path1@.
    --
    -- -   For import tasks, the list contains paths in the Amazon S3 bucket
    --     from which POSIX metadata changes are imported to the Amazon FSx
    --     file system. The path can be an S3 bucket or prefix in the format
    --     @s3:\/\/myBucket\/myPrefix@ (where @myPrefix@ is optional).
    paths :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the type of data repository task to create.
    type' :: DataRepositoryTaskType,
    fileSystemId :: Prelude.Text,
    -- | Defines whether or not Amazon FSx provides a CompletionReport once the
    -- task has completed. A CompletionReport provides a detailed report on the
    -- files that Amazon FSx processed that meet the criteria specified by the
    -- @Scope@ parameter. For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/task-completion-report.html Working with Task Completion Reports>.
    report :: CompletionReport
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataRepositoryTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDataRepositoryTask_tags' - Undocumented member.
--
-- 'clientRequestToken', 'createDataRepositoryTask_clientRequestToken' - Undocumented member.
--
-- 'capacityToRelease', 'createDataRepositoryTask_capacityToRelease' - Specifies the amount of data to release, in GiB, by an Amazon File Cache
-- @AUTO_RELEASE_DATA@ task that automatically releases files from the
-- cache.
--
-- 'paths', 'createDataRepositoryTask_paths' - A list of paths for the data repository task to use when the task is
-- processed. If a path that you provide isn\'t valid, the task fails.
--
-- -   For export tasks, the list contains paths on the Amazon FSx file
--     system from which the files are exported to the Amazon S3 bucket.
--     The default path is the file system root directory. The paths you
--     provide need to be relative to the mount point of the file system.
--     If the mount point is @\/mnt\/fsx@ and @\/mnt\/fsx\/path1@ is a
--     directory or file on the file system you want to export, then the
--     path to provide is @path1@.
--
-- -   For import tasks, the list contains paths in the Amazon S3 bucket
--     from which POSIX metadata changes are imported to the Amazon FSx
--     file system. The path can be an S3 bucket or prefix in the format
--     @s3:\/\/myBucket\/myPrefix@ (where @myPrefix@ is optional).
--
-- 'type'', 'createDataRepositoryTask_type' - Specifies the type of data repository task to create.
--
-- 'fileSystemId', 'createDataRepositoryTask_fileSystemId' - Undocumented member.
--
-- 'report', 'createDataRepositoryTask_report' - Defines whether or not Amazon FSx provides a CompletionReport once the
-- task has completed. A CompletionReport provides a detailed report on the
-- files that Amazon FSx processed that meet the criteria specified by the
-- @Scope@ parameter. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/task-completion-report.html Working with Task Completion Reports>.
newCreateDataRepositoryTask ::
  -- | 'type''
  DataRepositoryTaskType ->
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'report'
  CompletionReport ->
  CreateDataRepositoryTask
newCreateDataRepositoryTask
  pType_
  pFileSystemId_
  pReport_ =
    CreateDataRepositoryTask'
      { tags = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        capacityToRelease = Prelude.Nothing,
        paths = Prelude.Nothing,
        type' = pType_,
        fileSystemId = pFileSystemId_,
        report = pReport_
      }

-- | Undocumented member.
createDataRepositoryTask_tags :: Lens.Lens' CreateDataRepositoryTask (Prelude.Maybe (Prelude.NonEmpty Tag))
createDataRepositoryTask_tags = Lens.lens (\CreateDataRepositoryTask' {tags} -> tags) (\s@CreateDataRepositoryTask' {} a -> s {tags = a} :: CreateDataRepositoryTask) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createDataRepositoryTask_clientRequestToken :: Lens.Lens' CreateDataRepositoryTask (Prelude.Maybe Prelude.Text)
createDataRepositoryTask_clientRequestToken = Lens.lens (\CreateDataRepositoryTask' {clientRequestToken} -> clientRequestToken) (\s@CreateDataRepositoryTask' {} a -> s {clientRequestToken = a} :: CreateDataRepositoryTask)

-- | Specifies the amount of data to release, in GiB, by an Amazon File Cache
-- @AUTO_RELEASE_DATA@ task that automatically releases files from the
-- cache.
createDataRepositoryTask_capacityToRelease :: Lens.Lens' CreateDataRepositoryTask (Prelude.Maybe Prelude.Natural)
createDataRepositoryTask_capacityToRelease = Lens.lens (\CreateDataRepositoryTask' {capacityToRelease} -> capacityToRelease) (\s@CreateDataRepositoryTask' {} a -> s {capacityToRelease = a} :: CreateDataRepositoryTask)

-- | A list of paths for the data repository task to use when the task is
-- processed. If a path that you provide isn\'t valid, the task fails.
--
-- -   For export tasks, the list contains paths on the Amazon FSx file
--     system from which the files are exported to the Amazon S3 bucket.
--     The default path is the file system root directory. The paths you
--     provide need to be relative to the mount point of the file system.
--     If the mount point is @\/mnt\/fsx@ and @\/mnt\/fsx\/path1@ is a
--     directory or file on the file system you want to export, then the
--     path to provide is @path1@.
--
-- -   For import tasks, the list contains paths in the Amazon S3 bucket
--     from which POSIX metadata changes are imported to the Amazon FSx
--     file system. The path can be an S3 bucket or prefix in the format
--     @s3:\/\/myBucket\/myPrefix@ (where @myPrefix@ is optional).
createDataRepositoryTask_paths :: Lens.Lens' CreateDataRepositoryTask (Prelude.Maybe [Prelude.Text])
createDataRepositoryTask_paths = Lens.lens (\CreateDataRepositoryTask' {paths} -> paths) (\s@CreateDataRepositoryTask' {} a -> s {paths = a} :: CreateDataRepositoryTask) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the type of data repository task to create.
createDataRepositoryTask_type :: Lens.Lens' CreateDataRepositoryTask DataRepositoryTaskType
createDataRepositoryTask_type = Lens.lens (\CreateDataRepositoryTask' {type'} -> type') (\s@CreateDataRepositoryTask' {} a -> s {type' = a} :: CreateDataRepositoryTask)

-- | Undocumented member.
createDataRepositoryTask_fileSystemId :: Lens.Lens' CreateDataRepositoryTask Prelude.Text
createDataRepositoryTask_fileSystemId = Lens.lens (\CreateDataRepositoryTask' {fileSystemId} -> fileSystemId) (\s@CreateDataRepositoryTask' {} a -> s {fileSystemId = a} :: CreateDataRepositoryTask)

-- | Defines whether or not Amazon FSx provides a CompletionReport once the
-- task has completed. A CompletionReport provides a detailed report on the
-- files that Amazon FSx processed that meet the criteria specified by the
-- @Scope@ parameter. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/task-completion-report.html Working with Task Completion Reports>.
createDataRepositoryTask_report :: Lens.Lens' CreateDataRepositoryTask CompletionReport
createDataRepositoryTask_report = Lens.lens (\CreateDataRepositoryTask' {report} -> report) (\s@CreateDataRepositoryTask' {} a -> s {report = a} :: CreateDataRepositoryTask)

instance Core.AWSRequest CreateDataRepositoryTask where
  type
    AWSResponse CreateDataRepositoryTask =
      CreateDataRepositoryTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataRepositoryTaskResponse'
            Prelude.<$> (x Data..?> "DataRepositoryTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataRepositoryTask where
  hashWithSalt _salt CreateDataRepositoryTask' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` capacityToRelease
      `Prelude.hashWithSalt` paths
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` report

instance Prelude.NFData CreateDataRepositoryTask where
  rnf CreateDataRepositoryTask' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf capacityToRelease
      `Prelude.seq` Prelude.rnf paths
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf report

instance Data.ToHeaders CreateDataRepositoryTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.CreateDataRepositoryTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataRepositoryTask where
  toJSON CreateDataRepositoryTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("CapacityToRelease" Data..=)
              Prelude.<$> capacityToRelease,
            ("Paths" Data..=) Prelude.<$> paths,
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("FileSystemId" Data..= fileSystemId),
            Prelude.Just ("Report" Data..= report)
          ]
      )

instance Data.ToPath CreateDataRepositoryTask where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDataRepositoryTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataRepositoryTaskResponse' smart constructor.
data CreateDataRepositoryTaskResponse = CreateDataRepositoryTaskResponse'
  { -- | The description of the data repository task that you just created.
    dataRepositoryTask :: Prelude.Maybe DataRepositoryTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataRepositoryTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataRepositoryTask', 'createDataRepositoryTaskResponse_dataRepositoryTask' - The description of the data repository task that you just created.
--
-- 'httpStatus', 'createDataRepositoryTaskResponse_httpStatus' - The response's http status code.
newCreateDataRepositoryTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataRepositoryTaskResponse
newCreateDataRepositoryTaskResponse pHttpStatus_ =
  CreateDataRepositoryTaskResponse'
    { dataRepositoryTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the data repository task that you just created.
createDataRepositoryTaskResponse_dataRepositoryTask :: Lens.Lens' CreateDataRepositoryTaskResponse (Prelude.Maybe DataRepositoryTask)
createDataRepositoryTaskResponse_dataRepositoryTask = Lens.lens (\CreateDataRepositoryTaskResponse' {dataRepositoryTask} -> dataRepositoryTask) (\s@CreateDataRepositoryTaskResponse' {} a -> s {dataRepositoryTask = a} :: CreateDataRepositoryTaskResponse)

-- | The response's http status code.
createDataRepositoryTaskResponse_httpStatus :: Lens.Lens' CreateDataRepositoryTaskResponse Prelude.Int
createDataRepositoryTaskResponse_httpStatus = Lens.lens (\CreateDataRepositoryTaskResponse' {httpStatus} -> httpStatus) (\s@CreateDataRepositoryTaskResponse' {} a -> s {httpStatus = a} :: CreateDataRepositoryTaskResponse)

instance
  Prelude.NFData
    CreateDataRepositoryTaskResponse
  where
  rnf CreateDataRepositoryTaskResponse' {..} =
    Prelude.rnf dataRepositoryTask
      `Prelude.seq` Prelude.rnf httpStatus
