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
-- Module      : Network.AWS.RDS.DescribeDBLogFiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB log files for the DB instance.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBLogFiles
  ( -- * Creating a Request
    DescribeDBLogFiles (..),
    newDescribeDBLogFiles,

    -- * Request Lenses
    describeDBLogFiles_fileSize,
    describeDBLogFiles_filenameContains,
    describeDBLogFiles_filters,
    describeDBLogFiles_fileLastWritten,
    describeDBLogFiles_marker,
    describeDBLogFiles_maxRecords,
    describeDBLogFiles_dbInstanceIdentifier,

    -- * Destructuring the Response
    DescribeDBLogFilesResponse (..),
    newDescribeDBLogFilesResponse,

    -- * Response Lenses
    describeDBLogFilesResponse_describeDBLogFiles,
    describeDBLogFilesResponse_marker,
    describeDBLogFilesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBLogFiles' smart constructor.
data DescribeDBLogFiles = DescribeDBLogFiles'
  { -- | Filters the available log files for files larger than the specified
    -- size.
    fileSize :: Core.Maybe Core.Integer,
    -- | Filters the available log files for log file names that contain the
    -- specified string.
    filenameContains :: Core.Maybe Core.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Core.Maybe [Filter],
    -- | Filters the available log files for files written since the specified
    -- date, in POSIX timestamp format with milliseconds.
    fileLastWritten :: Core.Maybe Core.Integer,
    -- | The pagination token provided in the previous request. If this parameter
    -- is specified the response includes only records beyond the marker, up to
    -- MaxRecords.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxRecords value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    maxRecords :: Core.Maybe Core.Int,
    -- | The customer-assigned name of the DB instance that contains the log
    -- files you want to list.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBInstance.
    dbInstanceIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBLogFiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSize', 'describeDBLogFiles_fileSize' - Filters the available log files for files larger than the specified
-- size.
--
-- 'filenameContains', 'describeDBLogFiles_filenameContains' - Filters the available log files for log file names that contain the
-- specified string.
--
-- 'filters', 'describeDBLogFiles_filters' - This parameter isn\'t currently supported.
--
-- 'fileLastWritten', 'describeDBLogFiles_fileLastWritten' - Filters the available log files for files written since the specified
-- date, in POSIX timestamp format with milliseconds.
--
-- 'marker', 'describeDBLogFiles_marker' - The pagination token provided in the previous request. If this parameter
-- is specified the response includes only records beyond the marker, up to
-- MaxRecords.
--
-- 'maxRecords', 'describeDBLogFiles_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- 'dbInstanceIdentifier', 'describeDBLogFiles_dbInstanceIdentifier' - The customer-assigned name of the DB instance that contains the log
-- files you want to list.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
newDescribeDBLogFiles ::
  -- | 'dbInstanceIdentifier'
  Core.Text ->
  DescribeDBLogFiles
newDescribeDBLogFiles pDBInstanceIdentifier_ =
  DescribeDBLogFiles'
    { fileSize = Core.Nothing,
      filenameContains = Core.Nothing,
      filters = Core.Nothing,
      fileLastWritten = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | Filters the available log files for files larger than the specified
-- size.
describeDBLogFiles_fileSize :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Integer)
describeDBLogFiles_fileSize = Lens.lens (\DescribeDBLogFiles' {fileSize} -> fileSize) (\s@DescribeDBLogFiles' {} a -> s {fileSize = a} :: DescribeDBLogFiles)

-- | Filters the available log files for log file names that contain the
-- specified string.
describeDBLogFiles_filenameContains :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Text)
describeDBLogFiles_filenameContains = Lens.lens (\DescribeDBLogFiles' {filenameContains} -> filenameContains) (\s@DescribeDBLogFiles' {} a -> s {filenameContains = a} :: DescribeDBLogFiles)

-- | This parameter isn\'t currently supported.
describeDBLogFiles_filters :: Lens.Lens' DescribeDBLogFiles (Core.Maybe [Filter])
describeDBLogFiles_filters = Lens.lens (\DescribeDBLogFiles' {filters} -> filters) (\s@DescribeDBLogFiles' {} a -> s {filters = a} :: DescribeDBLogFiles) Core.. Lens.mapping Lens._Coerce

-- | Filters the available log files for files written since the specified
-- date, in POSIX timestamp format with milliseconds.
describeDBLogFiles_fileLastWritten :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Integer)
describeDBLogFiles_fileLastWritten = Lens.lens (\DescribeDBLogFiles' {fileLastWritten} -> fileLastWritten) (\s@DescribeDBLogFiles' {} a -> s {fileLastWritten = a} :: DescribeDBLogFiles)

-- | The pagination token provided in the previous request. If this parameter
-- is specified the response includes only records beyond the marker, up to
-- MaxRecords.
describeDBLogFiles_marker :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Text)
describeDBLogFiles_marker = Lens.lens (\DescribeDBLogFiles' {marker} -> marker) (\s@DescribeDBLogFiles' {} a -> s {marker = a} :: DescribeDBLogFiles)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
describeDBLogFiles_maxRecords :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Int)
describeDBLogFiles_maxRecords = Lens.lens (\DescribeDBLogFiles' {maxRecords} -> maxRecords) (\s@DescribeDBLogFiles' {} a -> s {maxRecords = a} :: DescribeDBLogFiles)

-- | The customer-assigned name of the DB instance that contains the log
-- files you want to list.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
describeDBLogFiles_dbInstanceIdentifier :: Lens.Lens' DescribeDBLogFiles Core.Text
describeDBLogFiles_dbInstanceIdentifier = Lens.lens (\DescribeDBLogFiles' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DescribeDBLogFiles' {} a -> s {dbInstanceIdentifier = a} :: DescribeDBLogFiles)

instance Core.AWSPager DescribeDBLogFiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBLogFilesResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBLogFilesResponse_describeDBLogFiles
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBLogFiles_marker
          Lens..~ rs
          Lens.^? describeDBLogFilesResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeDBLogFiles where
  type
    AWSResponse DescribeDBLogFiles =
      DescribeDBLogFilesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBLogFilesResult"
      ( \s h x ->
          DescribeDBLogFilesResponse'
            Core.<$> ( x Core..@? "DescribeDBLogFiles" Core..!@ Core.mempty
                         Core.>>= Core.may
                           (Core.parseXMLList "DescribeDBLogFilesDetails")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBLogFiles

instance Core.NFData DescribeDBLogFiles

instance Core.ToHeaders DescribeDBLogFiles where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBLogFiles where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBLogFiles where
  toQuery DescribeDBLogFiles' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBLogFiles" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "FileSize" Core.=: fileSize,
        "FilenameContains" Core.=: filenameContains,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "FileLastWritten" Core.=: fileLastWritten,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier
      ]

-- | The response from a call to @DescribeDBLogFiles@.
--
-- /See:/ 'newDescribeDBLogFilesResponse' smart constructor.
data DescribeDBLogFilesResponse = DescribeDBLogFilesResponse'
  { -- | The DB log files returned.
    describeDBLogFiles :: Core.Maybe [DescribeDBLogFilesDetails],
    -- | A pagination token that can be used in a later DescribeDBLogFiles
    -- request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBLogFilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'describeDBLogFiles', 'describeDBLogFilesResponse_describeDBLogFiles' - The DB log files returned.
--
-- 'marker', 'describeDBLogFilesResponse_marker' - A pagination token that can be used in a later DescribeDBLogFiles
-- request.
--
-- 'httpStatus', 'describeDBLogFilesResponse_httpStatus' - The response's http status code.
newDescribeDBLogFilesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBLogFilesResponse
newDescribeDBLogFilesResponse pHttpStatus_ =
  DescribeDBLogFilesResponse'
    { describeDBLogFiles =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The DB log files returned.
describeDBLogFilesResponse_describeDBLogFiles :: Lens.Lens' DescribeDBLogFilesResponse (Core.Maybe [DescribeDBLogFilesDetails])
describeDBLogFilesResponse_describeDBLogFiles = Lens.lens (\DescribeDBLogFilesResponse' {describeDBLogFiles} -> describeDBLogFiles) (\s@DescribeDBLogFilesResponse' {} a -> s {describeDBLogFiles = a} :: DescribeDBLogFilesResponse) Core.. Lens.mapping Lens._Coerce

-- | A pagination token that can be used in a later DescribeDBLogFiles
-- request.
describeDBLogFilesResponse_marker :: Lens.Lens' DescribeDBLogFilesResponse (Core.Maybe Core.Text)
describeDBLogFilesResponse_marker = Lens.lens (\DescribeDBLogFilesResponse' {marker} -> marker) (\s@DescribeDBLogFilesResponse' {} a -> s {marker = a} :: DescribeDBLogFilesResponse)

-- | The response's http status code.
describeDBLogFilesResponse_httpStatus :: Lens.Lens' DescribeDBLogFilesResponse Core.Int
describeDBLogFilesResponse_httpStatus = Lens.lens (\DescribeDBLogFilesResponse' {httpStatus} -> httpStatus) (\s@DescribeDBLogFilesResponse' {} a -> s {httpStatus = a} :: DescribeDBLogFilesResponse)

instance Core.NFData DescribeDBLogFilesResponse
