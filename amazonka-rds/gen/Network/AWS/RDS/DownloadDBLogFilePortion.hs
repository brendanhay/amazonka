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
-- Module      : Network.AWS.RDS.DownloadDBLogFilePortion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads all or a portion of the specified log file, up to 1 MB in
-- size.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DownloadDBLogFilePortion
  ( -- * Creating a Request
    DownloadDBLogFilePortion (..),
    newDownloadDBLogFilePortion,

    -- * Request Lenses
    downloadDBLogFilePortion_numberOfLines,
    downloadDBLogFilePortion_marker,
    downloadDBLogFilePortion_dbInstanceIdentifier,
    downloadDBLogFilePortion_logFileName,

    -- * Destructuring the Response
    DownloadDBLogFilePortionResponse (..),
    newDownloadDBLogFilePortionResponse,

    -- * Response Lenses
    downloadDBLogFilePortionResponse_additionalDataPending,
    downloadDBLogFilePortionResponse_logFileData,
    downloadDBLogFilePortionResponse_marker,
    downloadDBLogFilePortionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDownloadDBLogFilePortion' smart constructor.
data DownloadDBLogFilePortion = DownloadDBLogFilePortion'
  { -- | The number of lines to download. If the number of lines specified
    -- results in a file over 1 MB in size, the file is truncated at 1 MB in
    -- size.
    --
    -- If the NumberOfLines parameter is specified, then the block of lines
    -- returned can be from the beginning or the end of the log file, depending
    -- on the value of the Marker parameter.
    --
    -- -   If neither Marker or NumberOfLines are specified, the entire log
    --     file is returned up to a maximum of 10000 lines, starting with the
    --     most recent log entries first.
    --
    -- -   If NumberOfLines is specified and Marker isn\'t specified, then the
    --     most recent lines from the end of the log file are returned.
    --
    -- -   If Marker is specified as \"0\", then the specified number of lines
    --     from the beginning of the log file are returned.
    --
    -- -   You can download the log file in blocks of lines by specifying the
    --     size of the block using the NumberOfLines parameter, and by
    --     specifying a value of \"0\" for the Marker parameter in your first
    --     request. Include the Marker value returned in the response as the
    --     Marker value for the next request, continuing until the
    --     AdditionalDataPending response element returns false.
    numberOfLines :: Core.Maybe Core.Int,
    -- | The pagination token provided in the previous request or \"0\". If the
    -- Marker parameter is specified the response includes only records beyond
    -- the marker until the end of the file or up to NumberOfLines.
    marker :: Core.Maybe Core.Text,
    -- | The customer-assigned name of the DB instance that contains the log
    -- files you want to list.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBInstance.
    dbInstanceIdentifier :: Core.Text,
    -- | The name of the log file to be downloaded.
    logFileName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DownloadDBLogFilePortion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfLines', 'downloadDBLogFilePortion_numberOfLines' - The number of lines to download. If the number of lines specified
-- results in a file over 1 MB in size, the file is truncated at 1 MB in
-- size.
--
-- If the NumberOfLines parameter is specified, then the block of lines
-- returned can be from the beginning or the end of the log file, depending
-- on the value of the Marker parameter.
--
-- -   If neither Marker or NumberOfLines are specified, the entire log
--     file is returned up to a maximum of 10000 lines, starting with the
--     most recent log entries first.
--
-- -   If NumberOfLines is specified and Marker isn\'t specified, then the
--     most recent lines from the end of the log file are returned.
--
-- -   If Marker is specified as \"0\", then the specified number of lines
--     from the beginning of the log file are returned.
--
-- -   You can download the log file in blocks of lines by specifying the
--     size of the block using the NumberOfLines parameter, and by
--     specifying a value of \"0\" for the Marker parameter in your first
--     request. Include the Marker value returned in the response as the
--     Marker value for the next request, continuing until the
--     AdditionalDataPending response element returns false.
--
-- 'marker', 'downloadDBLogFilePortion_marker' - The pagination token provided in the previous request or \"0\". If the
-- Marker parameter is specified the response includes only records beyond
-- the marker until the end of the file or up to NumberOfLines.
--
-- 'dbInstanceIdentifier', 'downloadDBLogFilePortion_dbInstanceIdentifier' - The customer-assigned name of the DB instance that contains the log
-- files you want to list.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
--
-- 'logFileName', 'downloadDBLogFilePortion_logFileName' - The name of the log file to be downloaded.
newDownloadDBLogFilePortion ::
  -- | 'dbInstanceIdentifier'
  Core.Text ->
  -- | 'logFileName'
  Core.Text ->
  DownloadDBLogFilePortion
newDownloadDBLogFilePortion
  pDBInstanceIdentifier_
  pLogFileName_ =
    DownloadDBLogFilePortion'
      { numberOfLines =
          Core.Nothing,
        marker = Core.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        logFileName = pLogFileName_
      }

-- | The number of lines to download. If the number of lines specified
-- results in a file over 1 MB in size, the file is truncated at 1 MB in
-- size.
--
-- If the NumberOfLines parameter is specified, then the block of lines
-- returned can be from the beginning or the end of the log file, depending
-- on the value of the Marker parameter.
--
-- -   If neither Marker or NumberOfLines are specified, the entire log
--     file is returned up to a maximum of 10000 lines, starting with the
--     most recent log entries first.
--
-- -   If NumberOfLines is specified and Marker isn\'t specified, then the
--     most recent lines from the end of the log file are returned.
--
-- -   If Marker is specified as \"0\", then the specified number of lines
--     from the beginning of the log file are returned.
--
-- -   You can download the log file in blocks of lines by specifying the
--     size of the block using the NumberOfLines parameter, and by
--     specifying a value of \"0\" for the Marker parameter in your first
--     request. Include the Marker value returned in the response as the
--     Marker value for the next request, continuing until the
--     AdditionalDataPending response element returns false.
downloadDBLogFilePortion_numberOfLines :: Lens.Lens' DownloadDBLogFilePortion (Core.Maybe Core.Int)
downloadDBLogFilePortion_numberOfLines = Lens.lens (\DownloadDBLogFilePortion' {numberOfLines} -> numberOfLines) (\s@DownloadDBLogFilePortion' {} a -> s {numberOfLines = a} :: DownloadDBLogFilePortion)

-- | The pagination token provided in the previous request or \"0\". If the
-- Marker parameter is specified the response includes only records beyond
-- the marker until the end of the file or up to NumberOfLines.
downloadDBLogFilePortion_marker :: Lens.Lens' DownloadDBLogFilePortion (Core.Maybe Core.Text)
downloadDBLogFilePortion_marker = Lens.lens (\DownloadDBLogFilePortion' {marker} -> marker) (\s@DownloadDBLogFilePortion' {} a -> s {marker = a} :: DownloadDBLogFilePortion)

-- | The customer-assigned name of the DB instance that contains the log
-- files you want to list.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
downloadDBLogFilePortion_dbInstanceIdentifier :: Lens.Lens' DownloadDBLogFilePortion Core.Text
downloadDBLogFilePortion_dbInstanceIdentifier = Lens.lens (\DownloadDBLogFilePortion' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DownloadDBLogFilePortion' {} a -> s {dbInstanceIdentifier = a} :: DownloadDBLogFilePortion)

-- | The name of the log file to be downloaded.
downloadDBLogFilePortion_logFileName :: Lens.Lens' DownloadDBLogFilePortion Core.Text
downloadDBLogFilePortion_logFileName = Lens.lens (\DownloadDBLogFilePortion' {logFileName} -> logFileName) (\s@DownloadDBLogFilePortion' {} a -> s {logFileName = a} :: DownloadDBLogFilePortion)

instance Core.AWSPager DownloadDBLogFilePortion where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? downloadDBLogFilePortionResponse_additionalDataPending
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? downloadDBLogFilePortionResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& downloadDBLogFilePortion_marker
          Lens..~ rs
          Lens.^? downloadDBLogFilePortionResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DownloadDBLogFilePortion where
  type
    AWSResponse DownloadDBLogFilePortion =
      DownloadDBLogFilePortionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DownloadDBLogFilePortionResult"
      ( \s h x ->
          DownloadDBLogFilePortionResponse'
            Core.<$> (x Core..@? "AdditionalDataPending")
            Core.<*> (x Core..@? "LogFileData")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DownloadDBLogFilePortion

instance Core.NFData DownloadDBLogFilePortion

instance Core.ToHeaders DownloadDBLogFilePortion where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DownloadDBLogFilePortion where
  toPath = Core.const "/"

instance Core.ToQuery DownloadDBLogFilePortion where
  toQuery DownloadDBLogFilePortion' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DownloadDBLogFilePortion" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "NumberOfLines" Core.=: numberOfLines,
        "Marker" Core.=: marker,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier,
        "LogFileName" Core.=: logFileName
      ]

-- | This data type is used as a response element to
-- @DownloadDBLogFilePortion@.
--
-- /See:/ 'newDownloadDBLogFilePortionResponse' smart constructor.
data DownloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse'
  { -- | Boolean value that if true, indicates there is more data to be
    -- downloaded.
    additionalDataPending :: Core.Maybe Core.Bool,
    -- | Entries from the specified log file.
    logFileData :: Core.Maybe Core.Text,
    -- | A pagination token that can be used in a later DownloadDBLogFilePortion
    -- request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DownloadDBLogFilePortionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalDataPending', 'downloadDBLogFilePortionResponse_additionalDataPending' - Boolean value that if true, indicates there is more data to be
-- downloaded.
--
-- 'logFileData', 'downloadDBLogFilePortionResponse_logFileData' - Entries from the specified log file.
--
-- 'marker', 'downloadDBLogFilePortionResponse_marker' - A pagination token that can be used in a later DownloadDBLogFilePortion
-- request.
--
-- 'httpStatus', 'downloadDBLogFilePortionResponse_httpStatus' - The response's http status code.
newDownloadDBLogFilePortionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DownloadDBLogFilePortionResponse
newDownloadDBLogFilePortionResponse pHttpStatus_ =
  DownloadDBLogFilePortionResponse'
    { additionalDataPending =
        Core.Nothing,
      logFileData = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Boolean value that if true, indicates there is more data to be
-- downloaded.
downloadDBLogFilePortionResponse_additionalDataPending :: Lens.Lens' DownloadDBLogFilePortionResponse (Core.Maybe Core.Bool)
downloadDBLogFilePortionResponse_additionalDataPending = Lens.lens (\DownloadDBLogFilePortionResponse' {additionalDataPending} -> additionalDataPending) (\s@DownloadDBLogFilePortionResponse' {} a -> s {additionalDataPending = a} :: DownloadDBLogFilePortionResponse)

-- | Entries from the specified log file.
downloadDBLogFilePortionResponse_logFileData :: Lens.Lens' DownloadDBLogFilePortionResponse (Core.Maybe Core.Text)
downloadDBLogFilePortionResponse_logFileData = Lens.lens (\DownloadDBLogFilePortionResponse' {logFileData} -> logFileData) (\s@DownloadDBLogFilePortionResponse' {} a -> s {logFileData = a} :: DownloadDBLogFilePortionResponse)

-- | A pagination token that can be used in a later DownloadDBLogFilePortion
-- request.
downloadDBLogFilePortionResponse_marker :: Lens.Lens' DownloadDBLogFilePortionResponse (Core.Maybe Core.Text)
downloadDBLogFilePortionResponse_marker = Lens.lens (\DownloadDBLogFilePortionResponse' {marker} -> marker) (\s@DownloadDBLogFilePortionResponse' {} a -> s {marker = a} :: DownloadDBLogFilePortionResponse)

-- | The response's http status code.
downloadDBLogFilePortionResponse_httpStatus :: Lens.Lens' DownloadDBLogFilePortionResponse Core.Int
downloadDBLogFilePortionResponse_httpStatus = Lens.lens (\DownloadDBLogFilePortionResponse' {httpStatus} -> httpStatus) (\s@DownloadDBLogFilePortionResponse' {} a -> s {httpStatus = a} :: DownloadDBLogFilePortionResponse)

instance Core.NFData DownloadDBLogFilePortionResponse
