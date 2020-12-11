{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DownloadDBLogFilePortion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads all or a portion of the specified log file, up to 1 MB in size.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DownloadDBLogFilePortion
  ( -- * Creating a request
    DownloadDBLogFilePortion (..),
    mkDownloadDBLogFilePortion,

    -- ** Request lenses
    ddlfpNumberOfLines,
    ddlfpMarker,
    ddlfpDBInstanceIdentifier,
    ddlfpLogFileName,

    -- * Destructuring the response
    DownloadDBLogFilePortionResponse (..),
    mkDownloadDBLogFilePortionResponse,

    -- ** Response lenses
    ddlfprsLogFileData,
    ddlfprsAdditionalDataPending,
    ddlfprsMarker,
    ddlfprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDownloadDBLogFilePortion' smart constructor.
data DownloadDBLogFilePortion = DownloadDBLogFilePortion'
  { numberOfLines ::
      Lude.Maybe Lude.Int,
    marker :: Lude.Maybe Lude.Text,
    dbInstanceIdentifier :: Lude.Text,
    logFileName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DownloadDBLogFilePortion' with the minimum fields required to make a request.
--
-- * 'dbInstanceIdentifier' - The customer-assigned name of the DB instance that contains the log files you want to list.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
-- * 'logFileName' - The name of the log file to be downloaded.
-- * 'marker' - The pagination token provided in the previous request or "0". If the Marker parameter is specified the response includes only records beyond the marker until the end of the file or up to NumberOfLines.
-- * 'numberOfLines' - The number of lines to download. If the number of lines specified results in a file over 1 MB in size, the file is truncated at 1 MB in size.
--
-- If the NumberOfLines parameter is specified, then the block of lines returned can be from the beginning or the end of the log file, depending on the value of the Marker parameter.
--
--     * If neither Marker or NumberOfLines are specified, the entire log file is returned up to a maximum of 10000 lines, starting with the most recent log entries first.
--
--
--     * If NumberOfLines is specified and Marker isn't specified, then the most recent lines from the end of the log file are returned.
--
--
--     * If Marker is specified as "0", then the specified number of lines from the beginning of the log file are returned.
--
--
--     * You can download the log file in blocks of lines by specifying the size of the block using the NumberOfLines parameter, and by specifying a value of "0" for the Marker parameter in your first request. Include the Marker value returned in the response as the Marker value for the next request, continuing until the AdditionalDataPending response element returns false.
mkDownloadDBLogFilePortion ::
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  -- | 'logFileName'
  Lude.Text ->
  DownloadDBLogFilePortion
mkDownloadDBLogFilePortion pDBInstanceIdentifier_ pLogFileName_ =
  DownloadDBLogFilePortion'
    { numberOfLines = Lude.Nothing,
      marker = Lude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_,
      logFileName = pLogFileName_
    }

-- | The number of lines to download. If the number of lines specified results in a file over 1 MB in size, the file is truncated at 1 MB in size.
--
-- If the NumberOfLines parameter is specified, then the block of lines returned can be from the beginning or the end of the log file, depending on the value of the Marker parameter.
--
--     * If neither Marker or NumberOfLines are specified, the entire log file is returned up to a maximum of 10000 lines, starting with the most recent log entries first.
--
--
--     * If NumberOfLines is specified and Marker isn't specified, then the most recent lines from the end of the log file are returned.
--
--
--     * If Marker is specified as "0", then the specified number of lines from the beginning of the log file are returned.
--
--
--     * You can download the log file in blocks of lines by specifying the size of the block using the NumberOfLines parameter, and by specifying a value of "0" for the Marker parameter in your first request. Include the Marker value returned in the response as the Marker value for the next request, continuing until the AdditionalDataPending response element returns false.
--
--
--
-- /Note:/ Consider using 'numberOfLines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfpNumberOfLines :: Lens.Lens' DownloadDBLogFilePortion (Lude.Maybe Lude.Int)
ddlfpNumberOfLines = Lens.lens (numberOfLines :: DownloadDBLogFilePortion -> Lude.Maybe Lude.Int) (\s a -> s {numberOfLines = a} :: DownloadDBLogFilePortion)
{-# DEPRECATED ddlfpNumberOfLines "Use generic-lens or generic-optics with 'numberOfLines' instead." #-}

-- | The pagination token provided in the previous request or "0". If the Marker parameter is specified the response includes only records beyond the marker until the end of the file or up to NumberOfLines.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfpMarker :: Lens.Lens' DownloadDBLogFilePortion (Lude.Maybe Lude.Text)
ddlfpMarker = Lens.lens (marker :: DownloadDBLogFilePortion -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DownloadDBLogFilePortion)
{-# DEPRECATED ddlfpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The customer-assigned name of the DB instance that contains the log files you want to list.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfpDBInstanceIdentifier :: Lens.Lens' DownloadDBLogFilePortion Lude.Text
ddlfpDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: DownloadDBLogFilePortion -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: DownloadDBLogFilePortion)
{-# DEPRECATED ddlfpDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | The name of the log file to be downloaded.
--
-- /Note:/ Consider using 'logFileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfpLogFileName :: Lens.Lens' DownloadDBLogFilePortion Lude.Text
ddlfpLogFileName = Lens.lens (logFileName :: DownloadDBLogFilePortion -> Lude.Text) (\s a -> s {logFileName = a} :: DownloadDBLogFilePortion)
{-# DEPRECATED ddlfpLogFileName "Use generic-lens or generic-optics with 'logFileName' instead." #-}

instance Page.AWSPager DownloadDBLogFilePortion where
  page rq rs
    | Page.stop (rs Lens.^. ddlfprsAdditionalDataPending) =
      Lude.Nothing
    | Lude.isNothing (rs Lens.^. ddlfprsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddlfpMarker Lens..~ rs Lens.^. ddlfprsMarker

instance Lude.AWSRequest DownloadDBLogFilePortion where
  type Rs DownloadDBLogFilePortion = DownloadDBLogFilePortionResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DownloadDBLogFilePortionResult"
      ( \s h x ->
          DownloadDBLogFilePortionResponse'
            Lude.<$> (x Lude..@? "LogFileData")
            Lude.<*> (x Lude..@? "AdditionalDataPending")
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DownloadDBLogFilePortion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DownloadDBLogFilePortion where
  toPath = Lude.const "/"

instance Lude.ToQuery DownloadDBLogFilePortion where
  toQuery DownloadDBLogFilePortion' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DownloadDBLogFilePortion" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "NumberOfLines" Lude.=: numberOfLines,
        "Marker" Lude.=: marker,
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier,
        "LogFileName" Lude.=: logFileName
      ]

-- | This data type is used as a response element to @DownloadDBLogFilePortion@ .
--
-- /See:/ 'mkDownloadDBLogFilePortionResponse' smart constructor.
data DownloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse'
  { logFileData ::
      Lude.Maybe Lude.Text,
    additionalDataPending ::
      Lude.Maybe Lude.Bool,
    marker ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DownloadDBLogFilePortionResponse' with the minimum fields required to make a request.
--
-- * 'additionalDataPending' - Boolean value that if true, indicates there is more data to be downloaded.
-- * 'logFileData' - Entries from the specified log file.
-- * 'marker' - A pagination token that can be used in a later DownloadDBLogFilePortion request.
-- * 'responseStatus' - The response status code.
mkDownloadDBLogFilePortionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DownloadDBLogFilePortionResponse
mkDownloadDBLogFilePortionResponse pResponseStatus_ =
  DownloadDBLogFilePortionResponse'
    { logFileData = Lude.Nothing,
      additionalDataPending = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Entries from the specified log file.
--
-- /Note:/ Consider using 'logFileData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfprsLogFileData :: Lens.Lens' DownloadDBLogFilePortionResponse (Lude.Maybe Lude.Text)
ddlfprsLogFileData = Lens.lens (logFileData :: DownloadDBLogFilePortionResponse -> Lude.Maybe Lude.Text) (\s a -> s {logFileData = a} :: DownloadDBLogFilePortionResponse)
{-# DEPRECATED ddlfprsLogFileData "Use generic-lens or generic-optics with 'logFileData' instead." #-}

-- | Boolean value that if true, indicates there is more data to be downloaded.
--
-- /Note:/ Consider using 'additionalDataPending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfprsAdditionalDataPending :: Lens.Lens' DownloadDBLogFilePortionResponse (Lude.Maybe Lude.Bool)
ddlfprsAdditionalDataPending = Lens.lens (additionalDataPending :: DownloadDBLogFilePortionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {additionalDataPending = a} :: DownloadDBLogFilePortionResponse)
{-# DEPRECATED ddlfprsAdditionalDataPending "Use generic-lens or generic-optics with 'additionalDataPending' instead." #-}

-- | A pagination token that can be used in a later DownloadDBLogFilePortion request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfprsMarker :: Lens.Lens' DownloadDBLogFilePortionResponse (Lude.Maybe Lude.Text)
ddlfprsMarker = Lens.lens (marker :: DownloadDBLogFilePortionResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DownloadDBLogFilePortionResponse)
{-# DEPRECATED ddlfprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfprsResponseStatus :: Lens.Lens' DownloadDBLogFilePortionResponse Lude.Int
ddlfprsResponseStatus = Lens.lens (responseStatus :: DownloadDBLogFilePortionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DownloadDBLogFilePortionResponse)
{-# DEPRECATED ddlfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
