{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBLogFiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB log files for the DB instance.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBLogFiles
  ( -- * Creating a request
    DescribeDBLogFiles (..),
    mkDescribeDBLogFiles,

    -- ** Request lenses
    ddlfFilenameContains,
    ddlfFilters,
    ddlfFileSize,
    ddlfFileLastWritten,
    ddlfMarker,
    ddlfMaxRecords,
    ddlfDBInstanceIdentifier,

    -- * Destructuring the response
    DescribeDBLogFilesResponse (..),
    mkDescribeDBLogFilesResponse,

    -- ** Response lenses
    ddlfrsDescribeDBLogFiles,
    ddlfrsMarker,
    ddlfrsResponseStatus,
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
-- /See:/ 'mkDescribeDBLogFiles' smart constructor.
data DescribeDBLogFiles = DescribeDBLogFiles'
  { filenameContains ::
      Lude.Maybe Lude.Text,
    filters :: Lude.Maybe [Filter],
    fileSize :: Lude.Maybe Lude.Integer,
    fileLastWritten :: Lude.Maybe Lude.Integer,
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    dbInstanceIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBLogFiles' with the minimum fields required to make a request.
--
-- * 'dbInstanceIdentifier' - The customer-assigned name of the DB instance that contains the log files you want to list.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
-- * 'fileLastWritten' - Filters the available log files for files written since the specified date, in POSIX timestamp format with milliseconds.
-- * 'fileSize' - Filters the available log files for files larger than the specified size.
-- * 'filenameContains' - Filters the available log files for log file names that contain the specified string.
-- * 'filters' - This parameter isn't currently supported.
-- * 'marker' - The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to MaxRecords.
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
mkDescribeDBLogFiles ::
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  DescribeDBLogFiles
mkDescribeDBLogFiles pDBInstanceIdentifier_ =
  DescribeDBLogFiles'
    { filenameContains = Lude.Nothing,
      filters = Lude.Nothing,
      fileSize = Lude.Nothing,
      fileLastWritten = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | Filters the available log files for log file names that contain the specified string.
--
-- /Note:/ Consider using 'filenameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfFilenameContains :: Lens.Lens' DescribeDBLogFiles (Lude.Maybe Lude.Text)
ddlfFilenameContains = Lens.lens (filenameContains :: DescribeDBLogFiles -> Lude.Maybe Lude.Text) (\s a -> s {filenameContains = a} :: DescribeDBLogFiles)
{-# DEPRECATED ddlfFilenameContains "Use generic-lens or generic-optics with 'filenameContains' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfFilters :: Lens.Lens' DescribeDBLogFiles (Lude.Maybe [Filter])
ddlfFilters = Lens.lens (filters :: DescribeDBLogFiles -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBLogFiles)
{-# DEPRECATED ddlfFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Filters the available log files for files larger than the specified size.
--
-- /Note:/ Consider using 'fileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfFileSize :: Lens.Lens' DescribeDBLogFiles (Lude.Maybe Lude.Integer)
ddlfFileSize = Lens.lens (fileSize :: DescribeDBLogFiles -> Lude.Maybe Lude.Integer) (\s a -> s {fileSize = a} :: DescribeDBLogFiles)
{-# DEPRECATED ddlfFileSize "Use generic-lens or generic-optics with 'fileSize' instead." #-}

-- | Filters the available log files for files written since the specified date, in POSIX timestamp format with milliseconds.
--
-- /Note:/ Consider using 'fileLastWritten' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfFileLastWritten :: Lens.Lens' DescribeDBLogFiles (Lude.Maybe Lude.Integer)
ddlfFileLastWritten = Lens.lens (fileLastWritten :: DescribeDBLogFiles -> Lude.Maybe Lude.Integer) (\s a -> s {fileLastWritten = a} :: DescribeDBLogFiles)
{-# DEPRECATED ddlfFileLastWritten "Use generic-lens or generic-optics with 'fileLastWritten' instead." #-}

-- | The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to MaxRecords.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfMarker :: Lens.Lens' DescribeDBLogFiles (Lude.Maybe Lude.Text)
ddlfMarker = Lens.lens (marker :: DescribeDBLogFiles -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBLogFiles)
{-# DEPRECATED ddlfMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfMaxRecords :: Lens.Lens' DescribeDBLogFiles (Lude.Maybe Lude.Int)
ddlfMaxRecords = Lens.lens (maxRecords :: DescribeDBLogFiles -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBLogFiles)
{-# DEPRECATED ddlfMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The customer-assigned name of the DB instance that contains the log files you want to list.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfDBInstanceIdentifier :: Lens.Lens' DescribeDBLogFiles Lude.Text
ddlfDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: DescribeDBLogFiles -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: DescribeDBLogFiles)
{-# DEPRECATED ddlfDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

instance Page.AWSPager DescribeDBLogFiles where
  page rq rs
    | Page.stop (rs Lens.^. ddlfrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddlfrsDescribeDBLogFiles) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddlfMarker Lens..~ rs Lens.^. ddlfrsMarker

instance Lude.AWSRequest DescribeDBLogFiles where
  type Rs DescribeDBLogFiles = DescribeDBLogFilesResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBLogFilesResult"
      ( \s h x ->
          DescribeDBLogFilesResponse'
            Lude.<$> ( x Lude..@? "DescribeDBLogFiles" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DescribeDBLogFilesDetails")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBLogFiles where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBLogFiles where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBLogFiles where
  toQuery DescribeDBLogFiles' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDBLogFiles" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "FilenameContains" Lude.=: filenameContains,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "FileSize" Lude.=: fileSize,
        "FileLastWritten" Lude.=: fileLastWritten,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier
      ]

-- | The response from a call to @DescribeDBLogFiles@ .
--
-- /See:/ 'mkDescribeDBLogFilesResponse' smart constructor.
data DescribeDBLogFilesResponse = DescribeDBLogFilesResponse'
  { describeDBLogFiles ::
      Lude.Maybe
        [DescribeDBLogFilesDetails],
    marker :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBLogFilesResponse' with the minimum fields required to make a request.
--
-- * 'describeDBLogFiles' - The DB log files returned.
-- * 'marker' - A pagination token that can be used in a later DescribeDBLogFiles request.
-- * 'responseStatus' - The response status code.
mkDescribeDBLogFilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBLogFilesResponse
mkDescribeDBLogFilesResponse pResponseStatus_ =
  DescribeDBLogFilesResponse'
    { describeDBLogFiles = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The DB log files returned.
--
-- /Note:/ Consider using 'describeDBLogFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfrsDescribeDBLogFiles :: Lens.Lens' DescribeDBLogFilesResponse (Lude.Maybe [DescribeDBLogFilesDetails])
ddlfrsDescribeDBLogFiles = Lens.lens (describeDBLogFiles :: DescribeDBLogFilesResponse -> Lude.Maybe [DescribeDBLogFilesDetails]) (\s a -> s {describeDBLogFiles = a} :: DescribeDBLogFilesResponse)
{-# DEPRECATED ddlfrsDescribeDBLogFiles "Use generic-lens or generic-optics with 'describeDBLogFiles' instead." #-}

-- | A pagination token that can be used in a later DescribeDBLogFiles request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfrsMarker :: Lens.Lens' DescribeDBLogFilesResponse (Lude.Maybe Lude.Text)
ddlfrsMarker = Lens.lens (marker :: DescribeDBLogFilesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBLogFilesResponse)
{-# DEPRECATED ddlfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfrsResponseStatus :: Lens.Lens' DescribeDBLogFilesResponse Lude.Int
ddlfrsResponseStatus = Lens.lens (responseStatus :: DescribeDBLogFilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBLogFilesResponse)
{-# DEPRECATED ddlfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
