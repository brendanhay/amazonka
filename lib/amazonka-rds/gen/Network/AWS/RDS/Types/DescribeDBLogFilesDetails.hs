-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DescribeDBLogFilesDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DescribeDBLogFilesDetails
  ( DescribeDBLogFilesDetails (..),

    -- * Smart constructor
    mkDescribeDBLogFilesDetails,

    -- * Lenses
    ddlfdLastWritten,
    ddlfdSize,
    ddlfdLogFileName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used as a response element to @DescribeDBLogFiles@ .
--
-- /See:/ 'mkDescribeDBLogFilesDetails' smart constructor.
data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails'
  { lastWritten ::
      Lude.Maybe Lude.Integer,
    size :: Lude.Maybe Lude.Integer,
    logFileName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBLogFilesDetails' with the minimum fields required to make a request.
--
-- * 'lastWritten' - A POSIX timestamp when the last log entry was written.
-- * 'logFileName' - The name of the log file for the specified DB instance.
-- * 'size' - The size, in bytes, of the log file for the specified DB instance.
mkDescribeDBLogFilesDetails ::
  DescribeDBLogFilesDetails
mkDescribeDBLogFilesDetails =
  DescribeDBLogFilesDetails'
    { lastWritten = Lude.Nothing,
      size = Lude.Nothing,
      logFileName = Lude.Nothing
    }

-- | A POSIX timestamp when the last log entry was written.
--
-- /Note:/ Consider using 'lastWritten' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfdLastWritten :: Lens.Lens' DescribeDBLogFilesDetails (Lude.Maybe Lude.Integer)
ddlfdLastWritten = Lens.lens (lastWritten :: DescribeDBLogFilesDetails -> Lude.Maybe Lude.Integer) (\s a -> s {lastWritten = a} :: DescribeDBLogFilesDetails)
{-# DEPRECATED ddlfdLastWritten "Use generic-lens or generic-optics with 'lastWritten' instead." #-}

-- | The size, in bytes, of the log file for the specified DB instance.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfdSize :: Lens.Lens' DescribeDBLogFilesDetails (Lude.Maybe Lude.Integer)
ddlfdSize = Lens.lens (size :: DescribeDBLogFilesDetails -> Lude.Maybe Lude.Integer) (\s a -> s {size = a} :: DescribeDBLogFilesDetails)
{-# DEPRECATED ddlfdSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The name of the log file for the specified DB instance.
--
-- /Note:/ Consider using 'logFileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddlfdLogFileName :: Lens.Lens' DescribeDBLogFilesDetails (Lude.Maybe Lude.Text)
ddlfdLogFileName = Lens.lens (logFileName :: DescribeDBLogFilesDetails -> Lude.Maybe Lude.Text) (\s a -> s {logFileName = a} :: DescribeDBLogFilesDetails)
{-# DEPRECATED ddlfdLogFileName "Use generic-lens or generic-optics with 'logFileName' instead." #-}

instance Lude.FromXML DescribeDBLogFilesDetails where
  parseXML x =
    DescribeDBLogFilesDetails'
      Lude.<$> (x Lude..@? "LastWritten")
      Lude.<*> (x Lude..@? "Size")
      Lude.<*> (x Lude..@? "LogFileName")
