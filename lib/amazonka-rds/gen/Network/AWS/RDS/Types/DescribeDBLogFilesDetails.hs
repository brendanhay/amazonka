{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ddblfdLastWritten,
    ddblfdLogFileName,
    ddblfdSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | This data type is used as a response element to @DescribeDBLogFiles@ .
--
-- /See:/ 'mkDescribeDBLogFilesDetails' smart constructor.
data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails'
  { -- | A POSIX timestamp when the last log entry was written.
    lastWritten :: Core.Maybe Core.Integer,
    -- | The name of the log file for the specified DB instance.
    logFileName :: Core.Maybe Types.String,
    -- | The size, in bytes, of the log file for the specified DB instance.
    size :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBLogFilesDetails' value with any optional fields omitted.
mkDescribeDBLogFilesDetails ::
  DescribeDBLogFilesDetails
mkDescribeDBLogFilesDetails =
  DescribeDBLogFilesDetails'
    { lastWritten = Core.Nothing,
      logFileName = Core.Nothing,
      size = Core.Nothing
    }

-- | A POSIX timestamp when the last log entry was written.
--
-- /Note:/ Consider using 'lastWritten' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfdLastWritten :: Lens.Lens' DescribeDBLogFilesDetails (Core.Maybe Core.Integer)
ddblfdLastWritten = Lens.field @"lastWritten"
{-# DEPRECATED ddblfdLastWritten "Use generic-lens or generic-optics with 'lastWritten' instead." #-}

-- | The name of the log file for the specified DB instance.
--
-- /Note:/ Consider using 'logFileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfdLogFileName :: Lens.Lens' DescribeDBLogFilesDetails (Core.Maybe Types.String)
ddblfdLogFileName = Lens.field @"logFileName"
{-# DEPRECATED ddblfdLogFileName "Use generic-lens or generic-optics with 'logFileName' instead." #-}

-- | The size, in bytes, of the log file for the specified DB instance.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfdSize :: Lens.Lens' DescribeDBLogFilesDetails (Core.Maybe Core.Integer)
ddblfdSize = Lens.field @"size"
{-# DEPRECATED ddblfdSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Core.FromXML DescribeDBLogFilesDetails where
  parseXML x =
    DescribeDBLogFilesDetails'
      Core.<$> (x Core..@? "LastWritten")
      Core.<*> (x Core..@? "LogFileName")
      Core.<*> (x Core..@? "Size")
