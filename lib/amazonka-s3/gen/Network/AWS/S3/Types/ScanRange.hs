{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ScanRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ScanRange
  ( ScanRange (..),

    -- * Smart constructor
    mkScanRange,

    -- * Lenses
    srStart,
    srEnd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Specifies the byte range of the object to get the records from. A record is processed when its first byte is contained by the range. This parameter is optional, but when specified, it must not be empty. See RFC 2616, Section 14.35.1 about how to specify the start and end of the range.
--
-- /See:/ 'mkScanRange' smart constructor.
data ScanRange = ScanRange'
  { -- | Specifies the start of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is 0. If only start is supplied, it means scan from that point to the end of the file.For example; @<scanrange><start>50</start></scanrange>@ means scan from byte 50 until the end of the file.
    start :: Lude.Maybe Lude.Integer,
    -- | Specifies the end of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is one less than the size of the object being queried. If only the End parameter is supplied, it is interpreted to mean scan the last N bytes of the file. For example, @<scanrange><end>50</end></scanrange>@ means scan the last 50 bytes.
    end :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScanRange' with the minimum fields required to make a request.
--
-- * 'start' - Specifies the start of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is 0. If only start is supplied, it means scan from that point to the end of the file.For example; @<scanrange><start>50</start></scanrange>@ means scan from byte 50 until the end of the file.
-- * 'end' - Specifies the end of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is one less than the size of the object being queried. If only the End parameter is supplied, it is interpreted to mean scan the last N bytes of the file. For example, @<scanrange><end>50</end></scanrange>@ means scan the last 50 bytes.
mkScanRange ::
  ScanRange
mkScanRange = ScanRange' {start = Lude.Nothing, end = Lude.Nothing}

-- | Specifies the start of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is 0. If only start is supplied, it means scan from that point to the end of the file.For example; @<scanrange><start>50</start></scanrange>@ means scan from byte 50 until the end of the file.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srStart :: Lens.Lens' ScanRange (Lude.Maybe Lude.Integer)
srStart = Lens.lens (start :: ScanRange -> Lude.Maybe Lude.Integer) (\s a -> s {start = a} :: ScanRange)
{-# DEPRECATED srStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | Specifies the end of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is one less than the size of the object being queried. If only the End parameter is supplied, it is interpreted to mean scan the last N bytes of the file. For example, @<scanrange><end>50</end></scanrange>@ means scan the last 50 bytes.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srEnd :: Lens.Lens' ScanRange (Lude.Maybe Lude.Integer)
srEnd = Lens.lens (end :: ScanRange -> Lude.Maybe Lude.Integer) (\s a -> s {end = a} :: ScanRange)
{-# DEPRECATED srEnd "Use generic-lens or generic-optics with 'end' instead." #-}

instance Lude.ToXML ScanRange where
  toXML ScanRange' {..} =
    Lude.mconcat ["Start" Lude.@= start, "End" Lude.@= end]
