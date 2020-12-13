{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FecOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FecOutputSettings
  ( FecOutputSettings (..),

    -- * Smart constructor
    mkFecOutputSettings,

    -- * Lenses
    fosRowLength,
    fosIncludeFec,
    fosColumnDepth,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FecOutputIncludeFec
import qualified Network.AWS.Prelude as Lude

-- | Fec Output Settings
--
-- /See:/ 'mkFecOutputSettings' smart constructor.
data FecOutputSettings = FecOutputSettings'
  { -- | Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.  Must be between 1 and 20, inclusive. If only Column FEC is used, then larger values increase robustness.  If Row FEC is used, then this is the number of transport stream packets per row error correction packet, and the value must be between 4 and 20, inclusive, if includeFec is columnAndRow. If includeFec is column, this value must be 1 to 20, inclusive.
    rowLength :: Lude.Maybe Lude.Natural,
    -- | Enables column only or column and row based FEC
    includeFec :: Lude.Maybe FecOutputIncludeFec,
    -- | Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.  The number of transport stream packets per column error correction packet. Must be between 4 and 20, inclusive.
    columnDepth :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FecOutputSettings' with the minimum fields required to make a request.
--
-- * 'rowLength' - Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.  Must be between 1 and 20, inclusive. If only Column FEC is used, then larger values increase robustness.  If Row FEC is used, then this is the number of transport stream packets per row error correction packet, and the value must be between 4 and 20, inclusive, if includeFec is columnAndRow. If includeFec is column, this value must be 1 to 20, inclusive.
-- * 'includeFec' - Enables column only or column and row based FEC
-- * 'columnDepth' - Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.  The number of transport stream packets per column error correction packet. Must be between 4 and 20, inclusive.
mkFecOutputSettings ::
  FecOutputSettings
mkFecOutputSettings =
  FecOutputSettings'
    { rowLength = Lude.Nothing,
      includeFec = Lude.Nothing,
      columnDepth = Lude.Nothing
    }

-- | Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.  Must be between 1 and 20, inclusive. If only Column FEC is used, then larger values increase robustness.  If Row FEC is used, then this is the number of transport stream packets per row error correction packet, and the value must be between 4 and 20, inclusive, if includeFec is columnAndRow. If includeFec is column, this value must be 1 to 20, inclusive.
--
-- /Note:/ Consider using 'rowLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fosRowLength :: Lens.Lens' FecOutputSettings (Lude.Maybe Lude.Natural)
fosRowLength = Lens.lens (rowLength :: FecOutputSettings -> Lude.Maybe Lude.Natural) (\s a -> s {rowLength = a} :: FecOutputSettings)
{-# DEPRECATED fosRowLength "Use generic-lens or generic-optics with 'rowLength' instead." #-}

-- | Enables column only or column and row based FEC
--
-- /Note:/ Consider using 'includeFec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fosIncludeFec :: Lens.Lens' FecOutputSettings (Lude.Maybe FecOutputIncludeFec)
fosIncludeFec = Lens.lens (includeFec :: FecOutputSettings -> Lude.Maybe FecOutputIncludeFec) (\s a -> s {includeFec = a} :: FecOutputSettings)
{-# DEPRECATED fosIncludeFec "Use generic-lens or generic-optics with 'includeFec' instead." #-}

-- | Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.  The number of transport stream packets per column error correction packet. Must be between 4 and 20, inclusive.
--
-- /Note:/ Consider using 'columnDepth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fosColumnDepth :: Lens.Lens' FecOutputSettings (Lude.Maybe Lude.Natural)
fosColumnDepth = Lens.lens (columnDepth :: FecOutputSettings -> Lude.Maybe Lude.Natural) (\s a -> s {columnDepth = a} :: FecOutputSettings)
{-# DEPRECATED fosColumnDepth "Use generic-lens or generic-optics with 'columnDepth' instead." #-}

instance Lude.FromJSON FecOutputSettings where
  parseJSON =
    Lude.withObject
      "FecOutputSettings"
      ( \x ->
          FecOutputSettings'
            Lude.<$> (x Lude..:? "rowLength")
            Lude.<*> (x Lude..:? "includeFec")
            Lude.<*> (x Lude..:? "columnDepth")
      )

instance Lude.ToJSON FecOutputSettings where
  toJSON FecOutputSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("rowLength" Lude..=) Lude.<$> rowLength,
            ("includeFec" Lude..=) Lude.<$> includeFec,
            ("columnDepth" Lude..=) Lude.<$> columnDepth
          ]
      )
