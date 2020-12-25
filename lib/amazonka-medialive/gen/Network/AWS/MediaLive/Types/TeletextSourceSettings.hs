{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TeletextSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TeletextSourceSettings
  ( TeletextSourceSettings (..),

    -- * Smart constructor
    mkTeletextSourceSettings,

    -- * Lenses
    tssPageNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Teletext Source Settings
--
-- /See:/ 'mkTeletextSourceSettings' smart constructor.
newtype TeletextSourceSettings = TeletextSourceSettings'
  { -- | Specifies the teletext page number within the data stream from which to extract captions. Range of 0x100 (256) to 0x8FF (2303). Unused for passthrough. Should be specified as a hexadecimal string with no "0x" prefix.
    pageNumber :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TeletextSourceSettings' value with any optional fields omitted.
mkTeletextSourceSettings ::
  TeletextSourceSettings
mkTeletextSourceSettings =
  TeletextSourceSettings' {pageNumber = Core.Nothing}

-- | Specifies the teletext page number within the data stream from which to extract captions. Range of 0x100 (256) to 0x8FF (2303). Unused for passthrough. Should be specified as a hexadecimal string with no "0x" prefix.
--
-- /Note:/ Consider using 'pageNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tssPageNumber :: Lens.Lens' TeletextSourceSettings (Core.Maybe Core.Text)
tssPageNumber = Lens.field @"pageNumber"
{-# DEPRECATED tssPageNumber "Use generic-lens or generic-optics with 'pageNumber' instead." #-}

instance Core.FromJSON TeletextSourceSettings where
  toJSON TeletextSourceSettings {..} =
    Core.object
      (Core.catMaybes [("pageNumber" Core..=) Core.<$> pageNumber])

instance Core.FromJSON TeletextSourceSettings where
  parseJSON =
    Core.withObject "TeletextSourceSettings" Core.$
      \x -> TeletextSourceSettings' Core.<$> (x Core..:? "pageNumber")
