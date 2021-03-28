{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TeletextSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.TeletextSourceSettings
  ( TeletextSourceSettings (..)
  -- * Smart constructor
  , mkTeletextSourceSettings
  -- * Lenses
  , tssPageNumber
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings specific to Teletext caption sources, including Page number.
--
-- /See:/ 'mkTeletextSourceSettings' smart constructor.
newtype TeletextSourceSettings = TeletextSourceSettings'
  { pageNumber :: Core.Maybe Core.Text
    -- ^ Use Page Number (PageNumber) to specify the three-digit hexadecimal page number that will be used for Teletext captions. Do not use this setting if you are passing through teletext from the input source to output.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TeletextSourceSettings' value with any optional fields omitted.
mkTeletextSourceSettings
    :: TeletextSourceSettings
mkTeletextSourceSettings
  = TeletextSourceSettings'{pageNumber = Core.Nothing}

-- | Use Page Number (PageNumber) to specify the three-digit hexadecimal page number that will be used for Teletext captions. Do not use this setting if you are passing through teletext from the input source to output.
--
-- /Note:/ Consider using 'pageNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tssPageNumber :: Lens.Lens' TeletextSourceSettings (Core.Maybe Core.Text)
tssPageNumber = Lens.field @"pageNumber"
{-# INLINEABLE tssPageNumber #-}
{-# DEPRECATED pageNumber "Use generic-lens or generic-optics with 'pageNumber' instead"  #-}

instance Core.FromJSON TeletextSourceSettings where
        toJSON TeletextSourceSettings{..}
          = Core.object
              (Core.catMaybes [("pageNumber" Core..=) Core.<$> pageNumber])

instance Core.FromJSON TeletextSourceSettings where
        parseJSON
          = Core.withObject "TeletextSourceSettings" Core.$
              \ x -> TeletextSourceSettings' Core.<$> (x Core..:? "pageNumber")
