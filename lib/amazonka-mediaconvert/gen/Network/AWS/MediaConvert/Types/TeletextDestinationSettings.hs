{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TeletextDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.TeletextDestinationSettings
  ( TeletextDestinationSettings (..)
  -- * Smart constructor
  , mkTeletextDestinationSettings
  -- * Lenses
  , tdsPageNumber
  , tdsPageTypes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.TeletextPageType as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for Teletext caption output
--
-- /See:/ 'mkTeletextDestinationSettings' smart constructor.
data TeletextDestinationSettings = TeletextDestinationSettings'
  { pageNumber :: Core.Maybe Core.Text
    -- ^ Set pageNumber to the Teletext page number for the destination captions for this output. This value must be a three-digit hexadecimal string; strings ending in -FF are invalid. If you are passing through the entire set of Teletext data, do not use this field.
  , pageTypes :: Core.Maybe [Types.TeletextPageType]
    -- ^ Specify the page types for this Teletext page. If you don't specify a value here, the service sets the page type to the default value Subtitle (PAGE_TYPE_SUBTITLE). If you pass through the entire set of Teletext data, don't use this field. When you pass through a set of Teletext pages, your output has the same page types as your input.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TeletextDestinationSettings' value with any optional fields omitted.
mkTeletextDestinationSettings
    :: TeletextDestinationSettings
mkTeletextDestinationSettings
  = TeletextDestinationSettings'{pageNumber = Core.Nothing,
                                 pageTypes = Core.Nothing}

-- | Set pageNumber to the Teletext page number for the destination captions for this output. This value must be a three-digit hexadecimal string; strings ending in -FF are invalid. If you are passing through the entire set of Teletext data, do not use this field.
--
-- /Note:/ Consider using 'pageNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsPageNumber :: Lens.Lens' TeletextDestinationSettings (Core.Maybe Core.Text)
tdsPageNumber = Lens.field @"pageNumber"
{-# INLINEABLE tdsPageNumber #-}
{-# DEPRECATED pageNumber "Use generic-lens or generic-optics with 'pageNumber' instead"  #-}

-- | Specify the page types for this Teletext page. If you don't specify a value here, the service sets the page type to the default value Subtitle (PAGE_TYPE_SUBTITLE). If you pass through the entire set of Teletext data, don't use this field. When you pass through a set of Teletext pages, your output has the same page types as your input.
--
-- /Note:/ Consider using 'pageTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsPageTypes :: Lens.Lens' TeletextDestinationSettings (Core.Maybe [Types.TeletextPageType])
tdsPageTypes = Lens.field @"pageTypes"
{-# INLINEABLE tdsPageTypes #-}
{-# DEPRECATED pageTypes "Use generic-lens or generic-optics with 'pageTypes' instead"  #-}

instance Core.FromJSON TeletextDestinationSettings where
        toJSON TeletextDestinationSettings{..}
          = Core.object
              (Core.catMaybes
                 [("pageNumber" Core..=) Core.<$> pageNumber,
                  ("pageTypes" Core..=) Core.<$> pageTypes])

instance Core.FromJSON TeletextDestinationSettings where
        parseJSON
          = Core.withObject "TeletextDestinationSettings" Core.$
              \ x ->
                TeletextDestinationSettings' Core.<$>
                  (x Core..:? "pageNumber") Core.<*> x Core..:? "pageTypes"
