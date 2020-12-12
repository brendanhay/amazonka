{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TeletextDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TeletextDestinationSettings
  ( TeletextDestinationSettings (..),

    -- * Smart constructor
    mkTeletextDestinationSettings,

    -- * Lenses
    tdsPageTypes,
    tdsPageNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.TeletextPageType
import qualified Network.AWS.Prelude as Lude

-- | Settings for Teletext caption output
--
-- /See:/ 'mkTeletextDestinationSettings' smart constructor.
data TeletextDestinationSettings = TeletextDestinationSettings'
  { pageTypes ::
      Lude.Maybe [TeletextPageType],
    pageNumber :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TeletextDestinationSettings' with the minimum fields required to make a request.
--
-- * 'pageNumber' - Set pageNumber to the Teletext page number for the destination captions for this output. This value must be a three-digit hexadecimal string; strings ending in -FF are invalid. If you are passing through the entire set of Teletext data, do not use this field.
-- * 'pageTypes' - Specify the page types for this Teletext page. If you don't specify a value here, the service sets the page type to the default value Subtitle (PAGE_TYPE_SUBTITLE). If you pass through the entire set of Teletext data, don't use this field. When you pass through a set of Teletext pages, your output has the same page types as your input.
mkTeletextDestinationSettings ::
  TeletextDestinationSettings
mkTeletextDestinationSettings =
  TeletextDestinationSettings'
    { pageTypes = Lude.Nothing,
      pageNumber = Lude.Nothing
    }

-- | Specify the page types for this Teletext page. If you don't specify a value here, the service sets the page type to the default value Subtitle (PAGE_TYPE_SUBTITLE). If you pass through the entire set of Teletext data, don't use this field. When you pass through a set of Teletext pages, your output has the same page types as your input.
--
-- /Note:/ Consider using 'pageTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsPageTypes :: Lens.Lens' TeletextDestinationSettings (Lude.Maybe [TeletextPageType])
tdsPageTypes = Lens.lens (pageTypes :: TeletextDestinationSettings -> Lude.Maybe [TeletextPageType]) (\s a -> s {pageTypes = a} :: TeletextDestinationSettings)
{-# DEPRECATED tdsPageTypes "Use generic-lens or generic-optics with 'pageTypes' instead." #-}

-- | Set pageNumber to the Teletext page number for the destination captions for this output. This value must be a three-digit hexadecimal string; strings ending in -FF are invalid. If you are passing through the entire set of Teletext data, do not use this field.
--
-- /Note:/ Consider using 'pageNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsPageNumber :: Lens.Lens' TeletextDestinationSettings (Lude.Maybe Lude.Text)
tdsPageNumber = Lens.lens (pageNumber :: TeletextDestinationSettings -> Lude.Maybe Lude.Text) (\s a -> s {pageNumber = a} :: TeletextDestinationSettings)
{-# DEPRECATED tdsPageNumber "Use generic-lens or generic-optics with 'pageNumber' instead." #-}

instance Lude.FromJSON TeletextDestinationSettings where
  parseJSON =
    Lude.withObject
      "TeletextDestinationSettings"
      ( \x ->
          TeletextDestinationSettings'
            Lude.<$> (x Lude..:? "pageTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "pageNumber")
      )

instance Lude.ToJSON TeletextDestinationSettings where
  toJSON TeletextDestinationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("pageTypes" Lude..=) Lude.<$> pageTypes,
            ("pageNumber" Lude..=) Lude.<$> pageNumber
          ]
      )
