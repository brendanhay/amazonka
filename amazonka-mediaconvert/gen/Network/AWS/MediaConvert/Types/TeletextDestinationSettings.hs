{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TeletextDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TeletextDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.TeletextPageType

-- | Settings for Teletext caption output
--
-- /See:/ 'newTeletextDestinationSettings' smart constructor.
data TeletextDestinationSettings = TeletextDestinationSettings'
  { -- | Specify the page types for this Teletext page. If you don\'t specify a
    -- value here, the service sets the page type to the default value Subtitle
    -- (PAGE_TYPE_SUBTITLE). If you pass through the entire set of Teletext
    -- data, don\'t use this field. When you pass through a set of Teletext
    -- pages, your output has the same page types as your input.
    pageTypes :: Core.Maybe [TeletextPageType],
    -- | Set pageNumber to the Teletext page number for the destination captions
    -- for this output. This value must be a three-digit hexadecimal string;
    -- strings ending in -FF are invalid. If you are passing through the entire
    -- set of Teletext data, do not use this field.
    pageNumber :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TeletextDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageTypes', 'teletextDestinationSettings_pageTypes' - Specify the page types for this Teletext page. If you don\'t specify a
-- value here, the service sets the page type to the default value Subtitle
-- (PAGE_TYPE_SUBTITLE). If you pass through the entire set of Teletext
-- data, don\'t use this field. When you pass through a set of Teletext
-- pages, your output has the same page types as your input.
--
-- 'pageNumber', 'teletextDestinationSettings_pageNumber' - Set pageNumber to the Teletext page number for the destination captions
-- for this output. This value must be a three-digit hexadecimal string;
-- strings ending in -FF are invalid. If you are passing through the entire
-- set of Teletext data, do not use this field.
newTeletextDestinationSettings ::
  TeletextDestinationSettings
newTeletextDestinationSettings =
  TeletextDestinationSettings'
    { pageTypes =
        Core.Nothing,
      pageNumber = Core.Nothing
    }

-- | Specify the page types for this Teletext page. If you don\'t specify a
-- value here, the service sets the page type to the default value Subtitle
-- (PAGE_TYPE_SUBTITLE). If you pass through the entire set of Teletext
-- data, don\'t use this field. When you pass through a set of Teletext
-- pages, your output has the same page types as your input.
teletextDestinationSettings_pageTypes :: Lens.Lens' TeletextDestinationSettings (Core.Maybe [TeletextPageType])
teletextDestinationSettings_pageTypes = Lens.lens (\TeletextDestinationSettings' {pageTypes} -> pageTypes) (\s@TeletextDestinationSettings' {} a -> s {pageTypes = a} :: TeletextDestinationSettings) Core.. Lens.mapping Lens._Coerce

-- | Set pageNumber to the Teletext page number for the destination captions
-- for this output. This value must be a three-digit hexadecimal string;
-- strings ending in -FF are invalid. If you are passing through the entire
-- set of Teletext data, do not use this field.
teletextDestinationSettings_pageNumber :: Lens.Lens' TeletextDestinationSettings (Core.Maybe Core.Text)
teletextDestinationSettings_pageNumber = Lens.lens (\TeletextDestinationSettings' {pageNumber} -> pageNumber) (\s@TeletextDestinationSettings' {} a -> s {pageNumber = a} :: TeletextDestinationSettings)

instance Core.FromJSON TeletextDestinationSettings where
  parseJSON =
    Core.withObject
      "TeletextDestinationSettings"
      ( \x ->
          TeletextDestinationSettings'
            Core.<$> (x Core..:? "pageTypes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "pageNumber")
      )

instance Core.Hashable TeletextDestinationSettings

instance Core.NFData TeletextDestinationSettings

instance Core.ToJSON TeletextDestinationSettings where
  toJSON TeletextDestinationSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("pageTypes" Core..=) Core.<$> pageTypes,
            ("pageNumber" Core..=) Core.<$> pageNumber
          ]
      )
