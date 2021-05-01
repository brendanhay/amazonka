{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.TeletextPageType
import qualified Network.AWS.Prelude as Prelude

-- | Settings for Teletext caption output
--
-- /See:/ 'newTeletextDestinationSettings' smart constructor.
data TeletextDestinationSettings = TeletextDestinationSettings'
  { -- | Specify the page types for this Teletext page. If you don\'t specify a
    -- value here, the service sets the page type to the default value Subtitle
    -- (PAGE_TYPE_SUBTITLE). If you pass through the entire set of Teletext
    -- data, don\'t use this field. When you pass through a set of Teletext
    -- pages, your output has the same page types as your input.
    pageTypes :: Prelude.Maybe [TeletextPageType],
    -- | Set pageNumber to the Teletext page number for the destination captions
    -- for this output. This value must be a three-digit hexadecimal string;
    -- strings ending in -FF are invalid. If you are passing through the entire
    -- set of Teletext data, do not use this field.
    pageNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      pageNumber = Prelude.Nothing
    }

-- | Specify the page types for this Teletext page. If you don\'t specify a
-- value here, the service sets the page type to the default value Subtitle
-- (PAGE_TYPE_SUBTITLE). If you pass through the entire set of Teletext
-- data, don\'t use this field. When you pass through a set of Teletext
-- pages, your output has the same page types as your input.
teletextDestinationSettings_pageTypes :: Lens.Lens' TeletextDestinationSettings (Prelude.Maybe [TeletextPageType])
teletextDestinationSettings_pageTypes = Lens.lens (\TeletextDestinationSettings' {pageTypes} -> pageTypes) (\s@TeletextDestinationSettings' {} a -> s {pageTypes = a} :: TeletextDestinationSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | Set pageNumber to the Teletext page number for the destination captions
-- for this output. This value must be a three-digit hexadecimal string;
-- strings ending in -FF are invalid. If you are passing through the entire
-- set of Teletext data, do not use this field.
teletextDestinationSettings_pageNumber :: Lens.Lens' TeletextDestinationSettings (Prelude.Maybe Prelude.Text)
teletextDestinationSettings_pageNumber = Lens.lens (\TeletextDestinationSettings' {pageNumber} -> pageNumber) (\s@TeletextDestinationSettings' {} a -> s {pageNumber = a} :: TeletextDestinationSettings)

instance Prelude.FromJSON TeletextDestinationSettings where
  parseJSON =
    Prelude.withObject
      "TeletextDestinationSettings"
      ( \x ->
          TeletextDestinationSettings'
            Prelude.<$> ( x Prelude..:? "pageTypes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "pageNumber")
      )

instance Prelude.Hashable TeletextDestinationSettings

instance Prelude.NFData TeletextDestinationSettings

instance Prelude.ToJSON TeletextDestinationSettings where
  toJSON TeletextDestinationSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("pageTypes" Prelude..=) Prelude.<$> pageTypes,
            ("pageNumber" Prelude..=) Prelude.<$> pageNumber
          ]
      )
