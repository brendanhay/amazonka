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
-- Module      : Amazonka.MediaConvert.Types.TeletextDestinationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.TeletextDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.TeletextPageType
import qualified Amazonka.Prelude as Prelude

-- | Settings related to teletext captions. Set up teletext captions in the
-- same output as your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/teletext-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- TELETEXT.
--
-- /See:/ 'newTeletextDestinationSettings' smart constructor.
data TeletextDestinationSettings = TeletextDestinationSettings'
  { -- | Set pageNumber to the Teletext page number for the destination captions
    -- for this output. This value must be a three-digit hexadecimal string;
    -- strings ending in -FF are invalid. If you are passing through the entire
    -- set of Teletext data, do not use this field.
    pageNumber :: Prelude.Maybe Prelude.Text,
    -- | Specify the page types for this Teletext page. If you don\'t specify a
    -- value here, the service sets the page type to the default value Subtitle
    -- (PAGE_TYPE_SUBTITLE). If you pass through the entire set of Teletext
    -- data, don\'t use this field. When you pass through a set of Teletext
    -- pages, your output has the same page types as your input.
    pageTypes :: Prelude.Maybe [TeletextPageType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TeletextDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageNumber', 'teletextDestinationSettings_pageNumber' - Set pageNumber to the Teletext page number for the destination captions
-- for this output. This value must be a three-digit hexadecimal string;
-- strings ending in -FF are invalid. If you are passing through the entire
-- set of Teletext data, do not use this field.
--
-- 'pageTypes', 'teletextDestinationSettings_pageTypes' - Specify the page types for this Teletext page. If you don\'t specify a
-- value here, the service sets the page type to the default value Subtitle
-- (PAGE_TYPE_SUBTITLE). If you pass through the entire set of Teletext
-- data, don\'t use this field. When you pass through a set of Teletext
-- pages, your output has the same page types as your input.
newTeletextDestinationSettings ::
  TeletextDestinationSettings
newTeletextDestinationSettings =
  TeletextDestinationSettings'
    { pageNumber =
        Prelude.Nothing,
      pageTypes = Prelude.Nothing
    }

-- | Set pageNumber to the Teletext page number for the destination captions
-- for this output. This value must be a three-digit hexadecimal string;
-- strings ending in -FF are invalid. If you are passing through the entire
-- set of Teletext data, do not use this field.
teletextDestinationSettings_pageNumber :: Lens.Lens' TeletextDestinationSettings (Prelude.Maybe Prelude.Text)
teletextDestinationSettings_pageNumber = Lens.lens (\TeletextDestinationSettings' {pageNumber} -> pageNumber) (\s@TeletextDestinationSettings' {} a -> s {pageNumber = a} :: TeletextDestinationSettings)

-- | Specify the page types for this Teletext page. If you don\'t specify a
-- value here, the service sets the page type to the default value Subtitle
-- (PAGE_TYPE_SUBTITLE). If you pass through the entire set of Teletext
-- data, don\'t use this field. When you pass through a set of Teletext
-- pages, your output has the same page types as your input.
teletextDestinationSettings_pageTypes :: Lens.Lens' TeletextDestinationSettings (Prelude.Maybe [TeletextPageType])
teletextDestinationSettings_pageTypes = Lens.lens (\TeletextDestinationSettings' {pageTypes} -> pageTypes) (\s@TeletextDestinationSettings' {} a -> s {pageTypes = a} :: TeletextDestinationSettings) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TeletextDestinationSettings where
  parseJSON =
    Data.withObject
      "TeletextDestinationSettings"
      ( \x ->
          TeletextDestinationSettings'
            Prelude.<$> (x Data..:? "pageNumber")
            Prelude.<*> (x Data..:? "pageTypes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TeletextDestinationSettings where
  hashWithSalt _salt TeletextDestinationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` pageNumber
      `Prelude.hashWithSalt` pageTypes

instance Prelude.NFData TeletextDestinationSettings where
  rnf TeletextDestinationSettings' {..} =
    Prelude.rnf pageNumber
      `Prelude.seq` Prelude.rnf pageTypes

instance Data.ToJSON TeletextDestinationSettings where
  toJSON TeletextDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("pageNumber" Data..=) Prelude.<$> pageNumber,
            ("pageTypes" Data..=) Prelude.<$> pageTypes
          ]
      )
