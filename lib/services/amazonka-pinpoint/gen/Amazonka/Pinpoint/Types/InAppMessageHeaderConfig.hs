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
-- Module      : Amazonka.Pinpoint.Types.InAppMessageHeaderConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.InAppMessageHeaderConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.Alignment
import qualified Amazonka.Prelude as Prelude

-- | Text config for Message Header.
--
-- /See:/ 'newInAppMessageHeaderConfig' smart constructor.
data InAppMessageHeaderConfig = InAppMessageHeaderConfig'
  { -- | The alignment of the text. Valid values: LEFT, CENTER, RIGHT.
    alignment :: Alignment,
    -- | Message Header.
    header :: Prelude.Text,
    -- | The text color.
    textColor :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InAppMessageHeaderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alignment', 'inAppMessageHeaderConfig_alignment' - The alignment of the text. Valid values: LEFT, CENTER, RIGHT.
--
-- 'header', 'inAppMessageHeaderConfig_header' - Message Header.
--
-- 'textColor', 'inAppMessageHeaderConfig_textColor' - The text color.
newInAppMessageHeaderConfig ::
  -- | 'alignment'
  Alignment ->
  -- | 'header'
  Prelude.Text ->
  -- | 'textColor'
  Prelude.Text ->
  InAppMessageHeaderConfig
newInAppMessageHeaderConfig
  pAlignment_
  pHeader_
  pTextColor_ =
    InAppMessageHeaderConfig'
      { alignment = pAlignment_,
        header = pHeader_,
        textColor = pTextColor_
      }

-- | The alignment of the text. Valid values: LEFT, CENTER, RIGHT.
inAppMessageHeaderConfig_alignment :: Lens.Lens' InAppMessageHeaderConfig Alignment
inAppMessageHeaderConfig_alignment = Lens.lens (\InAppMessageHeaderConfig' {alignment} -> alignment) (\s@InAppMessageHeaderConfig' {} a -> s {alignment = a} :: InAppMessageHeaderConfig)

-- | Message Header.
inAppMessageHeaderConfig_header :: Lens.Lens' InAppMessageHeaderConfig Prelude.Text
inAppMessageHeaderConfig_header = Lens.lens (\InAppMessageHeaderConfig' {header} -> header) (\s@InAppMessageHeaderConfig' {} a -> s {header = a} :: InAppMessageHeaderConfig)

-- | The text color.
inAppMessageHeaderConfig_textColor :: Lens.Lens' InAppMessageHeaderConfig Prelude.Text
inAppMessageHeaderConfig_textColor = Lens.lens (\InAppMessageHeaderConfig' {textColor} -> textColor) (\s@InAppMessageHeaderConfig' {} a -> s {textColor = a} :: InAppMessageHeaderConfig)

instance Data.FromJSON InAppMessageHeaderConfig where
  parseJSON =
    Data.withObject
      "InAppMessageHeaderConfig"
      ( \x ->
          InAppMessageHeaderConfig'
            Prelude.<$> (x Data..: "Alignment")
            Prelude.<*> (x Data..: "Header")
            Prelude.<*> (x Data..: "TextColor")
      )

instance Prelude.Hashable InAppMessageHeaderConfig where
  hashWithSalt _salt InAppMessageHeaderConfig' {..} =
    _salt
      `Prelude.hashWithSalt` alignment
      `Prelude.hashWithSalt` header
      `Prelude.hashWithSalt` textColor

instance Prelude.NFData InAppMessageHeaderConfig where
  rnf InAppMessageHeaderConfig' {..} =
    Prelude.rnf alignment
      `Prelude.seq` Prelude.rnf header
      `Prelude.seq` Prelude.rnf textColor

instance Data.ToJSON InAppMessageHeaderConfig where
  toJSON InAppMessageHeaderConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Alignment" Data..= alignment),
            Prelude.Just ("Header" Data..= header),
            Prelude.Just ("TextColor" Data..= textColor)
          ]
      )
