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
-- Module      : Amazonka.Pinpoint.Types.InAppMessageBodyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.InAppMessageBodyConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.Alignment
import qualified Amazonka.Prelude as Prelude

-- | Text config for Message Body.
--
-- /See:/ 'newInAppMessageBodyConfig' smart constructor.
data InAppMessageBodyConfig = InAppMessageBodyConfig'
  { -- | The alignment of the text. Valid values: LEFT, CENTER, RIGHT.
    alignment :: Alignment,
    -- | The text color.
    textColor :: Prelude.Text,
    -- | Message Body.
    body :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InAppMessageBodyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alignment', 'inAppMessageBodyConfig_alignment' - The alignment of the text. Valid values: LEFT, CENTER, RIGHT.
--
-- 'textColor', 'inAppMessageBodyConfig_textColor' - The text color.
--
-- 'body', 'inAppMessageBodyConfig_body' - Message Body.
newInAppMessageBodyConfig ::
  -- | 'alignment'
  Alignment ->
  -- | 'textColor'
  Prelude.Text ->
  -- | 'body'
  Prelude.Text ->
  InAppMessageBodyConfig
newInAppMessageBodyConfig
  pAlignment_
  pTextColor_
  pBody_ =
    InAppMessageBodyConfig'
      { alignment = pAlignment_,
        textColor = pTextColor_,
        body = pBody_
      }

-- | The alignment of the text. Valid values: LEFT, CENTER, RIGHT.
inAppMessageBodyConfig_alignment :: Lens.Lens' InAppMessageBodyConfig Alignment
inAppMessageBodyConfig_alignment = Lens.lens (\InAppMessageBodyConfig' {alignment} -> alignment) (\s@InAppMessageBodyConfig' {} a -> s {alignment = a} :: InAppMessageBodyConfig)

-- | The text color.
inAppMessageBodyConfig_textColor :: Lens.Lens' InAppMessageBodyConfig Prelude.Text
inAppMessageBodyConfig_textColor = Lens.lens (\InAppMessageBodyConfig' {textColor} -> textColor) (\s@InAppMessageBodyConfig' {} a -> s {textColor = a} :: InAppMessageBodyConfig)

-- | Message Body.
inAppMessageBodyConfig_body :: Lens.Lens' InAppMessageBodyConfig Prelude.Text
inAppMessageBodyConfig_body = Lens.lens (\InAppMessageBodyConfig' {body} -> body) (\s@InAppMessageBodyConfig' {} a -> s {body = a} :: InAppMessageBodyConfig)

instance Data.FromJSON InAppMessageBodyConfig where
  parseJSON =
    Data.withObject
      "InAppMessageBodyConfig"
      ( \x ->
          InAppMessageBodyConfig'
            Prelude.<$> (x Data..: "Alignment")
            Prelude.<*> (x Data..: "TextColor")
            Prelude.<*> (x Data..: "Body")
      )

instance Prelude.Hashable InAppMessageBodyConfig where
  hashWithSalt _salt InAppMessageBodyConfig' {..} =
    _salt
      `Prelude.hashWithSalt` alignment
      `Prelude.hashWithSalt` textColor
      `Prelude.hashWithSalt` body

instance Prelude.NFData InAppMessageBodyConfig where
  rnf InAppMessageBodyConfig' {..} =
    Prelude.rnf alignment
      `Prelude.seq` Prelude.rnf textColor
      `Prelude.seq` Prelude.rnf body

instance Data.ToJSON InAppMessageBodyConfig where
  toJSON InAppMessageBodyConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Alignment" Data..= alignment),
            Prelude.Just ("TextColor" Data..= textColor),
            Prelude.Just ("Body" Data..= body)
          ]
      )
