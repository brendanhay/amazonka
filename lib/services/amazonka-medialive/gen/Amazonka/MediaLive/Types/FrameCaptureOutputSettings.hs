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
-- Module      : Amazonka.MediaLive.Types.FrameCaptureOutputSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FrameCaptureOutputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Frame Capture Output Settings
--
-- /See:/ 'newFrameCaptureOutputSettings' smart constructor.
data FrameCaptureOutputSettings = FrameCaptureOutputSettings'
  { -- | Required if the output group contains more than one output. This
    -- modifier forms part of the output file name.
    nameModifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FrameCaptureOutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameModifier', 'frameCaptureOutputSettings_nameModifier' - Required if the output group contains more than one output. This
-- modifier forms part of the output file name.
newFrameCaptureOutputSettings ::
  FrameCaptureOutputSettings
newFrameCaptureOutputSettings =
  FrameCaptureOutputSettings'
    { nameModifier =
        Prelude.Nothing
    }

-- | Required if the output group contains more than one output. This
-- modifier forms part of the output file name.
frameCaptureOutputSettings_nameModifier :: Lens.Lens' FrameCaptureOutputSettings (Prelude.Maybe Prelude.Text)
frameCaptureOutputSettings_nameModifier = Lens.lens (\FrameCaptureOutputSettings' {nameModifier} -> nameModifier) (\s@FrameCaptureOutputSettings' {} a -> s {nameModifier = a} :: FrameCaptureOutputSettings)

instance Data.FromJSON FrameCaptureOutputSettings where
  parseJSON =
    Data.withObject
      "FrameCaptureOutputSettings"
      ( \x ->
          FrameCaptureOutputSettings'
            Prelude.<$> (x Data..:? "nameModifier")
      )

instance Prelude.Hashable FrameCaptureOutputSettings where
  hashWithSalt _salt FrameCaptureOutputSettings' {..} =
    _salt `Prelude.hashWithSalt` nameModifier

instance Prelude.NFData FrameCaptureOutputSettings where
  rnf FrameCaptureOutputSettings' {..} =
    Prelude.rnf nameModifier

instance Data.ToJSON FrameCaptureOutputSettings where
  toJSON FrameCaptureOutputSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("nameModifier" Data..=) Prelude.<$> nameModifier]
      )
