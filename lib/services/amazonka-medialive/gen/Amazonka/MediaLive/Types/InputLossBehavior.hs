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
-- Module      : Amazonka.MediaLive.Types.InputLossBehavior
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputLossBehavior where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.InputLocation
import Amazonka.MediaLive.Types.InputLossImageType
import qualified Amazonka.Prelude as Prelude

-- | Input Loss Behavior
--
-- /See:/ 'newInputLossBehavior' smart constructor.
data InputLossBehavior = InputLossBehavior'
  { -- | When input loss image type is \"slate\" these fields specify the
    -- parameters for accessing the slate.
    inputLossImageSlate :: Prelude.Maybe InputLocation,
    -- | Indicates whether to substitute a solid color or a slate into the output
    -- after input loss exceeds blackFrameMsec.
    inputLossImageType :: Prelude.Maybe InputLossImageType,
    -- | Documentation update needed
    blackFrameMsec :: Prelude.Maybe Prelude.Natural,
    -- | When input loss image type is \"color\" this field specifies the color
    -- to use. Value: 6 hex characters representing the values of RGB.
    inputLossImageColor :: Prelude.Maybe Prelude.Text,
    -- | Documentation update needed
    repeatFrameMsec :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputLossBehavior' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputLossImageSlate', 'inputLossBehavior_inputLossImageSlate' - When input loss image type is \"slate\" these fields specify the
-- parameters for accessing the slate.
--
-- 'inputLossImageType', 'inputLossBehavior_inputLossImageType' - Indicates whether to substitute a solid color or a slate into the output
-- after input loss exceeds blackFrameMsec.
--
-- 'blackFrameMsec', 'inputLossBehavior_blackFrameMsec' - Documentation update needed
--
-- 'inputLossImageColor', 'inputLossBehavior_inputLossImageColor' - When input loss image type is \"color\" this field specifies the color
-- to use. Value: 6 hex characters representing the values of RGB.
--
-- 'repeatFrameMsec', 'inputLossBehavior_repeatFrameMsec' - Documentation update needed
newInputLossBehavior ::
  InputLossBehavior
newInputLossBehavior =
  InputLossBehavior'
    { inputLossImageSlate =
        Prelude.Nothing,
      inputLossImageType = Prelude.Nothing,
      blackFrameMsec = Prelude.Nothing,
      inputLossImageColor = Prelude.Nothing,
      repeatFrameMsec = Prelude.Nothing
    }

-- | When input loss image type is \"slate\" these fields specify the
-- parameters for accessing the slate.
inputLossBehavior_inputLossImageSlate :: Lens.Lens' InputLossBehavior (Prelude.Maybe InputLocation)
inputLossBehavior_inputLossImageSlate = Lens.lens (\InputLossBehavior' {inputLossImageSlate} -> inputLossImageSlate) (\s@InputLossBehavior' {} a -> s {inputLossImageSlate = a} :: InputLossBehavior)

-- | Indicates whether to substitute a solid color or a slate into the output
-- after input loss exceeds blackFrameMsec.
inputLossBehavior_inputLossImageType :: Lens.Lens' InputLossBehavior (Prelude.Maybe InputLossImageType)
inputLossBehavior_inputLossImageType = Lens.lens (\InputLossBehavior' {inputLossImageType} -> inputLossImageType) (\s@InputLossBehavior' {} a -> s {inputLossImageType = a} :: InputLossBehavior)

-- | Documentation update needed
inputLossBehavior_blackFrameMsec :: Lens.Lens' InputLossBehavior (Prelude.Maybe Prelude.Natural)
inputLossBehavior_blackFrameMsec = Lens.lens (\InputLossBehavior' {blackFrameMsec} -> blackFrameMsec) (\s@InputLossBehavior' {} a -> s {blackFrameMsec = a} :: InputLossBehavior)

-- | When input loss image type is \"color\" this field specifies the color
-- to use. Value: 6 hex characters representing the values of RGB.
inputLossBehavior_inputLossImageColor :: Lens.Lens' InputLossBehavior (Prelude.Maybe Prelude.Text)
inputLossBehavior_inputLossImageColor = Lens.lens (\InputLossBehavior' {inputLossImageColor} -> inputLossImageColor) (\s@InputLossBehavior' {} a -> s {inputLossImageColor = a} :: InputLossBehavior)

-- | Documentation update needed
inputLossBehavior_repeatFrameMsec :: Lens.Lens' InputLossBehavior (Prelude.Maybe Prelude.Natural)
inputLossBehavior_repeatFrameMsec = Lens.lens (\InputLossBehavior' {repeatFrameMsec} -> repeatFrameMsec) (\s@InputLossBehavior' {} a -> s {repeatFrameMsec = a} :: InputLossBehavior)

instance Data.FromJSON InputLossBehavior where
  parseJSON =
    Data.withObject
      "InputLossBehavior"
      ( \x ->
          InputLossBehavior'
            Prelude.<$> (x Data..:? "inputLossImageSlate")
            Prelude.<*> (x Data..:? "inputLossImageType")
            Prelude.<*> (x Data..:? "blackFrameMsec")
            Prelude.<*> (x Data..:? "inputLossImageColor")
            Prelude.<*> (x Data..:? "repeatFrameMsec")
      )

instance Prelude.Hashable InputLossBehavior where
  hashWithSalt _salt InputLossBehavior' {..} =
    _salt `Prelude.hashWithSalt` inputLossImageSlate
      `Prelude.hashWithSalt` inputLossImageType
      `Prelude.hashWithSalt` blackFrameMsec
      `Prelude.hashWithSalt` inputLossImageColor
      `Prelude.hashWithSalt` repeatFrameMsec

instance Prelude.NFData InputLossBehavior where
  rnf InputLossBehavior' {..} =
    Prelude.rnf inputLossImageSlate
      `Prelude.seq` Prelude.rnf inputLossImageType
      `Prelude.seq` Prelude.rnf blackFrameMsec
      `Prelude.seq` Prelude.rnf inputLossImageColor
      `Prelude.seq` Prelude.rnf repeatFrameMsec

instance Data.ToJSON InputLossBehavior where
  toJSON InputLossBehavior' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inputLossImageSlate" Data..=)
              Prelude.<$> inputLossImageSlate,
            ("inputLossImageType" Data..=)
              Prelude.<$> inputLossImageType,
            ("blackFrameMsec" Data..=)
              Prelude.<$> blackFrameMsec,
            ("inputLossImageColor" Data..=)
              Prelude.<$> inputLossImageColor,
            ("repeatFrameMsec" Data..=)
              Prelude.<$> repeatFrameMsec
          ]
      )
