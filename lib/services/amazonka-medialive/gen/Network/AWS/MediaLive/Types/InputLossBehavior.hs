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
-- Module      : Network.AWS.MediaLive.Types.InputLossBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossBehavior where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputLocation
import Network.AWS.MediaLive.Types.InputLossImageType
import qualified Network.AWS.Prelude as Prelude

-- | Input Loss Behavior
--
-- /See:/ 'newInputLossBehavior' smart constructor.
data InputLossBehavior = InputLossBehavior'
  { -- | Documentation update needed
    blackFrameMsec :: Prelude.Maybe Prelude.Natural,
    -- | When input loss image type is \"color\" this field specifies the color
    -- to use. Value: 6 hex characters representing the values of RGB.
    inputLossImageColor :: Prelude.Maybe Prelude.Text,
    -- | When input loss image type is \"slate\" these fields specify the
    -- parameters for accessing the slate.
    inputLossImageSlate :: Prelude.Maybe InputLocation,
    -- | Documentation update needed
    repeatFrameMsec :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether to substitute a solid color or a slate into the output
    -- after input loss exceeds blackFrameMsec.
    inputLossImageType :: Prelude.Maybe InputLossImageType
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
-- 'blackFrameMsec', 'inputLossBehavior_blackFrameMsec' - Documentation update needed
--
-- 'inputLossImageColor', 'inputLossBehavior_inputLossImageColor' - When input loss image type is \"color\" this field specifies the color
-- to use. Value: 6 hex characters representing the values of RGB.
--
-- 'inputLossImageSlate', 'inputLossBehavior_inputLossImageSlate' - When input loss image type is \"slate\" these fields specify the
-- parameters for accessing the slate.
--
-- 'repeatFrameMsec', 'inputLossBehavior_repeatFrameMsec' - Documentation update needed
--
-- 'inputLossImageType', 'inputLossBehavior_inputLossImageType' - Indicates whether to substitute a solid color or a slate into the output
-- after input loss exceeds blackFrameMsec.
newInputLossBehavior ::
  InputLossBehavior
newInputLossBehavior =
  InputLossBehavior'
    { blackFrameMsec =
        Prelude.Nothing,
      inputLossImageColor = Prelude.Nothing,
      inputLossImageSlate = Prelude.Nothing,
      repeatFrameMsec = Prelude.Nothing,
      inputLossImageType = Prelude.Nothing
    }

-- | Documentation update needed
inputLossBehavior_blackFrameMsec :: Lens.Lens' InputLossBehavior (Prelude.Maybe Prelude.Natural)
inputLossBehavior_blackFrameMsec = Lens.lens (\InputLossBehavior' {blackFrameMsec} -> blackFrameMsec) (\s@InputLossBehavior' {} a -> s {blackFrameMsec = a} :: InputLossBehavior)

-- | When input loss image type is \"color\" this field specifies the color
-- to use. Value: 6 hex characters representing the values of RGB.
inputLossBehavior_inputLossImageColor :: Lens.Lens' InputLossBehavior (Prelude.Maybe Prelude.Text)
inputLossBehavior_inputLossImageColor = Lens.lens (\InputLossBehavior' {inputLossImageColor} -> inputLossImageColor) (\s@InputLossBehavior' {} a -> s {inputLossImageColor = a} :: InputLossBehavior)

-- | When input loss image type is \"slate\" these fields specify the
-- parameters for accessing the slate.
inputLossBehavior_inputLossImageSlate :: Lens.Lens' InputLossBehavior (Prelude.Maybe InputLocation)
inputLossBehavior_inputLossImageSlate = Lens.lens (\InputLossBehavior' {inputLossImageSlate} -> inputLossImageSlate) (\s@InputLossBehavior' {} a -> s {inputLossImageSlate = a} :: InputLossBehavior)

-- | Documentation update needed
inputLossBehavior_repeatFrameMsec :: Lens.Lens' InputLossBehavior (Prelude.Maybe Prelude.Natural)
inputLossBehavior_repeatFrameMsec = Lens.lens (\InputLossBehavior' {repeatFrameMsec} -> repeatFrameMsec) (\s@InputLossBehavior' {} a -> s {repeatFrameMsec = a} :: InputLossBehavior)

-- | Indicates whether to substitute a solid color or a slate into the output
-- after input loss exceeds blackFrameMsec.
inputLossBehavior_inputLossImageType :: Lens.Lens' InputLossBehavior (Prelude.Maybe InputLossImageType)
inputLossBehavior_inputLossImageType = Lens.lens (\InputLossBehavior' {inputLossImageType} -> inputLossImageType) (\s@InputLossBehavior' {} a -> s {inputLossImageType = a} :: InputLossBehavior)

instance Core.FromJSON InputLossBehavior where
  parseJSON =
    Core.withObject
      "InputLossBehavior"
      ( \x ->
          InputLossBehavior'
            Prelude.<$> (x Core..:? "blackFrameMsec")
            Prelude.<*> (x Core..:? "inputLossImageColor")
            Prelude.<*> (x Core..:? "inputLossImageSlate")
            Prelude.<*> (x Core..:? "repeatFrameMsec")
            Prelude.<*> (x Core..:? "inputLossImageType")
      )

instance Prelude.Hashable InputLossBehavior

instance Prelude.NFData InputLossBehavior

instance Core.ToJSON InputLossBehavior where
  toJSON InputLossBehavior' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("blackFrameMsec" Core..=)
              Prelude.<$> blackFrameMsec,
            ("inputLossImageColor" Core..=)
              Prelude.<$> inputLossImageColor,
            ("inputLossImageSlate" Core..=)
              Prelude.<$> inputLossImageSlate,
            ("repeatFrameMsec" Core..=)
              Prelude.<$> repeatFrameMsec,
            ("inputLossImageType" Core..=)
              Prelude.<$> inputLossImageType
          ]
      )
