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
-- Module      : Network.AWS.MediaConvert.Types.F4vSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.F4vSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.F4vMoovPlacement
import qualified Network.AWS.Prelude as Prelude

-- | Settings for F4v container
--
-- /See:/ 'newF4vSettings' smart constructor.
data F4vSettings = F4vSettings'
  { -- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
    -- beginning of the archive as required for progressive downloading.
    -- Otherwise it is placed normally at the end.
    moovPlacement :: Prelude.Maybe F4vMoovPlacement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'F4vSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'moovPlacement', 'f4vSettings_moovPlacement' - If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
-- beginning of the archive as required for progressive downloading.
-- Otherwise it is placed normally at the end.
newF4vSettings ::
  F4vSettings
newF4vSettings =
  F4vSettings' {moovPlacement = Prelude.Nothing}

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
-- beginning of the archive as required for progressive downloading.
-- Otherwise it is placed normally at the end.
f4vSettings_moovPlacement :: Lens.Lens' F4vSettings (Prelude.Maybe F4vMoovPlacement)
f4vSettings_moovPlacement = Lens.lens (\F4vSettings' {moovPlacement} -> moovPlacement) (\s@F4vSettings' {} a -> s {moovPlacement = a} :: F4vSettings)

instance Prelude.FromJSON F4vSettings where
  parseJSON =
    Prelude.withObject
      "F4vSettings"
      ( \x ->
          F4vSettings'
            Prelude.<$> (x Prelude..:? "moovPlacement")
      )

instance Prelude.Hashable F4vSettings

instance Prelude.NFData F4vSettings

instance Prelude.ToJSON F4vSettings where
  toJSON F4vSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("moovPlacement" Prelude..=)
              Prelude.<$> moovPlacement
          ]
      )
