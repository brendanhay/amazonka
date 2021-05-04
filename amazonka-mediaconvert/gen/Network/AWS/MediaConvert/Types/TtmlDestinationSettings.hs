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
-- Module      : Network.AWS.MediaConvert.Types.TtmlDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TtmlDestinationSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.TtmlStylePassthrough
import qualified Network.AWS.Prelude as Prelude

-- | Settings specific to TTML caption outputs, including Pass style
-- information (TtmlStylePassthrough).
--
-- /See:/ 'newTtmlDestinationSettings' smart constructor.
data TtmlDestinationSettings = TtmlDestinationSettings'
  { -- | Pass through style and position information from a TTML-like input
    -- source (TTML, SMPTE-TT) to the TTML output.
    stylePassthrough :: Prelude.Maybe TtmlStylePassthrough
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TtmlDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stylePassthrough', 'ttmlDestinationSettings_stylePassthrough' - Pass through style and position information from a TTML-like input
-- source (TTML, SMPTE-TT) to the TTML output.
newTtmlDestinationSettings ::
  TtmlDestinationSettings
newTtmlDestinationSettings =
  TtmlDestinationSettings'
    { stylePassthrough =
        Prelude.Nothing
    }

-- | Pass through style and position information from a TTML-like input
-- source (TTML, SMPTE-TT) to the TTML output.
ttmlDestinationSettings_stylePassthrough :: Lens.Lens' TtmlDestinationSettings (Prelude.Maybe TtmlStylePassthrough)
ttmlDestinationSettings_stylePassthrough = Lens.lens (\TtmlDestinationSettings' {stylePassthrough} -> stylePassthrough) (\s@TtmlDestinationSettings' {} a -> s {stylePassthrough = a} :: TtmlDestinationSettings)

instance Prelude.FromJSON TtmlDestinationSettings where
  parseJSON =
    Prelude.withObject
      "TtmlDestinationSettings"
      ( \x ->
          TtmlDestinationSettings'
            Prelude.<$> (x Prelude..:? "stylePassthrough")
      )

instance Prelude.Hashable TtmlDestinationSettings

instance Prelude.NFData TtmlDestinationSettings

instance Prelude.ToJSON TtmlDestinationSettings where
  toJSON TtmlDestinationSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("stylePassthrough" Prelude..=)
              Prelude.<$> stylePassthrough
          ]
      )
