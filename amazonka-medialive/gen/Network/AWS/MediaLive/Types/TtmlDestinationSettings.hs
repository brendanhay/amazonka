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
-- Module      : Network.AWS.MediaLive.Types.TtmlDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TtmlDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.TtmlDestinationStyleControl

-- | Ttml Destination Settings
--
-- /See:/ 'newTtmlDestinationSettings' smart constructor.
data TtmlDestinationSettings = TtmlDestinationSettings'
  { -- | When set to passthrough, passes through style and position information
    -- from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT
    -- output or TTML output.
    styleControl :: Core.Maybe TtmlDestinationStyleControl
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TtmlDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'styleControl', 'ttmlDestinationSettings_styleControl' - When set to passthrough, passes through style and position information
-- from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT
-- output or TTML output.
newTtmlDestinationSettings ::
  TtmlDestinationSettings
newTtmlDestinationSettings =
  TtmlDestinationSettings'
    { styleControl =
        Core.Nothing
    }

-- | When set to passthrough, passes through style and position information
-- from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT
-- output or TTML output.
ttmlDestinationSettings_styleControl :: Lens.Lens' TtmlDestinationSettings (Core.Maybe TtmlDestinationStyleControl)
ttmlDestinationSettings_styleControl = Lens.lens (\TtmlDestinationSettings' {styleControl} -> styleControl) (\s@TtmlDestinationSettings' {} a -> s {styleControl = a} :: TtmlDestinationSettings)

instance Core.FromJSON TtmlDestinationSettings where
  parseJSON =
    Core.withObject
      "TtmlDestinationSettings"
      ( \x ->
          TtmlDestinationSettings'
            Core.<$> (x Core..:? "styleControl")
      )

instance Core.Hashable TtmlDestinationSettings

instance Core.NFData TtmlDestinationSettings

instance Core.ToJSON TtmlDestinationSettings where
  toJSON TtmlDestinationSettings' {..} =
    Core.object
      ( Core.catMaybes
          [("styleControl" Core..=) Core.<$> styleControl]
      )
