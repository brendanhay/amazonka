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
-- Module      : Amazonka.MediaLive.Types.WebvttDestinationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.WebvttDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.WebvttDestinationStyleControl
import qualified Amazonka.Prelude as Prelude

-- | Webvtt Destination Settings
--
-- /See:/ 'newWebvttDestinationSettings' smart constructor.
data WebvttDestinationSettings = WebvttDestinationSettings'
  { -- | Controls whether the color and position of the source captions is passed
    -- through to the WebVTT output captions. PASSTHROUGH - Valid only if the
    -- source captions are EMBEDDED or TELETEXT. NO_STYLE_DATA - Don\'t pass
    -- through the style. The output captions will not contain any font styling
    -- information.
    styleControl :: Prelude.Maybe WebvttDestinationStyleControl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WebvttDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'styleControl', 'webvttDestinationSettings_styleControl' - Controls whether the color and position of the source captions is passed
-- through to the WebVTT output captions. PASSTHROUGH - Valid only if the
-- source captions are EMBEDDED or TELETEXT. NO_STYLE_DATA - Don\'t pass
-- through the style. The output captions will not contain any font styling
-- information.
newWebvttDestinationSettings ::
  WebvttDestinationSettings
newWebvttDestinationSettings =
  WebvttDestinationSettings'
    { styleControl =
        Prelude.Nothing
    }

-- | Controls whether the color and position of the source captions is passed
-- through to the WebVTT output captions. PASSTHROUGH - Valid only if the
-- source captions are EMBEDDED or TELETEXT. NO_STYLE_DATA - Don\'t pass
-- through the style. The output captions will not contain any font styling
-- information.
webvttDestinationSettings_styleControl :: Lens.Lens' WebvttDestinationSettings (Prelude.Maybe WebvttDestinationStyleControl)
webvttDestinationSettings_styleControl = Lens.lens (\WebvttDestinationSettings' {styleControl} -> styleControl) (\s@WebvttDestinationSettings' {} a -> s {styleControl = a} :: WebvttDestinationSettings)

instance Data.FromJSON WebvttDestinationSettings where
  parseJSON =
    Data.withObject
      "WebvttDestinationSettings"
      ( \x ->
          WebvttDestinationSettings'
            Prelude.<$> (x Data..:? "styleControl")
      )

instance Prelude.Hashable WebvttDestinationSettings where
  hashWithSalt _salt WebvttDestinationSettings' {..} =
    _salt `Prelude.hashWithSalt` styleControl

instance Prelude.NFData WebvttDestinationSettings where
  rnf WebvttDestinationSettings' {..} =
    Prelude.rnf styleControl

instance Data.ToJSON WebvttDestinationSettings where
  toJSON WebvttDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("styleControl" Data..=) Prelude.<$> styleControl]
      )
