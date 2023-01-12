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
-- Module      : Amazonka.MediaLive.Types.MotionGraphicsSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MotionGraphicsSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.HtmlMotionGraphicsSettings
import qualified Amazonka.Prelude as Prelude

-- | Motion Graphics Settings
--
-- /See:/ 'newMotionGraphicsSettings' smart constructor.
data MotionGraphicsSettings = MotionGraphicsSettings'
  { htmlMotionGraphicsSettings :: Prelude.Maybe HtmlMotionGraphicsSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MotionGraphicsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'htmlMotionGraphicsSettings', 'motionGraphicsSettings_htmlMotionGraphicsSettings' - Undocumented member.
newMotionGraphicsSettings ::
  MotionGraphicsSettings
newMotionGraphicsSettings =
  MotionGraphicsSettings'
    { htmlMotionGraphicsSettings =
        Prelude.Nothing
    }

-- | Undocumented member.
motionGraphicsSettings_htmlMotionGraphicsSettings :: Lens.Lens' MotionGraphicsSettings (Prelude.Maybe HtmlMotionGraphicsSettings)
motionGraphicsSettings_htmlMotionGraphicsSettings = Lens.lens (\MotionGraphicsSettings' {htmlMotionGraphicsSettings} -> htmlMotionGraphicsSettings) (\s@MotionGraphicsSettings' {} a -> s {htmlMotionGraphicsSettings = a} :: MotionGraphicsSettings)

instance Data.FromJSON MotionGraphicsSettings where
  parseJSON =
    Data.withObject
      "MotionGraphicsSettings"
      ( \x ->
          MotionGraphicsSettings'
            Prelude.<$> (x Data..:? "htmlMotionGraphicsSettings")
      )

instance Prelude.Hashable MotionGraphicsSettings where
  hashWithSalt _salt MotionGraphicsSettings' {..} =
    _salt
      `Prelude.hashWithSalt` htmlMotionGraphicsSettings

instance Prelude.NFData MotionGraphicsSettings where
  rnf MotionGraphicsSettings' {..} =
    Prelude.rnf htmlMotionGraphicsSettings

instance Data.ToJSON MotionGraphicsSettings where
  toJSON MotionGraphicsSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("htmlMotionGraphicsSettings" Data..=)
              Prelude.<$> htmlMotionGraphicsSettings
          ]
      )
