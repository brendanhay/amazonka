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
-- Module      : Amazonka.MediaLive.Types.AvailConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AvailConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.AvailSettings
import qualified Amazonka.Prelude as Prelude

-- | Avail Configuration
--
-- /See:/ 'newAvailConfiguration' smart constructor.
data AvailConfiguration = AvailConfiguration'
  { -- | Ad avail settings.
    availSettings :: Prelude.Maybe AvailSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availSettings', 'availConfiguration_availSettings' - Ad avail settings.
newAvailConfiguration ::
  AvailConfiguration
newAvailConfiguration =
  AvailConfiguration'
    { availSettings =
        Prelude.Nothing
    }

-- | Ad avail settings.
availConfiguration_availSettings :: Lens.Lens' AvailConfiguration (Prelude.Maybe AvailSettings)
availConfiguration_availSettings = Lens.lens (\AvailConfiguration' {availSettings} -> availSettings) (\s@AvailConfiguration' {} a -> s {availSettings = a} :: AvailConfiguration)

instance Core.FromJSON AvailConfiguration where
  parseJSON =
    Core.withObject
      "AvailConfiguration"
      ( \x ->
          AvailConfiguration'
            Prelude.<$> (x Core..:? "availSettings")
      )

instance Prelude.Hashable AvailConfiguration where
  hashWithSalt _salt AvailConfiguration' {..} =
    _salt `Prelude.hashWithSalt` availSettings

instance Prelude.NFData AvailConfiguration where
  rnf AvailConfiguration' {..} =
    Prelude.rnf availSettings

instance Core.ToJSON AvailConfiguration where
  toJSON AvailConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("availSettings" Core..=)
              Prelude.<$> availSettings
          ]
      )
