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
-- Module      : Network.AWS.MediaLive.Types.AvailConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailConfiguration where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AvailSettings
import qualified Network.AWS.Prelude as Prelude

-- | Avail Configuration
--
-- /See:/ 'newAvailConfiguration' smart constructor.
data AvailConfiguration = AvailConfiguration'
  { -- | Ad avail settings.
    availSettings :: Prelude.Maybe AvailSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON AvailConfiguration where
  parseJSON =
    Prelude.withObject
      "AvailConfiguration"
      ( \x ->
          AvailConfiguration'
            Prelude.<$> (x Prelude..:? "availSettings")
      )

instance Prelude.Hashable AvailConfiguration

instance Prelude.NFData AvailConfiguration

instance Prelude.ToJSON AvailConfiguration where
  toJSON AvailConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("availSettings" Prelude..=)
              Prelude.<$> availSettings
          ]
      )
