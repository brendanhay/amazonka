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
-- Module      : Amazonka.LexV2Models.Types.TextLogSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TextLogSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.TextLogDestination
import qualified Amazonka.Prelude as Prelude

-- | Defines settings to enable text conversation logs.
--
-- /See:/ 'newTextLogSetting' smart constructor.
data TextLogSetting = TextLogSetting'
  { -- | Determines whether conversation logs should be stored for an alias.
    enabled :: Prelude.Bool,
    destination :: TextLogDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextLogSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'textLogSetting_enabled' - Determines whether conversation logs should be stored for an alias.
--
-- 'destination', 'textLogSetting_destination' - Undocumented member.
newTextLogSetting ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'destination'
  TextLogDestination ->
  TextLogSetting
newTextLogSetting pEnabled_ pDestination_ =
  TextLogSetting'
    { enabled = pEnabled_,
      destination = pDestination_
    }

-- | Determines whether conversation logs should be stored for an alias.
textLogSetting_enabled :: Lens.Lens' TextLogSetting Prelude.Bool
textLogSetting_enabled = Lens.lens (\TextLogSetting' {enabled} -> enabled) (\s@TextLogSetting' {} a -> s {enabled = a} :: TextLogSetting)

-- | Undocumented member.
textLogSetting_destination :: Lens.Lens' TextLogSetting TextLogDestination
textLogSetting_destination = Lens.lens (\TextLogSetting' {destination} -> destination) (\s@TextLogSetting' {} a -> s {destination = a} :: TextLogSetting)

instance Core.FromJSON TextLogSetting where
  parseJSON =
    Core.withObject
      "TextLogSetting"
      ( \x ->
          TextLogSetting'
            Prelude.<$> (x Core..: "enabled")
            Prelude.<*> (x Core..: "destination")
      )

instance Prelude.Hashable TextLogSetting where
  hashWithSalt _salt TextLogSetting' {..} =
    _salt `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` destination

instance Prelude.NFData TextLogSetting where
  rnf TextLogSetting' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf destination

instance Core.ToJSON TextLogSetting where
  toJSON TextLogSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("enabled" Core..= enabled),
            Prelude.Just ("destination" Core..= destination)
          ]
      )
