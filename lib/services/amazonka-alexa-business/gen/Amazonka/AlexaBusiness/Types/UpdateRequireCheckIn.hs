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
-- Module      : Amazonka.AlexaBusiness.Types.UpdateRequireCheckIn
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.UpdateRequireCheckIn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Updates settings for the require check in feature that are applied to a
-- room profile. Require check in allows a meeting room’s Alexa or AVS
-- device to prompt the user to check in; otherwise, the room will be
-- released.
--
-- /See:/ 'newUpdateRequireCheckIn' smart constructor.
data UpdateRequireCheckIn = UpdateRequireCheckIn'
  { -- | Whether require check in is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Duration between 5 and 20 minutes to determine when to release the room
    -- if it\'s not checked into.
    releaseAfterMinutes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRequireCheckIn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'updateRequireCheckIn_enabled' - Whether require check in is enabled or not.
--
-- 'releaseAfterMinutes', 'updateRequireCheckIn_releaseAfterMinutes' - Duration between 5 and 20 minutes to determine when to release the room
-- if it\'s not checked into.
newUpdateRequireCheckIn ::
  UpdateRequireCheckIn
newUpdateRequireCheckIn =
  UpdateRequireCheckIn'
    { enabled = Prelude.Nothing,
      releaseAfterMinutes = Prelude.Nothing
    }

-- | Whether require check in is enabled or not.
updateRequireCheckIn_enabled :: Lens.Lens' UpdateRequireCheckIn (Prelude.Maybe Prelude.Bool)
updateRequireCheckIn_enabled = Lens.lens (\UpdateRequireCheckIn' {enabled} -> enabled) (\s@UpdateRequireCheckIn' {} a -> s {enabled = a} :: UpdateRequireCheckIn)

-- | Duration between 5 and 20 minutes to determine when to release the room
-- if it\'s not checked into.
updateRequireCheckIn_releaseAfterMinutes :: Lens.Lens' UpdateRequireCheckIn (Prelude.Maybe Prelude.Int)
updateRequireCheckIn_releaseAfterMinutes = Lens.lens (\UpdateRequireCheckIn' {releaseAfterMinutes} -> releaseAfterMinutes) (\s@UpdateRequireCheckIn' {} a -> s {releaseAfterMinutes = a} :: UpdateRequireCheckIn)

instance Prelude.Hashable UpdateRequireCheckIn where
  hashWithSalt _salt UpdateRequireCheckIn' {..} =
    _salt `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` releaseAfterMinutes

instance Prelude.NFData UpdateRequireCheckIn where
  rnf UpdateRequireCheckIn' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf releaseAfterMinutes

instance Core.ToJSON UpdateRequireCheckIn where
  toJSON UpdateRequireCheckIn' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Enabled" Core..=) Prelude.<$> enabled,
            ("ReleaseAfterMinutes" Core..=)
              Prelude.<$> releaseAfterMinutes
          ]
      )
