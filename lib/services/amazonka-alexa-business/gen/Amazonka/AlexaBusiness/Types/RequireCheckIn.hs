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
-- Module      : Amazonka.AlexaBusiness.Types.RequireCheckIn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.RequireCheckIn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings for the require check in feature that are applied to a room
-- profile. Require check in allows a meeting room’s Alexa or AVS device to
-- prompt the user to check in; otherwise, the room will be released.
--
-- /See:/ 'newRequireCheckIn' smart constructor.
data RequireCheckIn = RequireCheckIn'
  { -- | Whether require check in is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Duration between 5 and 20 minutes to determine when to release the room
    -- if it\'s not checked into.
    releaseAfterMinutes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequireCheckIn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'requireCheckIn_enabled' - Whether require check in is enabled or not.
--
-- 'releaseAfterMinutes', 'requireCheckIn_releaseAfterMinutes' - Duration between 5 and 20 minutes to determine when to release the room
-- if it\'s not checked into.
newRequireCheckIn ::
  RequireCheckIn
newRequireCheckIn =
  RequireCheckIn'
    { enabled = Prelude.Nothing,
      releaseAfterMinutes = Prelude.Nothing
    }

-- | Whether require check in is enabled or not.
requireCheckIn_enabled :: Lens.Lens' RequireCheckIn (Prelude.Maybe Prelude.Bool)
requireCheckIn_enabled = Lens.lens (\RequireCheckIn' {enabled} -> enabled) (\s@RequireCheckIn' {} a -> s {enabled = a} :: RequireCheckIn)

-- | Duration between 5 and 20 minutes to determine when to release the room
-- if it\'s not checked into.
requireCheckIn_releaseAfterMinutes :: Lens.Lens' RequireCheckIn (Prelude.Maybe Prelude.Int)
requireCheckIn_releaseAfterMinutes = Lens.lens (\RequireCheckIn' {releaseAfterMinutes} -> releaseAfterMinutes) (\s@RequireCheckIn' {} a -> s {releaseAfterMinutes = a} :: RequireCheckIn)

instance Data.FromJSON RequireCheckIn where
  parseJSON =
    Data.withObject
      "RequireCheckIn"
      ( \x ->
          RequireCheckIn'
            Prelude.<$> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "ReleaseAfterMinutes")
      )

instance Prelude.Hashable RequireCheckIn where
  hashWithSalt _salt RequireCheckIn' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` releaseAfterMinutes

instance Prelude.NFData RequireCheckIn where
  rnf RequireCheckIn' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf releaseAfterMinutes
