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
-- Module      : Amazonka.MediaPackage.Types.DashEncryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.DashEncryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types.SpekeKeyProvider
import qualified Amazonka.Prelude as Prelude

-- | A Dynamic Adaptive Streaming over HTTP (DASH) encryption configuration.
--
-- /See:/ 'newDashEncryption' smart constructor.
data DashEncryption = DashEncryption'
  { -- | Time (in seconds) between each encryption key rotation.
    keyRotationIntervalSeconds :: Prelude.Maybe Prelude.Int,
    spekeKeyProvider :: SpekeKeyProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyRotationIntervalSeconds', 'dashEncryption_keyRotationIntervalSeconds' - Time (in seconds) between each encryption key rotation.
--
-- 'spekeKeyProvider', 'dashEncryption_spekeKeyProvider' - Undocumented member.
newDashEncryption ::
  -- | 'spekeKeyProvider'
  SpekeKeyProvider ->
  DashEncryption
newDashEncryption pSpekeKeyProvider_ =
  DashEncryption'
    { keyRotationIntervalSeconds =
        Prelude.Nothing,
      spekeKeyProvider = pSpekeKeyProvider_
    }

-- | Time (in seconds) between each encryption key rotation.
dashEncryption_keyRotationIntervalSeconds :: Lens.Lens' DashEncryption (Prelude.Maybe Prelude.Int)
dashEncryption_keyRotationIntervalSeconds = Lens.lens (\DashEncryption' {keyRotationIntervalSeconds} -> keyRotationIntervalSeconds) (\s@DashEncryption' {} a -> s {keyRotationIntervalSeconds = a} :: DashEncryption)

-- | Undocumented member.
dashEncryption_spekeKeyProvider :: Lens.Lens' DashEncryption SpekeKeyProvider
dashEncryption_spekeKeyProvider = Lens.lens (\DashEncryption' {spekeKeyProvider} -> spekeKeyProvider) (\s@DashEncryption' {} a -> s {spekeKeyProvider = a} :: DashEncryption)

instance Data.FromJSON DashEncryption where
  parseJSON =
    Data.withObject
      "DashEncryption"
      ( \x ->
          DashEncryption'
            Prelude.<$> (x Data..:? "keyRotationIntervalSeconds")
            Prelude.<*> (x Data..: "spekeKeyProvider")
      )

instance Prelude.Hashable DashEncryption where
  hashWithSalt _salt DashEncryption' {..} =
    _salt
      `Prelude.hashWithSalt` keyRotationIntervalSeconds
      `Prelude.hashWithSalt` spekeKeyProvider

instance Prelude.NFData DashEncryption where
  rnf DashEncryption' {..} =
    Prelude.rnf keyRotationIntervalSeconds
      `Prelude.seq` Prelude.rnf spekeKeyProvider

instance Data.ToJSON DashEncryption where
  toJSON DashEncryption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keyRotationIntervalSeconds" Data..=)
              Prelude.<$> keyRotationIntervalSeconds,
            Prelude.Just
              ("spekeKeyProvider" Data..= spekeKeyProvider)
          ]
      )
