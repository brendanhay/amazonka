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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ExpirationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ExpirationSettings where

import Amazonka.ChimeSDKMessaging.Types.ExpirationCriterion
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings that control the interval after which a channel is deleted.
--
-- /See:/ 'newExpirationSettings' smart constructor.
data ExpirationSettings = ExpirationSettings'
  { -- | The period in days after which the system automatically deletes a
    -- channel.
    expirationDays :: Prelude.Natural,
    -- | The conditions that must be met for a channel to expire.
    expirationCriterion :: ExpirationCriterion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExpirationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationDays', 'expirationSettings_expirationDays' - The period in days after which the system automatically deletes a
-- channel.
--
-- 'expirationCriterion', 'expirationSettings_expirationCriterion' - The conditions that must be met for a channel to expire.
newExpirationSettings ::
  -- | 'expirationDays'
  Prelude.Natural ->
  -- | 'expirationCriterion'
  ExpirationCriterion ->
  ExpirationSettings
newExpirationSettings
  pExpirationDays_
  pExpirationCriterion_ =
    ExpirationSettings'
      { expirationDays =
          pExpirationDays_,
        expirationCriterion = pExpirationCriterion_
      }

-- | The period in days after which the system automatically deletes a
-- channel.
expirationSettings_expirationDays :: Lens.Lens' ExpirationSettings Prelude.Natural
expirationSettings_expirationDays = Lens.lens (\ExpirationSettings' {expirationDays} -> expirationDays) (\s@ExpirationSettings' {} a -> s {expirationDays = a} :: ExpirationSettings)

-- | The conditions that must be met for a channel to expire.
expirationSettings_expirationCriterion :: Lens.Lens' ExpirationSettings ExpirationCriterion
expirationSettings_expirationCriterion = Lens.lens (\ExpirationSettings' {expirationCriterion} -> expirationCriterion) (\s@ExpirationSettings' {} a -> s {expirationCriterion = a} :: ExpirationSettings)

instance Data.FromJSON ExpirationSettings where
  parseJSON =
    Data.withObject
      "ExpirationSettings"
      ( \x ->
          ExpirationSettings'
            Prelude.<$> (x Data..: "ExpirationDays")
            Prelude.<*> (x Data..: "ExpirationCriterion")
      )

instance Prelude.Hashable ExpirationSettings where
  hashWithSalt _salt ExpirationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` expirationDays
      `Prelude.hashWithSalt` expirationCriterion

instance Prelude.NFData ExpirationSettings where
  rnf ExpirationSettings' {..} =
    Prelude.rnf expirationDays
      `Prelude.seq` Prelude.rnf expirationCriterion

instance Data.ToJSON ExpirationSettings where
  toJSON ExpirationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ExpirationDays" Data..= expirationDays),
            Prelude.Just
              ("ExpirationCriterion" Data..= expirationCriterion)
          ]
      )
