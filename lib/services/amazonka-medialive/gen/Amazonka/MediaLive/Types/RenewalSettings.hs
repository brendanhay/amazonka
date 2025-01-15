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
-- Module      : Amazonka.MediaLive.Types.RenewalSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.RenewalSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.ReservationAutomaticRenewal
import qualified Amazonka.Prelude as Prelude

-- | The Renewal settings for Reservations
--
-- /See:/ 'newRenewalSettings' smart constructor.
data RenewalSettings = RenewalSettings'
  { -- | Automatic renewal status for the reservation
    automaticRenewal :: Prelude.Maybe ReservationAutomaticRenewal,
    -- | Count for the reservation renewal
    renewalCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RenewalSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticRenewal', 'renewalSettings_automaticRenewal' - Automatic renewal status for the reservation
--
-- 'renewalCount', 'renewalSettings_renewalCount' - Count for the reservation renewal
newRenewalSettings ::
  RenewalSettings
newRenewalSettings =
  RenewalSettings'
    { automaticRenewal =
        Prelude.Nothing,
      renewalCount = Prelude.Nothing
    }

-- | Automatic renewal status for the reservation
renewalSettings_automaticRenewal :: Lens.Lens' RenewalSettings (Prelude.Maybe ReservationAutomaticRenewal)
renewalSettings_automaticRenewal = Lens.lens (\RenewalSettings' {automaticRenewal} -> automaticRenewal) (\s@RenewalSettings' {} a -> s {automaticRenewal = a} :: RenewalSettings)

-- | Count for the reservation renewal
renewalSettings_renewalCount :: Lens.Lens' RenewalSettings (Prelude.Maybe Prelude.Natural)
renewalSettings_renewalCount = Lens.lens (\RenewalSettings' {renewalCount} -> renewalCount) (\s@RenewalSettings' {} a -> s {renewalCount = a} :: RenewalSettings)

instance Data.FromJSON RenewalSettings where
  parseJSON =
    Data.withObject
      "RenewalSettings"
      ( \x ->
          RenewalSettings'
            Prelude.<$> (x Data..:? "automaticRenewal")
            Prelude.<*> (x Data..:? "renewalCount")
      )

instance Prelude.Hashable RenewalSettings where
  hashWithSalt _salt RenewalSettings' {..} =
    _salt
      `Prelude.hashWithSalt` automaticRenewal
      `Prelude.hashWithSalt` renewalCount

instance Prelude.NFData RenewalSettings where
  rnf RenewalSettings' {..} =
    Prelude.rnf automaticRenewal `Prelude.seq`
      Prelude.rnf renewalCount

instance Data.ToJSON RenewalSettings where
  toJSON RenewalSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("automaticRenewal" Data..=)
              Prelude.<$> automaticRenewal,
            ("renewalCount" Data..=) Prelude.<$> renewalCount
          ]
      )
