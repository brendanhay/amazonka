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
-- Module      : Amazonka.PrivateNetworks.Types.TrackingInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.TrackingInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about tracking a shipment.
--
-- /See:/ 'newTrackingInformation' smart constructor.
data TrackingInformation = TrackingInformation'
  { -- | The tracking number of the shipment.
    trackingNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrackingInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trackingNumber', 'trackingInformation_trackingNumber' - The tracking number of the shipment.
newTrackingInformation ::
  TrackingInformation
newTrackingInformation =
  TrackingInformation'
    { trackingNumber =
        Prelude.Nothing
    }

-- | The tracking number of the shipment.
trackingInformation_trackingNumber :: Lens.Lens' TrackingInformation (Prelude.Maybe Prelude.Text)
trackingInformation_trackingNumber = Lens.lens (\TrackingInformation' {trackingNumber} -> trackingNumber) (\s@TrackingInformation' {} a -> s {trackingNumber = a} :: TrackingInformation)

instance Data.FromJSON TrackingInformation where
  parseJSON =
    Data.withObject
      "TrackingInformation"
      ( \x ->
          TrackingInformation'
            Prelude.<$> (x Data..:? "trackingNumber")
      )

instance Prelude.Hashable TrackingInformation where
  hashWithSalt _salt TrackingInformation' {..} =
    _salt `Prelude.hashWithSalt` trackingNumber

instance Prelude.NFData TrackingInformation where
  rnf TrackingInformation' {..} =
    Prelude.rnf trackingNumber
