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
-- Module      : Amazonka.SESV2.Types.IspPlacement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.IspPlacement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.PlacementStatistics

-- | An object that describes how email sent during the predictive inbox
-- placement test was handled by a certain email provider.
--
-- /See:/ 'newIspPlacement' smart constructor.
data IspPlacement = IspPlacement'
  { -- | The name of the email provider that the inbox placement data applies to.
    ispName :: Prelude.Maybe Prelude.Text,
    -- | An object that contains inbox placement metrics for a specific email
    -- provider.
    placementStatistics :: Prelude.Maybe PlacementStatistics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IspPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ispName', 'ispPlacement_ispName' - The name of the email provider that the inbox placement data applies to.
--
-- 'placementStatistics', 'ispPlacement_placementStatistics' - An object that contains inbox placement metrics for a specific email
-- provider.
newIspPlacement ::
  IspPlacement
newIspPlacement =
  IspPlacement'
    { ispName = Prelude.Nothing,
      placementStatistics = Prelude.Nothing
    }

-- | The name of the email provider that the inbox placement data applies to.
ispPlacement_ispName :: Lens.Lens' IspPlacement (Prelude.Maybe Prelude.Text)
ispPlacement_ispName = Lens.lens (\IspPlacement' {ispName} -> ispName) (\s@IspPlacement' {} a -> s {ispName = a} :: IspPlacement)

-- | An object that contains inbox placement metrics for a specific email
-- provider.
ispPlacement_placementStatistics :: Lens.Lens' IspPlacement (Prelude.Maybe PlacementStatistics)
ispPlacement_placementStatistics = Lens.lens (\IspPlacement' {placementStatistics} -> placementStatistics) (\s@IspPlacement' {} a -> s {placementStatistics = a} :: IspPlacement)

instance Data.FromJSON IspPlacement where
  parseJSON =
    Data.withObject
      "IspPlacement"
      ( \x ->
          IspPlacement'
            Prelude.<$> (x Data..:? "IspName")
            Prelude.<*> (x Data..:? "PlacementStatistics")
      )

instance Prelude.Hashable IspPlacement where
  hashWithSalt _salt IspPlacement' {..} =
    _salt
      `Prelude.hashWithSalt` ispName
      `Prelude.hashWithSalt` placementStatistics

instance Prelude.NFData IspPlacement where
  rnf IspPlacement' {..} =
    Prelude.rnf ispName
      `Prelude.seq` Prelude.rnf placementStatistics
