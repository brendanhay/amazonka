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
-- Module      : Amazonka.M2.Types.HighAvailabilityConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.HighAvailabilityConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the details of a high availability configuration.
--
-- /See:/ 'newHighAvailabilityConfig' smart constructor.
data HighAvailabilityConfig = HighAvailabilityConfig'
  { -- | The number of instances in a high availability configuration.
    desiredCapacity :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HighAvailabilityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredCapacity', 'highAvailabilityConfig_desiredCapacity' - The number of instances in a high availability configuration.
newHighAvailabilityConfig ::
  -- | 'desiredCapacity'
  Prelude.Natural ->
  HighAvailabilityConfig
newHighAvailabilityConfig pDesiredCapacity_ =
  HighAvailabilityConfig'
    { desiredCapacity =
        pDesiredCapacity_
    }

-- | The number of instances in a high availability configuration.
highAvailabilityConfig_desiredCapacity :: Lens.Lens' HighAvailabilityConfig Prelude.Natural
highAvailabilityConfig_desiredCapacity = Lens.lens (\HighAvailabilityConfig' {desiredCapacity} -> desiredCapacity) (\s@HighAvailabilityConfig' {} a -> s {desiredCapacity = a} :: HighAvailabilityConfig)

instance Data.FromJSON HighAvailabilityConfig where
  parseJSON =
    Data.withObject
      "HighAvailabilityConfig"
      ( \x ->
          HighAvailabilityConfig'
            Prelude.<$> (x Data..: "desiredCapacity")
      )

instance Prelude.Hashable HighAvailabilityConfig where
  hashWithSalt _salt HighAvailabilityConfig' {..} =
    _salt `Prelude.hashWithSalt` desiredCapacity

instance Prelude.NFData HighAvailabilityConfig where
  rnf HighAvailabilityConfig' {..} =
    Prelude.rnf desiredCapacity

instance Data.ToJSON HighAvailabilityConfig where
  toJSON HighAvailabilityConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("desiredCapacity" Data..= desiredCapacity)
          ]
      )
