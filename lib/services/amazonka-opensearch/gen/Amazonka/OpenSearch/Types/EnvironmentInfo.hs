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
-- Module      : Amazonka.OpenSearch.Types.EnvironmentInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.EnvironmentInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.AvailabilityZoneInfo
import qualified Amazonka.Prelude as Prelude

-- | Information about the active domain environment.
--
-- /See:/ 'newEnvironmentInfo' smart constructor.
data EnvironmentInfo = EnvironmentInfo'
  { -- | A list of @AvailabilityZoneInfo@ for the domain.
    availabilityZoneInformation :: Prelude.Maybe [AvailabilityZoneInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneInformation', 'environmentInfo_availabilityZoneInformation' - A list of @AvailabilityZoneInfo@ for the domain.
newEnvironmentInfo ::
  EnvironmentInfo
newEnvironmentInfo =
  EnvironmentInfo'
    { availabilityZoneInformation =
        Prelude.Nothing
    }

-- | A list of @AvailabilityZoneInfo@ for the domain.
environmentInfo_availabilityZoneInformation :: Lens.Lens' EnvironmentInfo (Prelude.Maybe [AvailabilityZoneInfo])
environmentInfo_availabilityZoneInformation = Lens.lens (\EnvironmentInfo' {availabilityZoneInformation} -> availabilityZoneInformation) (\s@EnvironmentInfo' {} a -> s {availabilityZoneInformation = a} :: EnvironmentInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EnvironmentInfo where
  parseJSON =
    Data.withObject
      "EnvironmentInfo"
      ( \x ->
          EnvironmentInfo'
            Prelude.<$> ( x
                            Data..:? "AvailabilityZoneInformation"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EnvironmentInfo where
  hashWithSalt _salt EnvironmentInfo' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZoneInformation

instance Prelude.NFData EnvironmentInfo where
  rnf EnvironmentInfo' {..} =
    Prelude.rnf availabilityZoneInformation
