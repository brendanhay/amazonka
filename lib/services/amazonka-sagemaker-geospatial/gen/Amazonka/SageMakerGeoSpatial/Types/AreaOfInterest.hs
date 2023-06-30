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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.AreaOfInterest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.AreaOfInterest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.AreaOfInterestGeometry

-- |
--
-- /See:/ 'newAreaOfInterest' smart constructor.
data AreaOfInterest = AreaOfInterest'
  { areaOfInterestGeometry :: Prelude.Maybe AreaOfInterestGeometry
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AreaOfInterest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'areaOfInterestGeometry', 'areaOfInterest_areaOfInterestGeometry' -
newAreaOfInterest ::
  AreaOfInterest
newAreaOfInterest =
  AreaOfInterest'
    { areaOfInterestGeometry =
        Prelude.Nothing
    }

areaOfInterest_areaOfInterestGeometry :: Lens.Lens' AreaOfInterest (Prelude.Maybe AreaOfInterestGeometry)
areaOfInterest_areaOfInterestGeometry = Lens.lens (\AreaOfInterest' {areaOfInterestGeometry} -> areaOfInterestGeometry) (\s@AreaOfInterest' {} a -> s {areaOfInterestGeometry = a} :: AreaOfInterest)

instance Data.FromJSON AreaOfInterest where
  parseJSON =
    Data.withObject
      "AreaOfInterest"
      ( \x ->
          AreaOfInterest'
            Prelude.<$> (x Data..:? "AreaOfInterestGeometry")
      )

instance Prelude.Hashable AreaOfInterest where
  hashWithSalt _salt AreaOfInterest' {..} =
    _salt `Prelude.hashWithSalt` areaOfInterestGeometry

instance Prelude.NFData AreaOfInterest where
  rnf AreaOfInterest' {..} =
    Prelude.rnf areaOfInterestGeometry

instance Data.ToJSON AreaOfInterest where
  toJSON AreaOfInterest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AreaOfInterestGeometry" Data..=)
              Prelude.<$> areaOfInterestGeometry
          ]
      )
