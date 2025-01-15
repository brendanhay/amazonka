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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.LandsatCloudCoverLandInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.LandsatCloudCoverLandInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newLandsatCloudCoverLandInput' smart constructor.
data LandsatCloudCoverLandInput = LandsatCloudCoverLandInput'
  { lowerBound :: Prelude.Double,
    upperBound :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LandsatCloudCoverLandInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lowerBound', 'landsatCloudCoverLandInput_lowerBound' -
--
-- 'upperBound', 'landsatCloudCoverLandInput_upperBound' -
newLandsatCloudCoverLandInput ::
  -- | 'lowerBound'
  Prelude.Double ->
  -- | 'upperBound'
  Prelude.Double ->
  LandsatCloudCoverLandInput
newLandsatCloudCoverLandInput
  pLowerBound_
  pUpperBound_ =
    LandsatCloudCoverLandInput'
      { lowerBound =
          pLowerBound_,
        upperBound = pUpperBound_
      }

landsatCloudCoverLandInput_lowerBound :: Lens.Lens' LandsatCloudCoverLandInput Prelude.Double
landsatCloudCoverLandInput_lowerBound = Lens.lens (\LandsatCloudCoverLandInput' {lowerBound} -> lowerBound) (\s@LandsatCloudCoverLandInput' {} a -> s {lowerBound = a} :: LandsatCloudCoverLandInput)

landsatCloudCoverLandInput_upperBound :: Lens.Lens' LandsatCloudCoverLandInput Prelude.Double
landsatCloudCoverLandInput_upperBound = Lens.lens (\LandsatCloudCoverLandInput' {upperBound} -> upperBound) (\s@LandsatCloudCoverLandInput' {} a -> s {upperBound = a} :: LandsatCloudCoverLandInput)

instance Data.FromJSON LandsatCloudCoverLandInput where
  parseJSON =
    Data.withObject
      "LandsatCloudCoverLandInput"
      ( \x ->
          LandsatCloudCoverLandInput'
            Prelude.<$> (x Data..: "LowerBound")
            Prelude.<*> (x Data..: "UpperBound")
      )

instance Prelude.Hashable LandsatCloudCoverLandInput where
  hashWithSalt _salt LandsatCloudCoverLandInput' {..} =
    _salt
      `Prelude.hashWithSalt` lowerBound
      `Prelude.hashWithSalt` upperBound

instance Prelude.NFData LandsatCloudCoverLandInput where
  rnf LandsatCloudCoverLandInput' {..} =
    Prelude.rnf lowerBound `Prelude.seq`
      Prelude.rnf upperBound

instance Data.ToJSON LandsatCloudCoverLandInput where
  toJSON LandsatCloudCoverLandInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("LowerBound" Data..= lowerBound),
            Prelude.Just ("UpperBound" Data..= upperBound)
          ]
      )
