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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.EoCloudCoverInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.EoCloudCoverInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newEoCloudCoverInput' smart constructor.
data EoCloudCoverInput = EoCloudCoverInput'
  { lowerBound :: Prelude.Double,
    upperBound :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EoCloudCoverInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lowerBound', 'eoCloudCoverInput_lowerBound' -
--
-- 'upperBound', 'eoCloudCoverInput_upperBound' -
newEoCloudCoverInput ::
  -- | 'lowerBound'
  Prelude.Double ->
  -- | 'upperBound'
  Prelude.Double ->
  EoCloudCoverInput
newEoCloudCoverInput pLowerBound_ pUpperBound_ =
  EoCloudCoverInput'
    { lowerBound = pLowerBound_,
      upperBound = pUpperBound_
    }

-- |
eoCloudCoverInput_lowerBound :: Lens.Lens' EoCloudCoverInput Prelude.Double
eoCloudCoverInput_lowerBound = Lens.lens (\EoCloudCoverInput' {lowerBound} -> lowerBound) (\s@EoCloudCoverInput' {} a -> s {lowerBound = a} :: EoCloudCoverInput)

-- |
eoCloudCoverInput_upperBound :: Lens.Lens' EoCloudCoverInput Prelude.Double
eoCloudCoverInput_upperBound = Lens.lens (\EoCloudCoverInput' {upperBound} -> upperBound) (\s@EoCloudCoverInput' {} a -> s {upperBound = a} :: EoCloudCoverInput)

instance Data.FromJSON EoCloudCoverInput where
  parseJSON =
    Data.withObject
      "EoCloudCoverInput"
      ( \x ->
          EoCloudCoverInput'
            Prelude.<$> (x Data..: "LowerBound")
            Prelude.<*> (x Data..: "UpperBound")
      )

instance Prelude.Hashable EoCloudCoverInput where
  hashWithSalt _salt EoCloudCoverInput' {..} =
    _salt `Prelude.hashWithSalt` lowerBound
      `Prelude.hashWithSalt` upperBound

instance Prelude.NFData EoCloudCoverInput where
  rnf EoCloudCoverInput' {..} =
    Prelude.rnf lowerBound
      `Prelude.seq` Prelude.rnf upperBound

instance Data.ToJSON EoCloudCoverInput where
  toJSON EoCloudCoverInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("LowerBound" Data..= lowerBound),
            Prelude.Just ("UpperBound" Data..= upperBound)
          ]
      )
