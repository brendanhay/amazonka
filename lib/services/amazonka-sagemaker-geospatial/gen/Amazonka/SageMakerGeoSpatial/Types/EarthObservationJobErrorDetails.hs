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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobErrorDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobErrorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobErrorType

-- | The structure representing the errors in an EarthObservationJob.
--
-- /See:/ 'newEarthObservationJobErrorDetails' smart constructor.
data EarthObservationJobErrorDetails = EarthObservationJobErrorDetails'
  { message :: Prelude.Maybe Prelude.Text,
    type' :: Prelude.Maybe EarthObservationJobErrorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EarthObservationJobErrorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'earthObservationJobErrorDetails_message' -
--
-- 'type'', 'earthObservationJobErrorDetails_type' -
newEarthObservationJobErrorDetails ::
  EarthObservationJobErrorDetails
newEarthObservationJobErrorDetails =
  EarthObservationJobErrorDetails'
    { message =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- |
earthObservationJobErrorDetails_message :: Lens.Lens' EarthObservationJobErrorDetails (Prelude.Maybe Prelude.Text)
earthObservationJobErrorDetails_message = Lens.lens (\EarthObservationJobErrorDetails' {message} -> message) (\s@EarthObservationJobErrorDetails' {} a -> s {message = a} :: EarthObservationJobErrorDetails)

-- |
earthObservationJobErrorDetails_type :: Lens.Lens' EarthObservationJobErrorDetails (Prelude.Maybe EarthObservationJobErrorType)
earthObservationJobErrorDetails_type = Lens.lens (\EarthObservationJobErrorDetails' {type'} -> type') (\s@EarthObservationJobErrorDetails' {} a -> s {type' = a} :: EarthObservationJobErrorDetails)

instance
  Data.FromJSON
    EarthObservationJobErrorDetails
  where
  parseJSON =
    Data.withObject
      "EarthObservationJobErrorDetails"
      ( \x ->
          EarthObservationJobErrorDetails'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    EarthObservationJobErrorDetails
  where
  hashWithSalt
    _salt
    EarthObservationJobErrorDetails' {..} =
      _salt `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    EarthObservationJobErrorDetails
  where
  rnf EarthObservationJobErrorDetails' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf type'
