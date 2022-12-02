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
-- Module      : Amazonka.ConnectContactLens.Types.PointOfInterest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectContactLens.Types.PointOfInterest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The section of the contact audio where that category rule was detected.
--
-- /See:/ 'newPointOfInterest' smart constructor.
data PointOfInterest = PointOfInterest'
  { -- | The beginning offset in milliseconds where the category rule was
    -- detected.
    beginOffsetMillis :: Prelude.Natural,
    -- | The ending offset in milliseconds where the category rule was detected.
    endOffsetMillis :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PointOfInterest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffsetMillis', 'pointOfInterest_beginOffsetMillis' - The beginning offset in milliseconds where the category rule was
-- detected.
--
-- 'endOffsetMillis', 'pointOfInterest_endOffsetMillis' - The ending offset in milliseconds where the category rule was detected.
newPointOfInterest ::
  -- | 'beginOffsetMillis'
  Prelude.Natural ->
  -- | 'endOffsetMillis'
  Prelude.Natural ->
  PointOfInterest
newPointOfInterest
  pBeginOffsetMillis_
  pEndOffsetMillis_ =
    PointOfInterest'
      { beginOffsetMillis =
          pBeginOffsetMillis_,
        endOffsetMillis = pEndOffsetMillis_
      }

-- | The beginning offset in milliseconds where the category rule was
-- detected.
pointOfInterest_beginOffsetMillis :: Lens.Lens' PointOfInterest Prelude.Natural
pointOfInterest_beginOffsetMillis = Lens.lens (\PointOfInterest' {beginOffsetMillis} -> beginOffsetMillis) (\s@PointOfInterest' {} a -> s {beginOffsetMillis = a} :: PointOfInterest)

-- | The ending offset in milliseconds where the category rule was detected.
pointOfInterest_endOffsetMillis :: Lens.Lens' PointOfInterest Prelude.Natural
pointOfInterest_endOffsetMillis = Lens.lens (\PointOfInterest' {endOffsetMillis} -> endOffsetMillis) (\s@PointOfInterest' {} a -> s {endOffsetMillis = a} :: PointOfInterest)

instance Data.FromJSON PointOfInterest where
  parseJSON =
    Data.withObject
      "PointOfInterest"
      ( \x ->
          PointOfInterest'
            Prelude.<$> (x Data..: "BeginOffsetMillis")
            Prelude.<*> (x Data..: "EndOffsetMillis")
      )

instance Prelude.Hashable PointOfInterest where
  hashWithSalt _salt PointOfInterest' {..} =
    _salt `Prelude.hashWithSalt` beginOffsetMillis
      `Prelude.hashWithSalt` endOffsetMillis

instance Prelude.NFData PointOfInterest where
  rnf PointOfInterest' {..} =
    Prelude.rnf beginOffsetMillis
      `Prelude.seq` Prelude.rnf endOffsetMillis
