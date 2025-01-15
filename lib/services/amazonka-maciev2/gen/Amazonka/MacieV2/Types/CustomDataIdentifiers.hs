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
-- Module      : Amazonka.MacieV2.Types.CustomDataIdentifiers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.CustomDataIdentifiers where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.CustomDetection
import qualified Amazonka.Prelude as Prelude

-- | Provides information about custom data identifiers that produced a
-- sensitive data finding, and the number of occurrences of the data that
-- they detected for the finding.
--
-- /See:/ 'newCustomDataIdentifiers' smart constructor.
data CustomDataIdentifiers = CustomDataIdentifiers'
  { -- | The custom data identifiers that detected the data, and the number of
    -- occurrences of the data that each identifier detected.
    detections :: Prelude.Maybe [CustomDetection],
    -- | The total number of occurrences of the data that was detected by the
    -- custom data identifiers and produced the finding.
    totalCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomDataIdentifiers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detections', 'customDataIdentifiers_detections' - The custom data identifiers that detected the data, and the number of
-- occurrences of the data that each identifier detected.
--
-- 'totalCount', 'customDataIdentifiers_totalCount' - The total number of occurrences of the data that was detected by the
-- custom data identifiers and produced the finding.
newCustomDataIdentifiers ::
  CustomDataIdentifiers
newCustomDataIdentifiers =
  CustomDataIdentifiers'
    { detections =
        Prelude.Nothing,
      totalCount = Prelude.Nothing
    }

-- | The custom data identifiers that detected the data, and the number of
-- occurrences of the data that each identifier detected.
customDataIdentifiers_detections :: Lens.Lens' CustomDataIdentifiers (Prelude.Maybe [CustomDetection])
customDataIdentifiers_detections = Lens.lens (\CustomDataIdentifiers' {detections} -> detections) (\s@CustomDataIdentifiers' {} a -> s {detections = a} :: CustomDataIdentifiers) Prelude.. Lens.mapping Lens.coerced

-- | The total number of occurrences of the data that was detected by the
-- custom data identifiers and produced the finding.
customDataIdentifiers_totalCount :: Lens.Lens' CustomDataIdentifiers (Prelude.Maybe Prelude.Integer)
customDataIdentifiers_totalCount = Lens.lens (\CustomDataIdentifiers' {totalCount} -> totalCount) (\s@CustomDataIdentifiers' {} a -> s {totalCount = a} :: CustomDataIdentifiers)

instance Data.FromJSON CustomDataIdentifiers where
  parseJSON =
    Data.withObject
      "CustomDataIdentifiers"
      ( \x ->
          CustomDataIdentifiers'
            Prelude.<$> (x Data..:? "detections" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "totalCount")
      )

instance Prelude.Hashable CustomDataIdentifiers where
  hashWithSalt _salt CustomDataIdentifiers' {..} =
    _salt
      `Prelude.hashWithSalt` detections
      `Prelude.hashWithSalt` totalCount

instance Prelude.NFData CustomDataIdentifiers where
  rnf CustomDataIdentifiers' {..} =
    Prelude.rnf detections `Prelude.seq`
      Prelude.rnf totalCount
