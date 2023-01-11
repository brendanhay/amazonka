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
-- Module      : Amazonka.SecurityHub.Types.CustomDataIdentifiersResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.CustomDataIdentifiersResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.CustomDataIdentifiersDetections

-- | Contains an instance of sensitive data that was detected by a
-- customer-defined identifier.
--
-- /See:/ 'newCustomDataIdentifiersResult' smart constructor.
data CustomDataIdentifiersResult = CustomDataIdentifiersResult'
  { -- | The list of detected instances of sensitive data.
    detections :: Prelude.Maybe [CustomDataIdentifiersDetections],
    -- | The total number of occurrences of sensitive data.
    totalCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomDataIdentifiersResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detections', 'customDataIdentifiersResult_detections' - The list of detected instances of sensitive data.
--
-- 'totalCount', 'customDataIdentifiersResult_totalCount' - The total number of occurrences of sensitive data.
newCustomDataIdentifiersResult ::
  CustomDataIdentifiersResult
newCustomDataIdentifiersResult =
  CustomDataIdentifiersResult'
    { detections =
        Prelude.Nothing,
      totalCount = Prelude.Nothing
    }

-- | The list of detected instances of sensitive data.
customDataIdentifiersResult_detections :: Lens.Lens' CustomDataIdentifiersResult (Prelude.Maybe [CustomDataIdentifiersDetections])
customDataIdentifiersResult_detections = Lens.lens (\CustomDataIdentifiersResult' {detections} -> detections) (\s@CustomDataIdentifiersResult' {} a -> s {detections = a} :: CustomDataIdentifiersResult) Prelude.. Lens.mapping Lens.coerced

-- | The total number of occurrences of sensitive data.
customDataIdentifiersResult_totalCount :: Lens.Lens' CustomDataIdentifiersResult (Prelude.Maybe Prelude.Integer)
customDataIdentifiersResult_totalCount = Lens.lens (\CustomDataIdentifiersResult' {totalCount} -> totalCount) (\s@CustomDataIdentifiersResult' {} a -> s {totalCount = a} :: CustomDataIdentifiersResult)

instance Data.FromJSON CustomDataIdentifiersResult where
  parseJSON =
    Data.withObject
      "CustomDataIdentifiersResult"
      ( \x ->
          CustomDataIdentifiersResult'
            Prelude.<$> (x Data..:? "Detections" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TotalCount")
      )

instance Prelude.Hashable CustomDataIdentifiersResult where
  hashWithSalt _salt CustomDataIdentifiersResult' {..} =
    _salt `Prelude.hashWithSalt` detections
      `Prelude.hashWithSalt` totalCount

instance Prelude.NFData CustomDataIdentifiersResult where
  rnf CustomDataIdentifiersResult' {..} =
    Prelude.rnf detections
      `Prelude.seq` Prelude.rnf totalCount

instance Data.ToJSON CustomDataIdentifiersResult where
  toJSON CustomDataIdentifiersResult' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Detections" Data..=) Prelude.<$> detections,
            ("TotalCount" Data..=) Prelude.<$> totalCount
          ]
      )
