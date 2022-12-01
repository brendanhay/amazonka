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
-- Module      : Amazonka.SecurityHub.Types.SensitiveDataResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.SensitiveDataResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.SensitiveDataDetections

-- | Contains a detected instance of sensitive data that are based on
-- built-in identifiers.
--
-- /See:/ 'newSensitiveDataResult' smart constructor.
data SensitiveDataResult = SensitiveDataResult'
  { -- | The list of detected instances of sensitive data.
    detections :: Prelude.Maybe [SensitiveDataDetections],
    -- | The category of sensitive data that was detected. For example, the
    -- category can indicate that the sensitive data involved credentials,
    -- financial information, or personal information.
    category :: Prelude.Maybe Prelude.Text,
    -- | The total number of occurrences of sensitive data.
    totalCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SensitiveDataResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detections', 'sensitiveDataResult_detections' - The list of detected instances of sensitive data.
--
-- 'category', 'sensitiveDataResult_category' - The category of sensitive data that was detected. For example, the
-- category can indicate that the sensitive data involved credentials,
-- financial information, or personal information.
--
-- 'totalCount', 'sensitiveDataResult_totalCount' - The total number of occurrences of sensitive data.
newSensitiveDataResult ::
  SensitiveDataResult
newSensitiveDataResult =
  SensitiveDataResult'
    { detections = Prelude.Nothing,
      category = Prelude.Nothing,
      totalCount = Prelude.Nothing
    }

-- | The list of detected instances of sensitive data.
sensitiveDataResult_detections :: Lens.Lens' SensitiveDataResult (Prelude.Maybe [SensitiveDataDetections])
sensitiveDataResult_detections = Lens.lens (\SensitiveDataResult' {detections} -> detections) (\s@SensitiveDataResult' {} a -> s {detections = a} :: SensitiveDataResult) Prelude.. Lens.mapping Lens.coerced

-- | The category of sensitive data that was detected. For example, the
-- category can indicate that the sensitive data involved credentials,
-- financial information, or personal information.
sensitiveDataResult_category :: Lens.Lens' SensitiveDataResult (Prelude.Maybe Prelude.Text)
sensitiveDataResult_category = Lens.lens (\SensitiveDataResult' {category} -> category) (\s@SensitiveDataResult' {} a -> s {category = a} :: SensitiveDataResult)

-- | The total number of occurrences of sensitive data.
sensitiveDataResult_totalCount :: Lens.Lens' SensitiveDataResult (Prelude.Maybe Prelude.Integer)
sensitiveDataResult_totalCount = Lens.lens (\SensitiveDataResult' {totalCount} -> totalCount) (\s@SensitiveDataResult' {} a -> s {totalCount = a} :: SensitiveDataResult)

instance Core.FromJSON SensitiveDataResult where
  parseJSON =
    Core.withObject
      "SensitiveDataResult"
      ( \x ->
          SensitiveDataResult'
            Prelude.<$> (x Core..:? "Detections" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Category")
            Prelude.<*> (x Core..:? "TotalCount")
      )

instance Prelude.Hashable SensitiveDataResult where
  hashWithSalt _salt SensitiveDataResult' {..} =
    _salt `Prelude.hashWithSalt` detections
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` totalCount

instance Prelude.NFData SensitiveDataResult where
  rnf SensitiveDataResult' {..} =
    Prelude.rnf detections
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf totalCount

instance Core.ToJSON SensitiveDataResult where
  toJSON SensitiveDataResult' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Detections" Core..=) Prelude.<$> detections,
            ("Category" Core..=) Prelude.<$> category,
            ("TotalCount" Core..=) Prelude.<$> totalCount
          ]
      )
