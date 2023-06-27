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
-- Module      : Amazonka.Rekognition.Types.GetLabelDetectionRequestMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.GetLabelDetectionRequestMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.LabelDetectionAggregateBy
import Amazonka.Rekognition.Types.LabelDetectionSortBy

-- | Contains metadata about a label detection request, including the SortBy
-- and AggregateBy options.
--
-- /See:/ 'newGetLabelDetectionRequestMetadata' smart constructor.
data GetLabelDetectionRequestMetadata = GetLabelDetectionRequestMetadata'
  { -- | The aggregation method chosen for a GetLabelDetection request.
    aggregateBy :: Prelude.Maybe LabelDetectionAggregateBy,
    -- | The sorting method chosen for a GetLabelDetection request.
    sortBy :: Prelude.Maybe LabelDetectionSortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLabelDetectionRequestMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregateBy', 'getLabelDetectionRequestMetadata_aggregateBy' - The aggregation method chosen for a GetLabelDetection request.
--
-- 'sortBy', 'getLabelDetectionRequestMetadata_sortBy' - The sorting method chosen for a GetLabelDetection request.
newGetLabelDetectionRequestMetadata ::
  GetLabelDetectionRequestMetadata
newGetLabelDetectionRequestMetadata =
  GetLabelDetectionRequestMetadata'
    { aggregateBy =
        Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | The aggregation method chosen for a GetLabelDetection request.
getLabelDetectionRequestMetadata_aggregateBy :: Lens.Lens' GetLabelDetectionRequestMetadata (Prelude.Maybe LabelDetectionAggregateBy)
getLabelDetectionRequestMetadata_aggregateBy = Lens.lens (\GetLabelDetectionRequestMetadata' {aggregateBy} -> aggregateBy) (\s@GetLabelDetectionRequestMetadata' {} a -> s {aggregateBy = a} :: GetLabelDetectionRequestMetadata)

-- | The sorting method chosen for a GetLabelDetection request.
getLabelDetectionRequestMetadata_sortBy :: Lens.Lens' GetLabelDetectionRequestMetadata (Prelude.Maybe LabelDetectionSortBy)
getLabelDetectionRequestMetadata_sortBy = Lens.lens (\GetLabelDetectionRequestMetadata' {sortBy} -> sortBy) (\s@GetLabelDetectionRequestMetadata' {} a -> s {sortBy = a} :: GetLabelDetectionRequestMetadata)

instance
  Data.FromJSON
    GetLabelDetectionRequestMetadata
  where
  parseJSON =
    Data.withObject
      "GetLabelDetectionRequestMetadata"
      ( \x ->
          GetLabelDetectionRequestMetadata'
            Prelude.<$> (x Data..:? "AggregateBy")
            Prelude.<*> (x Data..:? "SortBy")
      )

instance
  Prelude.Hashable
    GetLabelDetectionRequestMetadata
  where
  hashWithSalt
    _salt
    GetLabelDetectionRequestMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` aggregateBy
        `Prelude.hashWithSalt` sortBy

instance
  Prelude.NFData
    GetLabelDetectionRequestMetadata
  where
  rnf GetLabelDetectionRequestMetadata' {..} =
    Prelude.rnf aggregateBy
      `Prelude.seq` Prelude.rnf sortBy
