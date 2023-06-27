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
-- Module      : Amazonka.Rekognition.Types.GetContentModerationRequestMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.GetContentModerationRequestMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.ContentModerationAggregateBy
import Amazonka.Rekognition.Types.ContentModerationSortBy

-- | Contains metadata about a content moderation request, including the
-- SortBy and AggregateBy options.
--
-- /See:/ 'newGetContentModerationRequestMetadata' smart constructor.
data GetContentModerationRequestMetadata = GetContentModerationRequestMetadata'
  { -- | The aggregation method chosen for a GetContentModeration request.
    aggregateBy :: Prelude.Maybe ContentModerationAggregateBy,
    -- | The sorting method chosen for a GetContentModeration request.
    sortBy :: Prelude.Maybe ContentModerationSortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContentModerationRequestMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregateBy', 'getContentModerationRequestMetadata_aggregateBy' - The aggregation method chosen for a GetContentModeration request.
--
-- 'sortBy', 'getContentModerationRequestMetadata_sortBy' - The sorting method chosen for a GetContentModeration request.
newGetContentModerationRequestMetadata ::
  GetContentModerationRequestMetadata
newGetContentModerationRequestMetadata =
  GetContentModerationRequestMetadata'
    { aggregateBy =
        Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | The aggregation method chosen for a GetContentModeration request.
getContentModerationRequestMetadata_aggregateBy :: Lens.Lens' GetContentModerationRequestMetadata (Prelude.Maybe ContentModerationAggregateBy)
getContentModerationRequestMetadata_aggregateBy = Lens.lens (\GetContentModerationRequestMetadata' {aggregateBy} -> aggregateBy) (\s@GetContentModerationRequestMetadata' {} a -> s {aggregateBy = a} :: GetContentModerationRequestMetadata)

-- | The sorting method chosen for a GetContentModeration request.
getContentModerationRequestMetadata_sortBy :: Lens.Lens' GetContentModerationRequestMetadata (Prelude.Maybe ContentModerationSortBy)
getContentModerationRequestMetadata_sortBy = Lens.lens (\GetContentModerationRequestMetadata' {sortBy} -> sortBy) (\s@GetContentModerationRequestMetadata' {} a -> s {sortBy = a} :: GetContentModerationRequestMetadata)

instance
  Data.FromJSON
    GetContentModerationRequestMetadata
  where
  parseJSON =
    Data.withObject
      "GetContentModerationRequestMetadata"
      ( \x ->
          GetContentModerationRequestMetadata'
            Prelude.<$> (x Data..:? "AggregateBy")
            Prelude.<*> (x Data..:? "SortBy")
      )

instance
  Prelude.Hashable
    GetContentModerationRequestMetadata
  where
  hashWithSalt
    _salt
    GetContentModerationRequestMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` aggregateBy
        `Prelude.hashWithSalt` sortBy

instance
  Prelude.NFData
    GetContentModerationRequestMetadata
  where
  rnf GetContentModerationRequestMetadata' {..} =
    Prelude.rnf aggregateBy
      `Prelude.seq` Prelude.rnf sortBy
