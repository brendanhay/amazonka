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
-- Module      : Amazonka.Pinpoint.Types.SegmentsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SegmentsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.SegmentResponse
import qualified Amazonka.Prelude as Prelude

-- | Provides information about all the segments that are associated with an
-- application.
--
-- /See:/ 'newSegmentsResponse' smart constructor.
data SegmentsResponse = SegmentsResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of responses, one for each segment that\'s associated with the
    -- application (Segments resource) or each version of a segment that\'s
    -- associated with the application (Segment Versions resource).
    item :: [SegmentResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SegmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'segmentsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'item', 'segmentsResponse_item' - An array of responses, one for each segment that\'s associated with the
-- application (Segments resource) or each version of a segment that\'s
-- associated with the application (Segment Versions resource).
newSegmentsResponse ::
  SegmentsResponse
newSegmentsResponse =
  SegmentsResponse'
    { nextToken = Prelude.Nothing,
      item = Prelude.mempty
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
segmentsResponse_nextToken :: Lens.Lens' SegmentsResponse (Prelude.Maybe Prelude.Text)
segmentsResponse_nextToken = Lens.lens (\SegmentsResponse' {nextToken} -> nextToken) (\s@SegmentsResponse' {} a -> s {nextToken = a} :: SegmentsResponse)

-- | An array of responses, one for each segment that\'s associated with the
-- application (Segments resource) or each version of a segment that\'s
-- associated with the application (Segment Versions resource).
segmentsResponse_item :: Lens.Lens' SegmentsResponse [SegmentResponse]
segmentsResponse_item = Lens.lens (\SegmentsResponse' {item} -> item) (\s@SegmentsResponse' {} a -> s {item = a} :: SegmentsResponse) Prelude.. Lens.coerced

instance Data.FromJSON SegmentsResponse where
  parseJSON =
    Data.withObject
      "SegmentsResponse"
      ( \x ->
          SegmentsResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> (x Data..:? "Item" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SegmentsResponse where
  hashWithSalt _salt SegmentsResponse' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` item

instance Prelude.NFData SegmentsResponse where
  rnf SegmentsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf item
