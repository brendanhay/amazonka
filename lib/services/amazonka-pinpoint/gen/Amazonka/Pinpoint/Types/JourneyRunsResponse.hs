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
-- Module      : Amazonka.Pinpoint.Types.JourneyRunsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyRunsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.JourneyRunResponse
import qualified Amazonka.Prelude as Prelude

-- | Provides information from all runs of a journey.
--
-- /See:/ 'newJourneyRunsResponse' smart constructor.
data JourneyRunsResponse = JourneyRunsResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of responses, one for each run of the journey
    item :: [JourneyRunResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneyRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'journeyRunsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'item', 'journeyRunsResponse_item' - An array of responses, one for each run of the journey
newJourneyRunsResponse ::
  JourneyRunsResponse
newJourneyRunsResponse =
  JourneyRunsResponse'
    { nextToken = Prelude.Nothing,
      item = Prelude.mempty
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
journeyRunsResponse_nextToken :: Lens.Lens' JourneyRunsResponse (Prelude.Maybe Prelude.Text)
journeyRunsResponse_nextToken = Lens.lens (\JourneyRunsResponse' {nextToken} -> nextToken) (\s@JourneyRunsResponse' {} a -> s {nextToken = a} :: JourneyRunsResponse)

-- | An array of responses, one for each run of the journey
journeyRunsResponse_item :: Lens.Lens' JourneyRunsResponse [JourneyRunResponse]
journeyRunsResponse_item = Lens.lens (\JourneyRunsResponse' {item} -> item) (\s@JourneyRunsResponse' {} a -> s {item = a} :: JourneyRunsResponse) Prelude.. Lens.coerced

instance Data.FromJSON JourneyRunsResponse where
  parseJSON =
    Data.withObject
      "JourneyRunsResponse"
      ( \x ->
          JourneyRunsResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> (x Data..:? "Item" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable JourneyRunsResponse where
  hashWithSalt _salt JourneyRunsResponse' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` item

instance Prelude.NFData JourneyRunsResponse where
  rnf JourneyRunsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf item
