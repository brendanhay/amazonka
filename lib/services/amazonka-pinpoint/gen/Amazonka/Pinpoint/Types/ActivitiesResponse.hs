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
-- Module      : Amazonka.Pinpoint.Types.ActivitiesResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ActivitiesResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ActivityResponse
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the activities that were performed by a
-- campaign.
--
-- /See:/ 'newActivitiesResponse' smart constructor.
data ActivitiesResponse = ActivitiesResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of responses, one for each activity that was performed by the
    -- campaign.
    item :: [ActivityResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'activitiesResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'item', 'activitiesResponse_item' - An array of responses, one for each activity that was performed by the
-- campaign.
newActivitiesResponse ::
  ActivitiesResponse
newActivitiesResponse =
  ActivitiesResponse'
    { nextToken = Prelude.Nothing,
      item = Prelude.mempty
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
activitiesResponse_nextToken :: Lens.Lens' ActivitiesResponse (Prelude.Maybe Prelude.Text)
activitiesResponse_nextToken = Lens.lens (\ActivitiesResponse' {nextToken} -> nextToken) (\s@ActivitiesResponse' {} a -> s {nextToken = a} :: ActivitiesResponse)

-- | An array of responses, one for each activity that was performed by the
-- campaign.
activitiesResponse_item :: Lens.Lens' ActivitiesResponse [ActivityResponse]
activitiesResponse_item = Lens.lens (\ActivitiesResponse' {item} -> item) (\s@ActivitiesResponse' {} a -> s {item = a} :: ActivitiesResponse) Prelude.. Lens.coerced

instance Data.FromJSON ActivitiesResponse where
  parseJSON =
    Data.withObject
      "ActivitiesResponse"
      ( \x ->
          ActivitiesResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> (x Data..:? "Item" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ActivitiesResponse where
  hashWithSalt _salt ActivitiesResponse' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` item

instance Prelude.NFData ActivitiesResponse where
  rnf ActivitiesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf item
