{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.ActivitiesResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ActivitiesResponse where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ActivityResponse
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
activitiesResponse_item = Lens.lens (\ActivitiesResponse' {item} -> item) (\s@ActivitiesResponse' {} a -> s {item = a} :: ActivitiesResponse) Prelude.. Prelude._Coerce

instance Prelude.FromJSON ActivitiesResponse where
  parseJSON =
    Prelude.withObject
      "ActivitiesResponse"
      ( \x ->
          ActivitiesResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> (x Prelude..:? "Item" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable ActivitiesResponse

instance Prelude.NFData ActivitiesResponse
