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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ActivityResponse

-- | Provides information about the activities that were performed by a
-- campaign.
--
-- /See:/ 'newActivitiesResponse' smart constructor.
data ActivitiesResponse = ActivitiesResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of responses, one for each activity that was performed by the
    -- campaign.
    item :: [ActivityResponse]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      item = Core.mempty
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
activitiesResponse_nextToken :: Lens.Lens' ActivitiesResponse (Core.Maybe Core.Text)
activitiesResponse_nextToken = Lens.lens (\ActivitiesResponse' {nextToken} -> nextToken) (\s@ActivitiesResponse' {} a -> s {nextToken = a} :: ActivitiesResponse)

-- | An array of responses, one for each activity that was performed by the
-- campaign.
activitiesResponse_item :: Lens.Lens' ActivitiesResponse [ActivityResponse]
activitiesResponse_item = Lens.lens (\ActivitiesResponse' {item} -> item) (\s@ActivitiesResponse' {} a -> s {item = a} :: ActivitiesResponse) Core.. Lens._Coerce

instance Core.FromJSON ActivitiesResponse where
  parseJSON =
    Core.withObject
      "ActivitiesResponse"
      ( \x ->
          ActivitiesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Item" Core..!= Core.mempty)
      )

instance Core.Hashable ActivitiesResponse

instance Core.NFData ActivitiesResponse
