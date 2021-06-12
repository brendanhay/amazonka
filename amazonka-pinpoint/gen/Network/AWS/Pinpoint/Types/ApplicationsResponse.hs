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
-- Module      : Network.AWS.Pinpoint.Types.ApplicationsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ApplicationsResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ApplicationResponse

-- | Provides information about all of your applications.
--
-- /See:/ 'newApplicationsResponse' smart constructor.
data ApplicationsResponse = ApplicationsResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of responses, one for each application that was returned.
    item :: Core.Maybe [ApplicationResponse]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'applicationsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'item', 'applicationsResponse_item' - An array of responses, one for each application that was returned.
newApplicationsResponse ::
  ApplicationsResponse
newApplicationsResponse =
  ApplicationsResponse'
    { nextToken = Core.Nothing,
      item = Core.Nothing
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
applicationsResponse_nextToken :: Lens.Lens' ApplicationsResponse (Core.Maybe Core.Text)
applicationsResponse_nextToken = Lens.lens (\ApplicationsResponse' {nextToken} -> nextToken) (\s@ApplicationsResponse' {} a -> s {nextToken = a} :: ApplicationsResponse)

-- | An array of responses, one for each application that was returned.
applicationsResponse_item :: Lens.Lens' ApplicationsResponse (Core.Maybe [ApplicationResponse])
applicationsResponse_item = Lens.lens (\ApplicationsResponse' {item} -> item) (\s@ApplicationsResponse' {} a -> s {item = a} :: ApplicationsResponse) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ApplicationsResponse where
  parseJSON =
    Core.withObject
      "ApplicationsResponse"
      ( \x ->
          ApplicationsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Item" Core..!= Core.mempty)
      )

instance Core.Hashable ApplicationsResponse

instance Core.NFData ApplicationsResponse
