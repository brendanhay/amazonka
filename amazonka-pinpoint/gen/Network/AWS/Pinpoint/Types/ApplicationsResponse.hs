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
-- Module      : Network.AWS.Pinpoint.Types.ApplicationsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ApplicationsResponse where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ApplicationResponse
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about all of your applications.
--
-- /See:/ 'newApplicationsResponse' smart constructor.
data ApplicationsResponse = ApplicationsResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of responses, one for each application that was returned.
    item :: Prelude.Maybe [ApplicationResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      item = Prelude.Nothing
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
applicationsResponse_nextToken :: Lens.Lens' ApplicationsResponse (Prelude.Maybe Prelude.Text)
applicationsResponse_nextToken = Lens.lens (\ApplicationsResponse' {nextToken} -> nextToken) (\s@ApplicationsResponse' {} a -> s {nextToken = a} :: ApplicationsResponse)

-- | An array of responses, one for each application that was returned.
applicationsResponse_item :: Lens.Lens' ApplicationsResponse (Prelude.Maybe [ApplicationResponse])
applicationsResponse_item = Lens.lens (\ApplicationsResponse' {item} -> item) (\s@ApplicationsResponse' {} a -> s {item = a} :: ApplicationsResponse) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ApplicationsResponse where
  parseJSON =
    Prelude.withObject
      "ApplicationsResponse"
      ( \x ->
          ApplicationsResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> (x Prelude..:? "Item" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable ApplicationsResponse

instance Prelude.NFData ApplicationsResponse
