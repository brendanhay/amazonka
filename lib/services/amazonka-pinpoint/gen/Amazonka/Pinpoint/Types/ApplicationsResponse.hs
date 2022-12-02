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
-- Module      : Amazonka.Pinpoint.Types.ApplicationsResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ApplicationsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ApplicationResponse
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
applicationsResponse_item = Lens.lens (\ApplicationsResponse' {item} -> item) (\s@ApplicationsResponse' {} a -> s {item = a} :: ApplicationsResponse) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ApplicationsResponse where
  parseJSON =
    Data.withObject
      "ApplicationsResponse"
      ( \x ->
          ApplicationsResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> (x Data..:? "Item" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ApplicationsResponse where
  hashWithSalt _salt ApplicationsResponse' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` item

instance Prelude.NFData ApplicationsResponse where
  rnf ApplicationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf item
