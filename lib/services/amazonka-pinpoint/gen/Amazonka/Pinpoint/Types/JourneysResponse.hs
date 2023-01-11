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
-- Module      : Amazonka.Pinpoint.Types.JourneysResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneysResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.JourneyResponse
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status, configuration, and other settings
-- for all the journeys that are associated with an application.
--
-- /See:/ 'newJourneysResponse' smart constructor.
data JourneysResponse = JourneysResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of responses, one for each journey that\'s associated with the
    -- application.
    item :: [JourneyResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'journeysResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'item', 'journeysResponse_item' - An array of responses, one for each journey that\'s associated with the
-- application.
newJourneysResponse ::
  JourneysResponse
newJourneysResponse =
  JourneysResponse'
    { nextToken = Prelude.Nothing,
      item = Prelude.mempty
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
journeysResponse_nextToken :: Lens.Lens' JourneysResponse (Prelude.Maybe Prelude.Text)
journeysResponse_nextToken = Lens.lens (\JourneysResponse' {nextToken} -> nextToken) (\s@JourneysResponse' {} a -> s {nextToken = a} :: JourneysResponse)

-- | An array of responses, one for each journey that\'s associated with the
-- application.
journeysResponse_item :: Lens.Lens' JourneysResponse [JourneyResponse]
journeysResponse_item = Lens.lens (\JourneysResponse' {item} -> item) (\s@JourneysResponse' {} a -> s {item = a} :: JourneysResponse) Prelude.. Lens.coerced

instance Data.FromJSON JourneysResponse where
  parseJSON =
    Data.withObject
      "JourneysResponse"
      ( \x ->
          JourneysResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> (x Data..:? "Item" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable JourneysResponse where
  hashWithSalt _salt JourneysResponse' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` item

instance Prelude.NFData JourneysResponse where
  rnf JourneysResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf item
