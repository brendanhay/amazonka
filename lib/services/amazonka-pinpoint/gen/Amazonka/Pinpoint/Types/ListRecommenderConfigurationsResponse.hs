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
-- Module      : Amazonka.Pinpoint.Types.ListRecommenderConfigurationsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ListRecommenderConfigurationsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.RecommenderConfigurationResponse
import qualified Amazonka.Prelude as Prelude

-- | Provides information about all the recommender model configurations that
-- are associated with your Amazon Pinpoint account.
--
-- /See:/ 'newListRecommenderConfigurationsResponse' smart constructor.
data ListRecommenderConfigurationsResponse = ListRecommenderConfigurationsResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of responses, one for each recommender model configuration
    -- that\'s associated with your Amazon Pinpoint account.
    item :: [RecommenderConfigurationResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecommenderConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecommenderConfigurationsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'item', 'listRecommenderConfigurationsResponse_item' - An array of responses, one for each recommender model configuration
-- that\'s associated with your Amazon Pinpoint account.
newListRecommenderConfigurationsResponse ::
  ListRecommenderConfigurationsResponse
newListRecommenderConfigurationsResponse =
  ListRecommenderConfigurationsResponse'
    { nextToken =
        Prelude.Nothing,
      item = Prelude.mempty
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listRecommenderConfigurationsResponse_nextToken :: Lens.Lens' ListRecommenderConfigurationsResponse (Prelude.Maybe Prelude.Text)
listRecommenderConfigurationsResponse_nextToken = Lens.lens (\ListRecommenderConfigurationsResponse' {nextToken} -> nextToken) (\s@ListRecommenderConfigurationsResponse' {} a -> s {nextToken = a} :: ListRecommenderConfigurationsResponse)

-- | An array of responses, one for each recommender model configuration
-- that\'s associated with your Amazon Pinpoint account.
listRecommenderConfigurationsResponse_item :: Lens.Lens' ListRecommenderConfigurationsResponse [RecommenderConfigurationResponse]
listRecommenderConfigurationsResponse_item = Lens.lens (\ListRecommenderConfigurationsResponse' {item} -> item) (\s@ListRecommenderConfigurationsResponse' {} a -> s {item = a} :: ListRecommenderConfigurationsResponse) Prelude.. Lens.coerced

instance
  Data.FromJSON
    ListRecommenderConfigurationsResponse
  where
  parseJSON =
    Data.withObject
      "ListRecommenderConfigurationsResponse"
      ( \x ->
          ListRecommenderConfigurationsResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> (x Data..:? "Item" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    ListRecommenderConfigurationsResponse
  where
  hashWithSalt
    _salt
    ListRecommenderConfigurationsResponse' {..} =
      _salt
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` item

instance
  Prelude.NFData
    ListRecommenderConfigurationsResponse
  where
  rnf ListRecommenderConfigurationsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf item
