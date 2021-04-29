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
-- Module      : Network.AWS.Pinpoint.Types.ListRecommenderConfigurationsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ListRecommenderConfigurationsResponse where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listRecommenderConfigurationsResponse_item = Lens.lens (\ListRecommenderConfigurationsResponse' {item} -> item) (\s@ListRecommenderConfigurationsResponse' {} a -> s {item = a} :: ListRecommenderConfigurationsResponse) Prelude.. Prelude._Coerce

instance
  Prelude.FromJSON
    ListRecommenderConfigurationsResponse
  where
  parseJSON =
    Prelude.withObject
      "ListRecommenderConfigurationsResponse"
      ( \x ->
          ListRecommenderConfigurationsResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> (x Prelude..:? "Item" Prelude..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    ListRecommenderConfigurationsResponse

instance
  Prelude.NFData
    ListRecommenderConfigurationsResponse
