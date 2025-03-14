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
-- Module      : Amazonka.Pinpoint.Types.CampaignsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CampaignsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.CampaignResponse
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the configuration and other settings for all
-- the campaigns that are associated with an application.
--
-- /See:/ 'newCampaignsResponse' smart constructor.
data CampaignsResponse = CampaignsResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of responses, one for each campaign that\'s associated with the
    -- application.
    item :: [CampaignResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CampaignsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'campaignsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'item', 'campaignsResponse_item' - An array of responses, one for each campaign that\'s associated with the
-- application.
newCampaignsResponse ::
  CampaignsResponse
newCampaignsResponse =
  CampaignsResponse'
    { nextToken = Prelude.Nothing,
      item = Prelude.mempty
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
campaignsResponse_nextToken :: Lens.Lens' CampaignsResponse (Prelude.Maybe Prelude.Text)
campaignsResponse_nextToken = Lens.lens (\CampaignsResponse' {nextToken} -> nextToken) (\s@CampaignsResponse' {} a -> s {nextToken = a} :: CampaignsResponse)

-- | An array of responses, one for each campaign that\'s associated with the
-- application.
campaignsResponse_item :: Lens.Lens' CampaignsResponse [CampaignResponse]
campaignsResponse_item = Lens.lens (\CampaignsResponse' {item} -> item) (\s@CampaignsResponse' {} a -> s {item = a} :: CampaignsResponse) Prelude.. Lens.coerced

instance Data.FromJSON CampaignsResponse where
  parseJSON =
    Data.withObject
      "CampaignsResponse"
      ( \x ->
          CampaignsResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> (x Data..:? "Item" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CampaignsResponse where
  hashWithSalt _salt CampaignsResponse' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` item

instance Prelude.NFData CampaignsResponse where
  rnf CampaignsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf item
