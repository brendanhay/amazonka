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
-- Module      : Amazonka.ConnectCampaigns.Types.CampaignFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.CampaignFilters where

import Amazonka.ConnectCampaigns.Types.InstanceIdFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filter model by type
--
-- /See:/ 'newCampaignFilters' smart constructor.
data CampaignFilters = CampaignFilters'
  { instanceIdFilter :: Prelude.Maybe InstanceIdFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CampaignFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIdFilter', 'campaignFilters_instanceIdFilter' - Undocumented member.
newCampaignFilters ::
  CampaignFilters
newCampaignFilters =
  CampaignFilters'
    { instanceIdFilter =
        Prelude.Nothing
    }

-- | Undocumented member.
campaignFilters_instanceIdFilter :: Lens.Lens' CampaignFilters (Prelude.Maybe InstanceIdFilter)
campaignFilters_instanceIdFilter = Lens.lens (\CampaignFilters' {instanceIdFilter} -> instanceIdFilter) (\s@CampaignFilters' {} a -> s {instanceIdFilter = a} :: CampaignFilters)

instance Prelude.Hashable CampaignFilters where
  hashWithSalt _salt CampaignFilters' {..} =
    _salt `Prelude.hashWithSalt` instanceIdFilter

instance Prelude.NFData CampaignFilters where
  rnf CampaignFilters' {..} =
    Prelude.rnf instanceIdFilter

instance Data.ToJSON CampaignFilters where
  toJSON CampaignFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("instanceIdFilter" Data..=)
              Prelude.<$> instanceIdFilter
          ]
      )
