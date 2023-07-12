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
-- Module      : Amazonka.SageMaker.Types.SubscribedWorkteam
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SubscribedWorkteam where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a work team of a vendor that does the a labelling job.
--
-- /See:/ 'newSubscribedWorkteam' smart constructor.
data SubscribedWorkteam = SubscribedWorkteam'
  { -- | Marketplace product listing ID.
    listingId :: Prelude.Maybe Prelude.Text,
    -- | The description of the vendor from the Amazon Marketplace.
    marketplaceDescription :: Prelude.Maybe Prelude.Text,
    -- | The title of the service provided by the vendor in the Amazon
    -- Marketplace.
    marketplaceTitle :: Prelude.Maybe Prelude.Text,
    -- | The name of the vendor in the Amazon Marketplace.
    sellerName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the vendor that you have subscribed.
    workteamArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubscribedWorkteam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listingId', 'subscribedWorkteam_listingId' - Marketplace product listing ID.
--
-- 'marketplaceDescription', 'subscribedWorkteam_marketplaceDescription' - The description of the vendor from the Amazon Marketplace.
--
-- 'marketplaceTitle', 'subscribedWorkteam_marketplaceTitle' - The title of the service provided by the vendor in the Amazon
-- Marketplace.
--
-- 'sellerName', 'subscribedWorkteam_sellerName' - The name of the vendor in the Amazon Marketplace.
--
-- 'workteamArn', 'subscribedWorkteam_workteamArn' - The Amazon Resource Name (ARN) of the vendor that you have subscribed.
newSubscribedWorkteam ::
  -- | 'workteamArn'
  Prelude.Text ->
  SubscribedWorkteam
newSubscribedWorkteam pWorkteamArn_ =
  SubscribedWorkteam'
    { listingId = Prelude.Nothing,
      marketplaceDescription = Prelude.Nothing,
      marketplaceTitle = Prelude.Nothing,
      sellerName = Prelude.Nothing,
      workteamArn = pWorkteamArn_
    }

-- | Marketplace product listing ID.
subscribedWorkteam_listingId :: Lens.Lens' SubscribedWorkteam (Prelude.Maybe Prelude.Text)
subscribedWorkteam_listingId = Lens.lens (\SubscribedWorkteam' {listingId} -> listingId) (\s@SubscribedWorkteam' {} a -> s {listingId = a} :: SubscribedWorkteam)

-- | The description of the vendor from the Amazon Marketplace.
subscribedWorkteam_marketplaceDescription :: Lens.Lens' SubscribedWorkteam (Prelude.Maybe Prelude.Text)
subscribedWorkteam_marketplaceDescription = Lens.lens (\SubscribedWorkteam' {marketplaceDescription} -> marketplaceDescription) (\s@SubscribedWorkteam' {} a -> s {marketplaceDescription = a} :: SubscribedWorkteam)

-- | The title of the service provided by the vendor in the Amazon
-- Marketplace.
subscribedWorkteam_marketplaceTitle :: Lens.Lens' SubscribedWorkteam (Prelude.Maybe Prelude.Text)
subscribedWorkteam_marketplaceTitle = Lens.lens (\SubscribedWorkteam' {marketplaceTitle} -> marketplaceTitle) (\s@SubscribedWorkteam' {} a -> s {marketplaceTitle = a} :: SubscribedWorkteam)

-- | The name of the vendor in the Amazon Marketplace.
subscribedWorkteam_sellerName :: Lens.Lens' SubscribedWorkteam (Prelude.Maybe Prelude.Text)
subscribedWorkteam_sellerName = Lens.lens (\SubscribedWorkteam' {sellerName} -> sellerName) (\s@SubscribedWorkteam' {} a -> s {sellerName = a} :: SubscribedWorkteam)

-- | The Amazon Resource Name (ARN) of the vendor that you have subscribed.
subscribedWorkteam_workteamArn :: Lens.Lens' SubscribedWorkteam Prelude.Text
subscribedWorkteam_workteamArn = Lens.lens (\SubscribedWorkteam' {workteamArn} -> workteamArn) (\s@SubscribedWorkteam' {} a -> s {workteamArn = a} :: SubscribedWorkteam)

instance Data.FromJSON SubscribedWorkteam where
  parseJSON =
    Data.withObject
      "SubscribedWorkteam"
      ( \x ->
          SubscribedWorkteam'
            Prelude.<$> (x Data..:? "ListingId")
            Prelude.<*> (x Data..:? "MarketplaceDescription")
            Prelude.<*> (x Data..:? "MarketplaceTitle")
            Prelude.<*> (x Data..:? "SellerName")
            Prelude.<*> (x Data..: "WorkteamArn")
      )

instance Prelude.Hashable SubscribedWorkteam where
  hashWithSalt _salt SubscribedWorkteam' {..} =
    _salt
      `Prelude.hashWithSalt` listingId
      `Prelude.hashWithSalt` marketplaceDescription
      `Prelude.hashWithSalt` marketplaceTitle
      `Prelude.hashWithSalt` sellerName
      `Prelude.hashWithSalt` workteamArn

instance Prelude.NFData SubscribedWorkteam where
  rnf SubscribedWorkteam' {..} =
    Prelude.rnf listingId
      `Prelude.seq` Prelude.rnf marketplaceDescription
      `Prelude.seq` Prelude.rnf marketplaceTitle
      `Prelude.seq` Prelude.rnf sellerName
      `Prelude.seq` Prelude.rnf workteamArn
