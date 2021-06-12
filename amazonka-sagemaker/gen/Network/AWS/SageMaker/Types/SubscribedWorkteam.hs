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
-- Module      : Network.AWS.SageMaker.Types.SubscribedWorkteam
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SubscribedWorkteam where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a work team of a vendor that does the a labelling job.
--
-- /See:/ 'newSubscribedWorkteam' smart constructor.
data SubscribedWorkteam = SubscribedWorkteam'
  { -- | The title of the service provided by the vendor in the Amazon
    -- Marketplace.
    marketplaceTitle :: Core.Maybe Core.Text,
    -- | Marketplace product listing ID.
    listingId :: Core.Maybe Core.Text,
    -- | The description of the vendor from the Amazon Marketplace.
    marketplaceDescription :: Core.Maybe Core.Text,
    -- | The name of the vendor in the Amazon Marketplace.
    sellerName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the vendor that you have subscribed.
    workteamArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubscribedWorkteam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marketplaceTitle', 'subscribedWorkteam_marketplaceTitle' - The title of the service provided by the vendor in the Amazon
-- Marketplace.
--
-- 'listingId', 'subscribedWorkteam_listingId' - Marketplace product listing ID.
--
-- 'marketplaceDescription', 'subscribedWorkteam_marketplaceDescription' - The description of the vendor from the Amazon Marketplace.
--
-- 'sellerName', 'subscribedWorkteam_sellerName' - The name of the vendor in the Amazon Marketplace.
--
-- 'workteamArn', 'subscribedWorkteam_workteamArn' - The Amazon Resource Name (ARN) of the vendor that you have subscribed.
newSubscribedWorkteam ::
  -- | 'workteamArn'
  Core.Text ->
  SubscribedWorkteam
newSubscribedWorkteam pWorkteamArn_ =
  SubscribedWorkteam'
    { marketplaceTitle =
        Core.Nothing,
      listingId = Core.Nothing,
      marketplaceDescription = Core.Nothing,
      sellerName = Core.Nothing,
      workteamArn = pWorkteamArn_
    }

-- | The title of the service provided by the vendor in the Amazon
-- Marketplace.
subscribedWorkteam_marketplaceTitle :: Lens.Lens' SubscribedWorkteam (Core.Maybe Core.Text)
subscribedWorkteam_marketplaceTitle = Lens.lens (\SubscribedWorkteam' {marketplaceTitle} -> marketplaceTitle) (\s@SubscribedWorkteam' {} a -> s {marketplaceTitle = a} :: SubscribedWorkteam)

-- | Marketplace product listing ID.
subscribedWorkteam_listingId :: Lens.Lens' SubscribedWorkteam (Core.Maybe Core.Text)
subscribedWorkteam_listingId = Lens.lens (\SubscribedWorkteam' {listingId} -> listingId) (\s@SubscribedWorkteam' {} a -> s {listingId = a} :: SubscribedWorkteam)

-- | The description of the vendor from the Amazon Marketplace.
subscribedWorkteam_marketplaceDescription :: Lens.Lens' SubscribedWorkteam (Core.Maybe Core.Text)
subscribedWorkteam_marketplaceDescription = Lens.lens (\SubscribedWorkteam' {marketplaceDescription} -> marketplaceDescription) (\s@SubscribedWorkteam' {} a -> s {marketplaceDescription = a} :: SubscribedWorkteam)

-- | The name of the vendor in the Amazon Marketplace.
subscribedWorkteam_sellerName :: Lens.Lens' SubscribedWorkteam (Core.Maybe Core.Text)
subscribedWorkteam_sellerName = Lens.lens (\SubscribedWorkteam' {sellerName} -> sellerName) (\s@SubscribedWorkteam' {} a -> s {sellerName = a} :: SubscribedWorkteam)

-- | The Amazon Resource Name (ARN) of the vendor that you have subscribed.
subscribedWorkteam_workteamArn :: Lens.Lens' SubscribedWorkteam Core.Text
subscribedWorkteam_workteamArn = Lens.lens (\SubscribedWorkteam' {workteamArn} -> workteamArn) (\s@SubscribedWorkteam' {} a -> s {workteamArn = a} :: SubscribedWorkteam)

instance Core.FromJSON SubscribedWorkteam where
  parseJSON =
    Core.withObject
      "SubscribedWorkteam"
      ( \x ->
          SubscribedWorkteam'
            Core.<$> (x Core..:? "MarketplaceTitle")
            Core.<*> (x Core..:? "ListingId")
            Core.<*> (x Core..:? "MarketplaceDescription")
            Core.<*> (x Core..:? "SellerName")
            Core.<*> (x Core..: "WorkteamArn")
      )

instance Core.Hashable SubscribedWorkteam

instance Core.NFData SubscribedWorkteam
