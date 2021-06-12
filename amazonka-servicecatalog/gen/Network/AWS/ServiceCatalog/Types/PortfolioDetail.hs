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
-- Module      : Network.AWS.ServiceCatalog.Types.PortfolioDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.PortfolioDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a portfolio.
--
-- /See:/ 'newPortfolioDetail' smart constructor.
data PortfolioDetail = PortfolioDetail'
  { -- | The name of the portfolio provider.
    providerName :: Core.Maybe Core.Text,
    -- | The portfolio identifier.
    id :: Core.Maybe Core.Text,
    -- | The ARN assigned to the portfolio.
    arn :: Core.Maybe Core.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The description of the portfolio.
    description :: Core.Maybe Core.Text,
    -- | The name to use for display purposes.
    displayName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PortfolioDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'providerName', 'portfolioDetail_providerName' - The name of the portfolio provider.
--
-- 'id', 'portfolioDetail_id' - The portfolio identifier.
--
-- 'arn', 'portfolioDetail_arn' - The ARN assigned to the portfolio.
--
-- 'createdTime', 'portfolioDetail_createdTime' - The UTC time stamp of the creation time.
--
-- 'description', 'portfolioDetail_description' - The description of the portfolio.
--
-- 'displayName', 'portfolioDetail_displayName' - The name to use for display purposes.
newPortfolioDetail ::
  PortfolioDetail
newPortfolioDetail =
  PortfolioDetail'
    { providerName = Core.Nothing,
      id = Core.Nothing,
      arn = Core.Nothing,
      createdTime = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing
    }

-- | The name of the portfolio provider.
portfolioDetail_providerName :: Lens.Lens' PortfolioDetail (Core.Maybe Core.Text)
portfolioDetail_providerName = Lens.lens (\PortfolioDetail' {providerName} -> providerName) (\s@PortfolioDetail' {} a -> s {providerName = a} :: PortfolioDetail)

-- | The portfolio identifier.
portfolioDetail_id :: Lens.Lens' PortfolioDetail (Core.Maybe Core.Text)
portfolioDetail_id = Lens.lens (\PortfolioDetail' {id} -> id) (\s@PortfolioDetail' {} a -> s {id = a} :: PortfolioDetail)

-- | The ARN assigned to the portfolio.
portfolioDetail_arn :: Lens.Lens' PortfolioDetail (Core.Maybe Core.Text)
portfolioDetail_arn = Lens.lens (\PortfolioDetail' {arn} -> arn) (\s@PortfolioDetail' {} a -> s {arn = a} :: PortfolioDetail)

-- | The UTC time stamp of the creation time.
portfolioDetail_createdTime :: Lens.Lens' PortfolioDetail (Core.Maybe Core.UTCTime)
portfolioDetail_createdTime = Lens.lens (\PortfolioDetail' {createdTime} -> createdTime) (\s@PortfolioDetail' {} a -> s {createdTime = a} :: PortfolioDetail) Core.. Lens.mapping Core._Time

-- | The description of the portfolio.
portfolioDetail_description :: Lens.Lens' PortfolioDetail (Core.Maybe Core.Text)
portfolioDetail_description = Lens.lens (\PortfolioDetail' {description} -> description) (\s@PortfolioDetail' {} a -> s {description = a} :: PortfolioDetail)

-- | The name to use for display purposes.
portfolioDetail_displayName :: Lens.Lens' PortfolioDetail (Core.Maybe Core.Text)
portfolioDetail_displayName = Lens.lens (\PortfolioDetail' {displayName} -> displayName) (\s@PortfolioDetail' {} a -> s {displayName = a} :: PortfolioDetail)

instance Core.FromJSON PortfolioDetail where
  parseJSON =
    Core.withObject
      "PortfolioDetail"
      ( \x ->
          PortfolioDetail'
            Core.<$> (x Core..:? "ProviderName")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "ARN")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "DisplayName")
      )

instance Core.Hashable PortfolioDetail

instance Core.NFData PortfolioDetail
