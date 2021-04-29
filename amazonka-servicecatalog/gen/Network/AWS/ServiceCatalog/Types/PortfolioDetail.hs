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
-- Module      : Network.AWS.ServiceCatalog.Types.PortfolioDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.PortfolioDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a portfolio.
--
-- /See:/ 'newPortfolioDetail' smart constructor.
data PortfolioDetail = PortfolioDetail'
  { -- | The name of the portfolio provider.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The portfolio identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ARN assigned to the portfolio.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Prelude.POSIX,
    -- | The description of the portfolio.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name to use for display purposes.
    displayName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { providerName = Prelude.Nothing,
      id = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing
    }

-- | The name of the portfolio provider.
portfolioDetail_providerName :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_providerName = Lens.lens (\PortfolioDetail' {providerName} -> providerName) (\s@PortfolioDetail' {} a -> s {providerName = a} :: PortfolioDetail)

-- | The portfolio identifier.
portfolioDetail_id :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_id = Lens.lens (\PortfolioDetail' {id} -> id) (\s@PortfolioDetail' {} a -> s {id = a} :: PortfolioDetail)

-- | The ARN assigned to the portfolio.
portfolioDetail_arn :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_arn = Lens.lens (\PortfolioDetail' {arn} -> arn) (\s@PortfolioDetail' {} a -> s {arn = a} :: PortfolioDetail)

-- | The UTC time stamp of the creation time.
portfolioDetail_createdTime :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.UTCTime)
portfolioDetail_createdTime = Lens.lens (\PortfolioDetail' {createdTime} -> createdTime) (\s@PortfolioDetail' {} a -> s {createdTime = a} :: PortfolioDetail) Prelude.. Lens.mapping Prelude._Time

-- | The description of the portfolio.
portfolioDetail_description :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_description = Lens.lens (\PortfolioDetail' {description} -> description) (\s@PortfolioDetail' {} a -> s {description = a} :: PortfolioDetail)

-- | The name to use for display purposes.
portfolioDetail_displayName :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_displayName = Lens.lens (\PortfolioDetail' {displayName} -> displayName) (\s@PortfolioDetail' {} a -> s {displayName = a} :: PortfolioDetail)

instance Prelude.FromJSON PortfolioDetail where
  parseJSON =
    Prelude.withObject
      "PortfolioDetail"
      ( \x ->
          PortfolioDetail'
            Prelude.<$> (x Prelude..:? "ProviderName")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "ARN")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "DisplayName")
      )

instance Prelude.Hashable PortfolioDetail

instance Prelude.NFData PortfolioDetail
