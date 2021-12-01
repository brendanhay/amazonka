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
-- Module      : Amazonka.ServiceCatalog.Types.PortfolioDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.PortfolioDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a portfolio.
--
-- /See:/ 'newPortfolioDetail' smart constructor.
data PortfolioDetail = PortfolioDetail'
  { -- | The ARN assigned to the portfolio.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The portfolio identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name to use for display purposes.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The description of the portfolio.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the portfolio provider.
    providerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortfolioDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'portfolioDetail_arn' - The ARN assigned to the portfolio.
--
-- 'createdTime', 'portfolioDetail_createdTime' - The UTC time stamp of the creation time.
--
-- 'id', 'portfolioDetail_id' - The portfolio identifier.
--
-- 'displayName', 'portfolioDetail_displayName' - The name to use for display purposes.
--
-- 'description', 'portfolioDetail_description' - The description of the portfolio.
--
-- 'providerName', 'portfolioDetail_providerName' - The name of the portfolio provider.
newPortfolioDetail ::
  PortfolioDetail
newPortfolioDetail =
  PortfolioDetail'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      id = Prelude.Nothing,
      displayName = Prelude.Nothing,
      description = Prelude.Nothing,
      providerName = Prelude.Nothing
    }

-- | The ARN assigned to the portfolio.
portfolioDetail_arn :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_arn = Lens.lens (\PortfolioDetail' {arn} -> arn) (\s@PortfolioDetail' {} a -> s {arn = a} :: PortfolioDetail)

-- | The UTC time stamp of the creation time.
portfolioDetail_createdTime :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.UTCTime)
portfolioDetail_createdTime = Lens.lens (\PortfolioDetail' {createdTime} -> createdTime) (\s@PortfolioDetail' {} a -> s {createdTime = a} :: PortfolioDetail) Prelude.. Lens.mapping Core._Time

-- | The portfolio identifier.
portfolioDetail_id :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_id = Lens.lens (\PortfolioDetail' {id} -> id) (\s@PortfolioDetail' {} a -> s {id = a} :: PortfolioDetail)

-- | The name to use for display purposes.
portfolioDetail_displayName :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_displayName = Lens.lens (\PortfolioDetail' {displayName} -> displayName) (\s@PortfolioDetail' {} a -> s {displayName = a} :: PortfolioDetail)

-- | The description of the portfolio.
portfolioDetail_description :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_description = Lens.lens (\PortfolioDetail' {description} -> description) (\s@PortfolioDetail' {} a -> s {description = a} :: PortfolioDetail)

-- | The name of the portfolio provider.
portfolioDetail_providerName :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_providerName = Lens.lens (\PortfolioDetail' {providerName} -> providerName) (\s@PortfolioDetail' {} a -> s {providerName = a} :: PortfolioDetail)

instance Core.FromJSON PortfolioDetail where
  parseJSON =
    Core.withObject
      "PortfolioDetail"
      ( \x ->
          PortfolioDetail'
            Prelude.<$> (x Core..:? "ARN")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ProviderName")
      )

instance Prelude.Hashable PortfolioDetail where
  hashWithSalt salt' PortfolioDetail' {..} =
    salt' `Prelude.hashWithSalt` providerName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn

instance Prelude.NFData PortfolioDetail where
  rnf PortfolioDetail' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf providerName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdTime
