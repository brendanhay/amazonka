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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.PortfolioDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a portfolio.
--
-- /See:/ 'newPortfolioDetail' smart constructor.
data PortfolioDetail = PortfolioDetail'
  { -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the portfolio provider.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The ARN assigned to the portfolio.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name to use for display purposes.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The portfolio identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of the portfolio.
    description :: Prelude.Maybe Prelude.Text
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
-- 'createdTime', 'portfolioDetail_createdTime' - The UTC time stamp of the creation time.
--
-- 'providerName', 'portfolioDetail_providerName' - The name of the portfolio provider.
--
-- 'arn', 'portfolioDetail_arn' - The ARN assigned to the portfolio.
--
-- 'displayName', 'portfolioDetail_displayName' - The name to use for display purposes.
--
-- 'id', 'portfolioDetail_id' - The portfolio identifier.
--
-- 'description', 'portfolioDetail_description' - The description of the portfolio.
newPortfolioDetail ::
  PortfolioDetail
newPortfolioDetail =
  PortfolioDetail'
    { createdTime = Prelude.Nothing,
      providerName = Prelude.Nothing,
      arn = Prelude.Nothing,
      displayName = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The UTC time stamp of the creation time.
portfolioDetail_createdTime :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.UTCTime)
portfolioDetail_createdTime = Lens.lens (\PortfolioDetail' {createdTime} -> createdTime) (\s@PortfolioDetail' {} a -> s {createdTime = a} :: PortfolioDetail) Prelude.. Lens.mapping Data._Time

-- | The name of the portfolio provider.
portfolioDetail_providerName :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_providerName = Lens.lens (\PortfolioDetail' {providerName} -> providerName) (\s@PortfolioDetail' {} a -> s {providerName = a} :: PortfolioDetail)

-- | The ARN assigned to the portfolio.
portfolioDetail_arn :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_arn = Lens.lens (\PortfolioDetail' {arn} -> arn) (\s@PortfolioDetail' {} a -> s {arn = a} :: PortfolioDetail)

-- | The name to use for display purposes.
portfolioDetail_displayName :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_displayName = Lens.lens (\PortfolioDetail' {displayName} -> displayName) (\s@PortfolioDetail' {} a -> s {displayName = a} :: PortfolioDetail)

-- | The portfolio identifier.
portfolioDetail_id :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_id = Lens.lens (\PortfolioDetail' {id} -> id) (\s@PortfolioDetail' {} a -> s {id = a} :: PortfolioDetail)

-- | The description of the portfolio.
portfolioDetail_description :: Lens.Lens' PortfolioDetail (Prelude.Maybe Prelude.Text)
portfolioDetail_description = Lens.lens (\PortfolioDetail' {description} -> description) (\s@PortfolioDetail' {} a -> s {description = a} :: PortfolioDetail)

instance Data.FromJSON PortfolioDetail where
  parseJSON =
    Data.withObject
      "PortfolioDetail"
      ( \x ->
          PortfolioDetail'
            Prelude.<$> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "ProviderName")
            Prelude.<*> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Description")
      )

instance Prelude.Hashable PortfolioDetail where
  hashWithSalt _salt PortfolioDetail' {..} =
    _salt `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` providerName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description

instance Prelude.NFData PortfolioDetail where
  rnf PortfolioDetail' {..} =
    Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf providerName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
