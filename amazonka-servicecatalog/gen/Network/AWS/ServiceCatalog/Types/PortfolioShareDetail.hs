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
-- Module      : Network.AWS.ServiceCatalog.Types.PortfolioShareDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.PortfolioShareDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.DescribePortfolioShareType

-- | Information about the portfolio share.
--
-- /See:/ 'newPortfolioShareDetail' smart constructor.
data PortfolioShareDetail = PortfolioShareDetail'
  { -- | Indicates whether TagOptions sharing is enabled or disabled for the
    -- portfolio share.
    shareTagOptions :: Core.Maybe Core.Bool,
    -- | The identifier of the recipient entity that received the portfolio
    -- share. The recipient entities can be one of the following:
    --
    -- 1. An external account.
    --
    -- 2. An organziation member account.
    --
    -- 3. An organzational unit (OU).
    --
    -- 4. The organization itself. (This shares with every account in the
    -- organization).
    principalId :: Core.Maybe Core.Text,
    -- | Indicates whether the shared portfolio is imported by the recipient
    -- account. If the recipient is in an organization node, the share is
    -- automatically imported, and the field is always set to true.
    accepted :: Core.Maybe Core.Bool,
    -- | The type of the portfolio share.
    type' :: Core.Maybe DescribePortfolioShareType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PortfolioShareDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareTagOptions', 'portfolioShareDetail_shareTagOptions' - Indicates whether TagOptions sharing is enabled or disabled for the
-- portfolio share.
--
-- 'principalId', 'portfolioShareDetail_principalId' - The identifier of the recipient entity that received the portfolio
-- share. The recipient entities can be one of the following:
--
-- 1. An external account.
--
-- 2. An organziation member account.
--
-- 3. An organzational unit (OU).
--
-- 4. The organization itself. (This shares with every account in the
-- organization).
--
-- 'accepted', 'portfolioShareDetail_accepted' - Indicates whether the shared portfolio is imported by the recipient
-- account. If the recipient is in an organization node, the share is
-- automatically imported, and the field is always set to true.
--
-- 'type'', 'portfolioShareDetail_type' - The type of the portfolio share.
newPortfolioShareDetail ::
  PortfolioShareDetail
newPortfolioShareDetail =
  PortfolioShareDetail'
    { shareTagOptions =
        Core.Nothing,
      principalId = Core.Nothing,
      accepted = Core.Nothing,
      type' = Core.Nothing
    }

-- | Indicates whether TagOptions sharing is enabled or disabled for the
-- portfolio share.
portfolioShareDetail_shareTagOptions :: Lens.Lens' PortfolioShareDetail (Core.Maybe Core.Bool)
portfolioShareDetail_shareTagOptions = Lens.lens (\PortfolioShareDetail' {shareTagOptions} -> shareTagOptions) (\s@PortfolioShareDetail' {} a -> s {shareTagOptions = a} :: PortfolioShareDetail)

-- | The identifier of the recipient entity that received the portfolio
-- share. The recipient entities can be one of the following:
--
-- 1. An external account.
--
-- 2. An organziation member account.
--
-- 3. An organzational unit (OU).
--
-- 4. The organization itself. (This shares with every account in the
-- organization).
portfolioShareDetail_principalId :: Lens.Lens' PortfolioShareDetail (Core.Maybe Core.Text)
portfolioShareDetail_principalId = Lens.lens (\PortfolioShareDetail' {principalId} -> principalId) (\s@PortfolioShareDetail' {} a -> s {principalId = a} :: PortfolioShareDetail)

-- | Indicates whether the shared portfolio is imported by the recipient
-- account. If the recipient is in an organization node, the share is
-- automatically imported, and the field is always set to true.
portfolioShareDetail_accepted :: Lens.Lens' PortfolioShareDetail (Core.Maybe Core.Bool)
portfolioShareDetail_accepted = Lens.lens (\PortfolioShareDetail' {accepted} -> accepted) (\s@PortfolioShareDetail' {} a -> s {accepted = a} :: PortfolioShareDetail)

-- | The type of the portfolio share.
portfolioShareDetail_type :: Lens.Lens' PortfolioShareDetail (Core.Maybe DescribePortfolioShareType)
portfolioShareDetail_type = Lens.lens (\PortfolioShareDetail' {type'} -> type') (\s@PortfolioShareDetail' {} a -> s {type' = a} :: PortfolioShareDetail)

instance Core.FromJSON PortfolioShareDetail where
  parseJSON =
    Core.withObject
      "PortfolioShareDetail"
      ( \x ->
          PortfolioShareDetail'
            Core.<$> (x Core..:? "ShareTagOptions")
            Core.<*> (x Core..:? "PrincipalId")
            Core.<*> (x Core..:? "Accepted")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable PortfolioShareDetail

instance Core.NFData PortfolioShareDetail
