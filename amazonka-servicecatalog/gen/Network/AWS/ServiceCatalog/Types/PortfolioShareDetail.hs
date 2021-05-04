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
-- Module      : Network.AWS.ServiceCatalog.Types.PortfolioShareDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.PortfolioShareDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.DescribePortfolioShareType

-- | Information about the portfolio share.
--
-- /See:/ 'newPortfolioShareDetail' smart constructor.
data PortfolioShareDetail = PortfolioShareDetail'
  { -- | Indicates whether TagOptions sharing is enabled or disabled for the
    -- portfolio share.
    shareTagOptions :: Prelude.Maybe Prelude.Bool,
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
    principalId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the shared portfolio is imported by the recipient
    -- account. If the recipient is in an organization node, the share is
    -- automatically imported, and the field is always set to true.
    accepted :: Prelude.Maybe Prelude.Bool,
    -- | The type of the portfolio share.
    type' :: Prelude.Maybe DescribePortfolioShareType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      principalId = Prelude.Nothing,
      accepted = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Indicates whether TagOptions sharing is enabled or disabled for the
-- portfolio share.
portfolioShareDetail_shareTagOptions :: Lens.Lens' PortfolioShareDetail (Prelude.Maybe Prelude.Bool)
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
portfolioShareDetail_principalId :: Lens.Lens' PortfolioShareDetail (Prelude.Maybe Prelude.Text)
portfolioShareDetail_principalId = Lens.lens (\PortfolioShareDetail' {principalId} -> principalId) (\s@PortfolioShareDetail' {} a -> s {principalId = a} :: PortfolioShareDetail)

-- | Indicates whether the shared portfolio is imported by the recipient
-- account. If the recipient is in an organization node, the share is
-- automatically imported, and the field is always set to true.
portfolioShareDetail_accepted :: Lens.Lens' PortfolioShareDetail (Prelude.Maybe Prelude.Bool)
portfolioShareDetail_accepted = Lens.lens (\PortfolioShareDetail' {accepted} -> accepted) (\s@PortfolioShareDetail' {} a -> s {accepted = a} :: PortfolioShareDetail)

-- | The type of the portfolio share.
portfolioShareDetail_type :: Lens.Lens' PortfolioShareDetail (Prelude.Maybe DescribePortfolioShareType)
portfolioShareDetail_type = Lens.lens (\PortfolioShareDetail' {type'} -> type') (\s@PortfolioShareDetail' {} a -> s {type' = a} :: PortfolioShareDetail)

instance Prelude.FromJSON PortfolioShareDetail where
  parseJSON =
    Prelude.withObject
      "PortfolioShareDetail"
      ( \x ->
          PortfolioShareDetail'
            Prelude.<$> (x Prelude..:? "ShareTagOptions")
            Prelude.<*> (x Prelude..:? "PrincipalId")
            Prelude.<*> (x Prelude..:? "Accepted")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable PortfolioShareDetail

instance Prelude.NFData PortfolioShareDetail
