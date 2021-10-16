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
-- Module      : Network.AWS.CloudFront.Types.ConflictingAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ConflictingAlias where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An alias (also called a CNAME) and the CloudFront distribution and
-- Amazon Web Services account ID that it’s associated with. The
-- distribution and account IDs are partially hidden, which allows you to
-- identify the distributions and accounts that you own, but helps to
-- protect the information of ones that you don’t own.
--
-- /See:/ 'newConflictingAlias' smart constructor.
data ConflictingAlias = ConflictingAlias'
  { -- | The (partially hidden) ID of the Amazon Web Services account that owns
    -- the distribution that’s associated with the alias.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | An alias (also called a CNAME).
    alias :: Prelude.Maybe Prelude.Text,
    -- | The (partially hidden) ID of the CloudFront distribution associated with
    -- the alias.
    distributionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConflictingAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'conflictingAlias_accountId' - The (partially hidden) ID of the Amazon Web Services account that owns
-- the distribution that’s associated with the alias.
--
-- 'alias', 'conflictingAlias_alias' - An alias (also called a CNAME).
--
-- 'distributionId', 'conflictingAlias_distributionId' - The (partially hidden) ID of the CloudFront distribution associated with
-- the alias.
newConflictingAlias ::
  ConflictingAlias
newConflictingAlias =
  ConflictingAlias'
    { accountId = Prelude.Nothing,
      alias = Prelude.Nothing,
      distributionId = Prelude.Nothing
    }

-- | The (partially hidden) ID of the Amazon Web Services account that owns
-- the distribution that’s associated with the alias.
conflictingAlias_accountId :: Lens.Lens' ConflictingAlias (Prelude.Maybe Prelude.Text)
conflictingAlias_accountId = Lens.lens (\ConflictingAlias' {accountId} -> accountId) (\s@ConflictingAlias' {} a -> s {accountId = a} :: ConflictingAlias)

-- | An alias (also called a CNAME).
conflictingAlias_alias :: Lens.Lens' ConflictingAlias (Prelude.Maybe Prelude.Text)
conflictingAlias_alias = Lens.lens (\ConflictingAlias' {alias} -> alias) (\s@ConflictingAlias' {} a -> s {alias = a} :: ConflictingAlias)

-- | The (partially hidden) ID of the CloudFront distribution associated with
-- the alias.
conflictingAlias_distributionId :: Lens.Lens' ConflictingAlias (Prelude.Maybe Prelude.Text)
conflictingAlias_distributionId = Lens.lens (\ConflictingAlias' {distributionId} -> distributionId) (\s@ConflictingAlias' {} a -> s {distributionId = a} :: ConflictingAlias)

instance Core.FromXML ConflictingAlias where
  parseXML x =
    ConflictingAlias'
      Prelude.<$> (x Core..@? "AccountId")
      Prelude.<*> (x Core..@? "Alias")
      Prelude.<*> (x Core..@? "DistributionId")

instance Prelude.Hashable ConflictingAlias

instance Prelude.NFData ConflictingAlias
