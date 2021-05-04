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
-- Module      : Network.AWS.GuardDuty.Types.AccountLevelPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccountLevelPermissions where

import Network.AWS.GuardDuty.Types.BlockPublicAccess
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the account level permissions on the S3
-- bucket.
--
-- /See:/ 'newAccountLevelPermissions' smart constructor.
data AccountLevelPermissions = AccountLevelPermissions'
  { -- | Describes the S3 Block Public Access settings of the bucket\'s parent
    -- account.
    blockPublicAccess :: Prelude.Maybe BlockPublicAccess
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccountLevelPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockPublicAccess', 'accountLevelPermissions_blockPublicAccess' - Describes the S3 Block Public Access settings of the bucket\'s parent
-- account.
newAccountLevelPermissions ::
  AccountLevelPermissions
newAccountLevelPermissions =
  AccountLevelPermissions'
    { blockPublicAccess =
        Prelude.Nothing
    }

-- | Describes the S3 Block Public Access settings of the bucket\'s parent
-- account.
accountLevelPermissions_blockPublicAccess :: Lens.Lens' AccountLevelPermissions (Prelude.Maybe BlockPublicAccess)
accountLevelPermissions_blockPublicAccess = Lens.lens (\AccountLevelPermissions' {blockPublicAccess} -> blockPublicAccess) (\s@AccountLevelPermissions' {} a -> s {blockPublicAccess = a} :: AccountLevelPermissions)

instance Prelude.FromJSON AccountLevelPermissions where
  parseJSON =
    Prelude.withObject
      "AccountLevelPermissions"
      ( \x ->
          AccountLevelPermissions'
            Prelude.<$> (x Prelude..:? "blockPublicAccess")
      )

instance Prelude.Hashable AccountLevelPermissions

instance Prelude.NFData AccountLevelPermissions
