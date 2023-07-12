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
-- Module      : Amazonka.OpenSearchServerless.Types.AccountSettingsDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.AccountSettingsDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.CapacityLimits
import qualified Amazonka.Prelude as Prelude

-- | OpenSearch Serverless-related information for the current account.
--
-- /See:/ 'newAccountSettingsDetail' smart constructor.
data AccountSettingsDetail = AccountSettingsDetail'
  { capacityLimits :: Prelude.Maybe CapacityLimits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountSettingsDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityLimits', 'accountSettingsDetail_capacityLimits' - Undocumented member.
newAccountSettingsDetail ::
  AccountSettingsDetail
newAccountSettingsDetail =
  AccountSettingsDetail'
    { capacityLimits =
        Prelude.Nothing
    }

-- | Undocumented member.
accountSettingsDetail_capacityLimits :: Lens.Lens' AccountSettingsDetail (Prelude.Maybe CapacityLimits)
accountSettingsDetail_capacityLimits = Lens.lens (\AccountSettingsDetail' {capacityLimits} -> capacityLimits) (\s@AccountSettingsDetail' {} a -> s {capacityLimits = a} :: AccountSettingsDetail)

instance Data.FromJSON AccountSettingsDetail where
  parseJSON =
    Data.withObject
      "AccountSettingsDetail"
      ( \x ->
          AccountSettingsDetail'
            Prelude.<$> (x Data..:? "capacityLimits")
      )

instance Prelude.Hashable AccountSettingsDetail where
  hashWithSalt _salt AccountSettingsDetail' {..} =
    _salt `Prelude.hashWithSalt` capacityLimits

instance Prelude.NFData AccountSettingsDetail where
  rnf AccountSettingsDetail' {..} =
    Prelude.rnf capacityLimits
