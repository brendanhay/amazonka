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
-- Module      : Amazonka.Inspector2.Types.FreeTrialAccountInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.FreeTrialAccountInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.FreeTrialInfo
import qualified Amazonka.Prelude as Prelude

-- | Information about the Amazon Inspector free trial for an account.
--
-- /See:/ 'newFreeTrialAccountInfo' smart constructor.
data FreeTrialAccountInfo = FreeTrialAccountInfo'
  { -- | The account associated with the Amazon Inspector free trial information.
    accountId :: Prelude.Text,
    -- | Contains information about the Amazon Inspector free trial for an
    -- account.
    freeTrialInfo :: [FreeTrialInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FreeTrialAccountInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'freeTrialAccountInfo_accountId' - The account associated with the Amazon Inspector free trial information.
--
-- 'freeTrialInfo', 'freeTrialAccountInfo_freeTrialInfo' - Contains information about the Amazon Inspector free trial for an
-- account.
newFreeTrialAccountInfo ::
  -- | 'accountId'
  Prelude.Text ->
  FreeTrialAccountInfo
newFreeTrialAccountInfo pAccountId_ =
  FreeTrialAccountInfo'
    { accountId = pAccountId_,
      freeTrialInfo = Prelude.mempty
    }

-- | The account associated with the Amazon Inspector free trial information.
freeTrialAccountInfo_accountId :: Lens.Lens' FreeTrialAccountInfo Prelude.Text
freeTrialAccountInfo_accountId = Lens.lens (\FreeTrialAccountInfo' {accountId} -> accountId) (\s@FreeTrialAccountInfo' {} a -> s {accountId = a} :: FreeTrialAccountInfo)

-- | Contains information about the Amazon Inspector free trial for an
-- account.
freeTrialAccountInfo_freeTrialInfo :: Lens.Lens' FreeTrialAccountInfo [FreeTrialInfo]
freeTrialAccountInfo_freeTrialInfo = Lens.lens (\FreeTrialAccountInfo' {freeTrialInfo} -> freeTrialInfo) (\s@FreeTrialAccountInfo' {} a -> s {freeTrialInfo = a} :: FreeTrialAccountInfo) Prelude.. Lens.coerced

instance Data.FromJSON FreeTrialAccountInfo where
  parseJSON =
    Data.withObject
      "FreeTrialAccountInfo"
      ( \x ->
          FreeTrialAccountInfo'
            Prelude.<$> (x Data..: "accountId")
            Prelude.<*> (x Data..:? "freeTrialInfo" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FreeTrialAccountInfo where
  hashWithSalt _salt FreeTrialAccountInfo' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` freeTrialInfo

instance Prelude.NFData FreeTrialAccountInfo where
  rnf FreeTrialAccountInfo' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf freeTrialInfo
