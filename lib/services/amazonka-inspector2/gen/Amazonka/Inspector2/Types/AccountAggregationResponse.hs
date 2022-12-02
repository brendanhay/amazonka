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
-- Module      : Amazonka.Inspector2.Types.AccountAggregationResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AccountAggregationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | An aggregation of findings by Amazon Web Services account ID.
--
-- /See:/ 'newAccountAggregationResponse' smart constructor.
data AccountAggregationResponse = AccountAggregationResponse'
  { -- | The number of findings by severity.
    severityCounts :: Prelude.Maybe SeverityCounts,
    -- | The Amazon Web Services account ID.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountAggregationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'severityCounts', 'accountAggregationResponse_severityCounts' - The number of findings by severity.
--
-- 'accountId', 'accountAggregationResponse_accountId' - The Amazon Web Services account ID.
newAccountAggregationResponse ::
  AccountAggregationResponse
newAccountAggregationResponse =
  AccountAggregationResponse'
    { severityCounts =
        Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The number of findings by severity.
accountAggregationResponse_severityCounts :: Lens.Lens' AccountAggregationResponse (Prelude.Maybe SeverityCounts)
accountAggregationResponse_severityCounts = Lens.lens (\AccountAggregationResponse' {severityCounts} -> severityCounts) (\s@AccountAggregationResponse' {} a -> s {severityCounts = a} :: AccountAggregationResponse)

-- | The Amazon Web Services account ID.
accountAggregationResponse_accountId :: Lens.Lens' AccountAggregationResponse (Prelude.Maybe Prelude.Text)
accountAggregationResponse_accountId = Lens.lens (\AccountAggregationResponse' {accountId} -> accountId) (\s@AccountAggregationResponse' {} a -> s {accountId = a} :: AccountAggregationResponse)

instance Data.FromJSON AccountAggregationResponse where
  parseJSON =
    Data.withObject
      "AccountAggregationResponse"
      ( \x ->
          AccountAggregationResponse'
            Prelude.<$> (x Data..:? "severityCounts")
            Prelude.<*> (x Data..:? "accountId")
      )

instance Prelude.Hashable AccountAggregationResponse where
  hashWithSalt _salt AccountAggregationResponse' {..} =
    _salt `Prelude.hashWithSalt` severityCounts
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData AccountAggregationResponse where
  rnf AccountAggregationResponse' {..} =
    Prelude.rnf severityCounts
      `Prelude.seq` Prelude.rnf accountId
