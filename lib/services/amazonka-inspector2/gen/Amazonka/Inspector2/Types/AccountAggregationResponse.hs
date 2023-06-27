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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | The Amazon Web Services account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The number of findings by severity.
    severityCounts :: Prelude.Maybe SeverityCounts
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
-- 'accountId', 'accountAggregationResponse_accountId' - The Amazon Web Services account ID.
--
-- 'severityCounts', 'accountAggregationResponse_severityCounts' - The number of findings by severity.
newAccountAggregationResponse ::
  AccountAggregationResponse
newAccountAggregationResponse =
  AccountAggregationResponse'
    { accountId =
        Prelude.Nothing,
      severityCounts = Prelude.Nothing
    }

-- | The Amazon Web Services account ID.
accountAggregationResponse_accountId :: Lens.Lens' AccountAggregationResponse (Prelude.Maybe Prelude.Text)
accountAggregationResponse_accountId = Lens.lens (\AccountAggregationResponse' {accountId} -> accountId) (\s@AccountAggregationResponse' {} a -> s {accountId = a} :: AccountAggregationResponse)

-- | The number of findings by severity.
accountAggregationResponse_severityCounts :: Lens.Lens' AccountAggregationResponse (Prelude.Maybe SeverityCounts)
accountAggregationResponse_severityCounts = Lens.lens (\AccountAggregationResponse' {severityCounts} -> severityCounts) (\s@AccountAggregationResponse' {} a -> s {severityCounts = a} :: AccountAggregationResponse)

instance Data.FromJSON AccountAggregationResponse where
  parseJSON =
    Data.withObject
      "AccountAggregationResponse"
      ( \x ->
          AccountAggregationResponse'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "severityCounts")
      )

instance Prelude.Hashable AccountAggregationResponse where
  hashWithSalt _salt AccountAggregationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` severityCounts

instance Prelude.NFData AccountAggregationResponse where
  rnf AccountAggregationResponse' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf severityCounts
