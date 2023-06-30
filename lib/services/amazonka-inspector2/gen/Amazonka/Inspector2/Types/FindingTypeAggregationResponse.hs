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
-- Module      : Amazonka.Inspector2.Types.FindingTypeAggregationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.FindingTypeAggregationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | A response that contains the results of a finding type aggregation.
--
-- /See:/ 'newFindingTypeAggregationResponse' smart constructor.
data FindingTypeAggregationResponse = FindingTypeAggregationResponse'
  { -- | The ID of the Amazon Web Services account associated with the findings.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The value to sort results by.
    severityCounts :: Prelude.Maybe SeverityCounts
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingTypeAggregationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'findingTypeAggregationResponse_accountId' - The ID of the Amazon Web Services account associated with the findings.
--
-- 'severityCounts', 'findingTypeAggregationResponse_severityCounts' - The value to sort results by.
newFindingTypeAggregationResponse ::
  FindingTypeAggregationResponse
newFindingTypeAggregationResponse =
  FindingTypeAggregationResponse'
    { accountId =
        Prelude.Nothing,
      severityCounts = Prelude.Nothing
    }

-- | The ID of the Amazon Web Services account associated with the findings.
findingTypeAggregationResponse_accountId :: Lens.Lens' FindingTypeAggregationResponse (Prelude.Maybe Prelude.Text)
findingTypeAggregationResponse_accountId = Lens.lens (\FindingTypeAggregationResponse' {accountId} -> accountId) (\s@FindingTypeAggregationResponse' {} a -> s {accountId = a} :: FindingTypeAggregationResponse)

-- | The value to sort results by.
findingTypeAggregationResponse_severityCounts :: Lens.Lens' FindingTypeAggregationResponse (Prelude.Maybe SeverityCounts)
findingTypeAggregationResponse_severityCounts = Lens.lens (\FindingTypeAggregationResponse' {severityCounts} -> severityCounts) (\s@FindingTypeAggregationResponse' {} a -> s {severityCounts = a} :: FindingTypeAggregationResponse)

instance Data.FromJSON FindingTypeAggregationResponse where
  parseJSON =
    Data.withObject
      "FindingTypeAggregationResponse"
      ( \x ->
          FindingTypeAggregationResponse'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "severityCounts")
      )

instance
  Prelude.Hashable
    FindingTypeAggregationResponse
  where
  hashWithSalt
    _salt
    FindingTypeAggregationResponse' {..} =
      _salt
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` severityCounts

instance
  Prelude.NFData
    FindingTypeAggregationResponse
  where
  rnf FindingTypeAggregationResponse' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf severityCounts
