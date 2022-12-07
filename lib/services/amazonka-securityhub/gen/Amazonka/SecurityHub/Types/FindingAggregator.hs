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
-- Module      : Amazonka.SecurityHub.Types.FindingAggregator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.FindingAggregator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A finding aggregator. A finding aggregator contains the configuration
-- for finding aggregation.
--
-- /See:/ 'newFindingAggregator' smart constructor.
data FindingAggregator = FindingAggregator'
  { -- | The ARN of the finding aggregator. You use the finding aggregator ARN to
    -- retrieve details for, update, and delete the finding aggregator.
    findingAggregatorArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingAggregator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingAggregatorArn', 'findingAggregator_findingAggregatorArn' - The ARN of the finding aggregator. You use the finding aggregator ARN to
-- retrieve details for, update, and delete the finding aggregator.
newFindingAggregator ::
  FindingAggregator
newFindingAggregator =
  FindingAggregator'
    { findingAggregatorArn =
        Prelude.Nothing
    }

-- | The ARN of the finding aggregator. You use the finding aggregator ARN to
-- retrieve details for, update, and delete the finding aggregator.
findingAggregator_findingAggregatorArn :: Lens.Lens' FindingAggregator (Prelude.Maybe Prelude.Text)
findingAggregator_findingAggregatorArn = Lens.lens (\FindingAggregator' {findingAggregatorArn} -> findingAggregatorArn) (\s@FindingAggregator' {} a -> s {findingAggregatorArn = a} :: FindingAggregator)

instance Data.FromJSON FindingAggregator where
  parseJSON =
    Data.withObject
      "FindingAggregator"
      ( \x ->
          FindingAggregator'
            Prelude.<$> (x Data..:? "FindingAggregatorArn")
      )

instance Prelude.Hashable FindingAggregator where
  hashWithSalt _salt FindingAggregator' {..} =
    _salt `Prelude.hashWithSalt` findingAggregatorArn

instance Prelude.NFData FindingAggregator where
  rnf FindingAggregator' {..} =
    Prelude.rnf findingAggregatorArn
