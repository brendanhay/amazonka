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
-- Module      : Network.AWS.IoT.Types.TermsAggregation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TermsAggregation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Performs an aggregation that will return a list of buckets. The list of
-- buckets is a ranked list of the number of occurrences of an aggregation
-- field value.
--
-- /See:/ 'newTermsAggregation' smart constructor.
data TermsAggregation = TermsAggregation'
  { -- | The number of buckets to return in the response. Default to 10.
    maxBuckets :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TermsAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxBuckets', 'termsAggregation_maxBuckets' - The number of buckets to return in the response. Default to 10.
newTermsAggregation ::
  TermsAggregation
newTermsAggregation =
  TermsAggregation' {maxBuckets = Prelude.Nothing}

-- | The number of buckets to return in the response. Default to 10.
termsAggregation_maxBuckets :: Lens.Lens' TermsAggregation (Prelude.Maybe Prelude.Natural)
termsAggregation_maxBuckets = Lens.lens (\TermsAggregation' {maxBuckets} -> maxBuckets) (\s@TermsAggregation' {} a -> s {maxBuckets = a} :: TermsAggregation)

instance Prelude.Hashable TermsAggregation

instance Prelude.NFData TermsAggregation

instance Core.ToJSON TermsAggregation where
  toJSON TermsAggregation' {..} =
    Core.object
      ( Prelude.catMaybes
          [("maxBuckets" Core..=) Prelude.<$> maxBuckets]
      )
