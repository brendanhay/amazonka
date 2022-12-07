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
-- Module      : Amazonka.IoT.Types.BucketsAggregationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.BucketsAggregationType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.TermsAggregation
import qualified Amazonka.Prelude as Prelude

-- | The type of bucketed aggregation performed.
--
-- /See:/ 'newBucketsAggregationType' smart constructor.
data BucketsAggregationType = BucketsAggregationType'
  { -- | Performs an aggregation that will return a list of buckets. The list of
    -- buckets is a ranked list of the number of occurrences of an aggregation
    -- field value.
    termsAggregation :: Prelude.Maybe TermsAggregation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketsAggregationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'termsAggregation', 'bucketsAggregationType_termsAggregation' - Performs an aggregation that will return a list of buckets. The list of
-- buckets is a ranked list of the number of occurrences of an aggregation
-- field value.
newBucketsAggregationType ::
  BucketsAggregationType
newBucketsAggregationType =
  BucketsAggregationType'
    { termsAggregation =
        Prelude.Nothing
    }

-- | Performs an aggregation that will return a list of buckets. The list of
-- buckets is a ranked list of the number of occurrences of an aggregation
-- field value.
bucketsAggregationType_termsAggregation :: Lens.Lens' BucketsAggregationType (Prelude.Maybe TermsAggregation)
bucketsAggregationType_termsAggregation = Lens.lens (\BucketsAggregationType' {termsAggregation} -> termsAggregation) (\s@BucketsAggregationType' {} a -> s {termsAggregation = a} :: BucketsAggregationType)

instance Prelude.Hashable BucketsAggregationType where
  hashWithSalt _salt BucketsAggregationType' {..} =
    _salt `Prelude.hashWithSalt` termsAggregation

instance Prelude.NFData BucketsAggregationType where
  rnf BucketsAggregationType' {..} =
    Prelude.rnf termsAggregation

instance Data.ToJSON BucketsAggregationType where
  toJSON BucketsAggregationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("termsAggregation" Data..=)
              Prelude.<$> termsAggregation
          ]
      )
