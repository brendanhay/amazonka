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
-- Module      : Amazonka.OpenSearchServerless.Types.CapacityLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.CapacityLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The maximum capacity limits for all OpenSearch Serverless collections,
-- in OpenSearch Compute Units (OCUs). These limits are used to scale your
-- collections based on the current workload. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-overview.html#serverless-scaling Autoscaling>.
--
-- /See:/ 'newCapacityLimits' smart constructor.
data CapacityLimits = CapacityLimits'
  { -- | The maximum indexing capacity for collections.
    maxIndexingCapacityInOCU :: Prelude.Maybe Prelude.Natural,
    -- | The maximum search capacity for collections.
    maxSearchCapacityInOCU :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxIndexingCapacityInOCU', 'capacityLimits_maxIndexingCapacityInOCU' - The maximum indexing capacity for collections.
--
-- 'maxSearchCapacityInOCU', 'capacityLimits_maxSearchCapacityInOCU' - The maximum search capacity for collections.
newCapacityLimits ::
  CapacityLimits
newCapacityLimits =
  CapacityLimits'
    { maxIndexingCapacityInOCU =
        Prelude.Nothing,
      maxSearchCapacityInOCU = Prelude.Nothing
    }

-- | The maximum indexing capacity for collections.
capacityLimits_maxIndexingCapacityInOCU :: Lens.Lens' CapacityLimits (Prelude.Maybe Prelude.Natural)
capacityLimits_maxIndexingCapacityInOCU = Lens.lens (\CapacityLimits' {maxIndexingCapacityInOCU} -> maxIndexingCapacityInOCU) (\s@CapacityLimits' {} a -> s {maxIndexingCapacityInOCU = a} :: CapacityLimits)

-- | The maximum search capacity for collections.
capacityLimits_maxSearchCapacityInOCU :: Lens.Lens' CapacityLimits (Prelude.Maybe Prelude.Natural)
capacityLimits_maxSearchCapacityInOCU = Lens.lens (\CapacityLimits' {maxSearchCapacityInOCU} -> maxSearchCapacityInOCU) (\s@CapacityLimits' {} a -> s {maxSearchCapacityInOCU = a} :: CapacityLimits)

instance Data.FromJSON CapacityLimits where
  parseJSON =
    Data.withObject
      "CapacityLimits"
      ( \x ->
          CapacityLimits'
            Prelude.<$> (x Data..:? "maxIndexingCapacityInOCU")
            Prelude.<*> (x Data..:? "maxSearchCapacityInOCU")
      )

instance Prelude.Hashable CapacityLimits where
  hashWithSalt _salt CapacityLimits' {..} =
    _salt
      `Prelude.hashWithSalt` maxIndexingCapacityInOCU
      `Prelude.hashWithSalt` maxSearchCapacityInOCU

instance Prelude.NFData CapacityLimits where
  rnf CapacityLimits' {..} =
    Prelude.rnf maxIndexingCapacityInOCU
      `Prelude.seq` Prelude.rnf maxSearchCapacityInOCU

instance Data.ToJSON CapacityLimits where
  toJSON CapacityLimits' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxIndexingCapacityInOCU" Data..=)
              Prelude.<$> maxIndexingCapacityInOCU,
            ("maxSearchCapacityInOCU" Data..=)
              Prelude.<$> maxSearchCapacityInOCU
          ]
      )
