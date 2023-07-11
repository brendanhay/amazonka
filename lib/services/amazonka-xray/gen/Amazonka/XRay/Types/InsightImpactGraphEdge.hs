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
-- Module      : Amazonka.XRay.Types.InsightImpactGraphEdge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.InsightImpactGraphEdge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connection between two service in an insight impact graph.
--
-- /See:/ 'newInsightImpactGraphEdge' smart constructor.
data InsightImpactGraphEdge = InsightImpactGraphEdge'
  { -- | Identifier of the edge. Unique within a service map.
    referenceId :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightImpactGraphEdge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'referenceId', 'insightImpactGraphEdge_referenceId' - Identifier of the edge. Unique within a service map.
newInsightImpactGraphEdge ::
  InsightImpactGraphEdge
newInsightImpactGraphEdge =
  InsightImpactGraphEdge'
    { referenceId =
        Prelude.Nothing
    }

-- | Identifier of the edge. Unique within a service map.
insightImpactGraphEdge_referenceId :: Lens.Lens' InsightImpactGraphEdge (Prelude.Maybe Prelude.Int)
insightImpactGraphEdge_referenceId = Lens.lens (\InsightImpactGraphEdge' {referenceId} -> referenceId) (\s@InsightImpactGraphEdge' {} a -> s {referenceId = a} :: InsightImpactGraphEdge)

instance Data.FromJSON InsightImpactGraphEdge where
  parseJSON =
    Data.withObject
      "InsightImpactGraphEdge"
      ( \x ->
          InsightImpactGraphEdge'
            Prelude.<$> (x Data..:? "ReferenceId")
      )

instance Prelude.Hashable InsightImpactGraphEdge where
  hashWithSalt _salt InsightImpactGraphEdge' {..} =
    _salt `Prelude.hashWithSalt` referenceId

instance Prelude.NFData InsightImpactGraphEdge where
  rnf InsightImpactGraphEdge' {..} =
    Prelude.rnf referenceId
