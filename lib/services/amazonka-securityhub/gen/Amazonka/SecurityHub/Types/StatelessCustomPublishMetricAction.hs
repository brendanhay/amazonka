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
-- Module      : Amazonka.SecurityHub.Types.StatelessCustomPublishMetricAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StatelessCustomPublishMetricAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.StatelessCustomPublishMetricActionDimension

-- | Information about metrics to publish to CloudWatch.
--
-- /See:/ 'newStatelessCustomPublishMetricAction' smart constructor.
data StatelessCustomPublishMetricAction = StatelessCustomPublishMetricAction'
  { -- | Defines CloudWatch dimension values to publish.
    dimensions :: Prelude.Maybe [StatelessCustomPublishMetricActionDimension]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatelessCustomPublishMetricAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'statelessCustomPublishMetricAction_dimensions' - Defines CloudWatch dimension values to publish.
newStatelessCustomPublishMetricAction ::
  StatelessCustomPublishMetricAction
newStatelessCustomPublishMetricAction =
  StatelessCustomPublishMetricAction'
    { dimensions =
        Prelude.Nothing
    }

-- | Defines CloudWatch dimension values to publish.
statelessCustomPublishMetricAction_dimensions :: Lens.Lens' StatelessCustomPublishMetricAction (Prelude.Maybe [StatelessCustomPublishMetricActionDimension])
statelessCustomPublishMetricAction_dimensions = Lens.lens (\StatelessCustomPublishMetricAction' {dimensions} -> dimensions) (\s@StatelessCustomPublishMetricAction' {} a -> s {dimensions = a} :: StatelessCustomPublishMetricAction) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    StatelessCustomPublishMetricAction
  where
  parseJSON =
    Data.withObject
      "StatelessCustomPublishMetricAction"
      ( \x ->
          StatelessCustomPublishMetricAction'
            Prelude.<$> (x Data..:? "Dimensions" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    StatelessCustomPublishMetricAction
  where
  hashWithSalt
    _salt
    StatelessCustomPublishMetricAction' {..} =
      _salt `Prelude.hashWithSalt` dimensions

instance
  Prelude.NFData
    StatelessCustomPublishMetricAction
  where
  rnf StatelessCustomPublishMetricAction' {..} =
    Prelude.rnf dimensions

instance
  Data.ToJSON
    StatelessCustomPublishMetricAction
  where
  toJSON StatelessCustomPublishMetricAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Dimensions" Data..=) Prelude.<$> dimensions]
      )
