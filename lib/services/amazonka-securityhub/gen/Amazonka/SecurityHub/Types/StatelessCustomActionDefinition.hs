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
-- Module      : Amazonka.SecurityHub.Types.StatelessCustomActionDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StatelessCustomActionDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.StatelessCustomPublishMetricAction

-- | The definition of a custom action that can be used for stateless packet
-- handling.
--
-- /See:/ 'newStatelessCustomActionDefinition' smart constructor.
data StatelessCustomActionDefinition = StatelessCustomActionDefinition'
  { -- | Information about metrics to publish to CloudWatch.
    publishMetricAction :: Prelude.Maybe StatelessCustomPublishMetricAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatelessCustomActionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publishMetricAction', 'statelessCustomActionDefinition_publishMetricAction' - Information about metrics to publish to CloudWatch.
newStatelessCustomActionDefinition ::
  StatelessCustomActionDefinition
newStatelessCustomActionDefinition =
  StatelessCustomActionDefinition'
    { publishMetricAction =
        Prelude.Nothing
    }

-- | Information about metrics to publish to CloudWatch.
statelessCustomActionDefinition_publishMetricAction :: Lens.Lens' StatelessCustomActionDefinition (Prelude.Maybe StatelessCustomPublishMetricAction)
statelessCustomActionDefinition_publishMetricAction = Lens.lens (\StatelessCustomActionDefinition' {publishMetricAction} -> publishMetricAction) (\s@StatelessCustomActionDefinition' {} a -> s {publishMetricAction = a} :: StatelessCustomActionDefinition)

instance
  Data.FromJSON
    StatelessCustomActionDefinition
  where
  parseJSON =
    Data.withObject
      "StatelessCustomActionDefinition"
      ( \x ->
          StatelessCustomActionDefinition'
            Prelude.<$> (x Data..:? "PublishMetricAction")
      )

instance
  Prelude.Hashable
    StatelessCustomActionDefinition
  where
  hashWithSalt
    _salt
    StatelessCustomActionDefinition' {..} =
      _salt `Prelude.hashWithSalt` publishMetricAction

instance
  Prelude.NFData
    StatelessCustomActionDefinition
  where
  rnf StatelessCustomActionDefinition' {..} =
    Prelude.rnf publishMetricAction

instance Data.ToJSON StatelessCustomActionDefinition where
  toJSON StatelessCustomActionDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PublishMetricAction" Data..=)
              Prelude.<$> publishMetricAction
          ]
      )
