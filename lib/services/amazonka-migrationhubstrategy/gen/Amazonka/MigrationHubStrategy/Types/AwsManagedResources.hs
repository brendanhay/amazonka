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
-- Module      : Amazonka.MigrationHubStrategy.Types.AwsManagedResources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AwsManagedResources where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.AwsManagedTargetDestination
import qualified Amazonka.Prelude as Prelude

-- | Object containing the choice of application destination that you
-- specify.
--
-- /See:/ 'newAwsManagedResources' smart constructor.
data AwsManagedResources = AwsManagedResources'
  { -- | The choice of application destination that you specify.
    targetDestination :: Prelude.NonEmpty AwsManagedTargetDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsManagedResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetDestination', 'awsManagedResources_targetDestination' - The choice of application destination that you specify.
newAwsManagedResources ::
  -- | 'targetDestination'
  Prelude.NonEmpty AwsManagedTargetDestination ->
  AwsManagedResources
newAwsManagedResources pTargetDestination_ =
  AwsManagedResources'
    { targetDestination =
        Lens.coerced Lens.# pTargetDestination_
    }

-- | The choice of application destination that you specify.
awsManagedResources_targetDestination :: Lens.Lens' AwsManagedResources (Prelude.NonEmpty AwsManagedTargetDestination)
awsManagedResources_targetDestination = Lens.lens (\AwsManagedResources' {targetDestination} -> targetDestination) (\s@AwsManagedResources' {} a -> s {targetDestination = a} :: AwsManagedResources) Prelude.. Lens.coerced

instance Data.FromJSON AwsManagedResources where
  parseJSON =
    Data.withObject
      "AwsManagedResources"
      ( \x ->
          AwsManagedResources'
            Prelude.<$> (x Data..: "targetDestination")
      )

instance Prelude.Hashable AwsManagedResources where
  hashWithSalt _salt AwsManagedResources' {..} =
    _salt `Prelude.hashWithSalt` targetDestination

instance Prelude.NFData AwsManagedResources where
  rnf AwsManagedResources' {..} =
    Prelude.rnf targetDestination

instance Data.ToJSON AwsManagedResources where
  toJSON AwsManagedResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("targetDestination" Data..= targetDestination)
          ]
      )
