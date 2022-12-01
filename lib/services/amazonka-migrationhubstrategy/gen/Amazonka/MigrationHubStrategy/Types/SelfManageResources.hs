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
-- Module      : Amazonka.MigrationHubStrategy.Types.SelfManageResources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.SelfManageResources where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubStrategy.Types.SelfManageTargetDestination
import qualified Amazonka.Prelude as Prelude

-- | Self-managed resources.
--
-- /See:/ 'newSelfManageResources' smart constructor.
data SelfManageResources = SelfManageResources'
  { -- | Self-managed resources target destination.
    targetDestination :: Prelude.NonEmpty SelfManageTargetDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelfManageResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetDestination', 'selfManageResources_targetDestination' - Self-managed resources target destination.
newSelfManageResources ::
  -- | 'targetDestination'
  Prelude.NonEmpty SelfManageTargetDestination ->
  SelfManageResources
newSelfManageResources pTargetDestination_ =
  SelfManageResources'
    { targetDestination =
        Lens.coerced Lens.# pTargetDestination_
    }

-- | Self-managed resources target destination.
selfManageResources_targetDestination :: Lens.Lens' SelfManageResources (Prelude.NonEmpty SelfManageTargetDestination)
selfManageResources_targetDestination = Lens.lens (\SelfManageResources' {targetDestination} -> targetDestination) (\s@SelfManageResources' {} a -> s {targetDestination = a} :: SelfManageResources) Prelude.. Lens.coerced

instance Core.FromJSON SelfManageResources where
  parseJSON =
    Core.withObject
      "SelfManageResources"
      ( \x ->
          SelfManageResources'
            Prelude.<$> (x Core..: "targetDestination")
      )

instance Prelude.Hashable SelfManageResources where
  hashWithSalt _salt SelfManageResources' {..} =
    _salt `Prelude.hashWithSalt` targetDestination

instance Prelude.NFData SelfManageResources where
  rnf SelfManageResources' {..} =
    Prelude.rnf targetDestination

instance Core.ToJSON SelfManageResources where
  toJSON SelfManageResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("targetDestination" Core..= targetDestination)
          ]
      )
