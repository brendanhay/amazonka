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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.DefaultRouteInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.DefaultRouteInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types.RouteActivationState
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the default route type.
--
-- /See:/ 'newDefaultRouteInput' smart constructor.
data DefaultRouteInput = DefaultRouteInput'
  { -- | If set to @ACTIVE@, traffic is forwarded to this route’s service after
    -- the route is created.
    activationState :: Prelude.Maybe RouteActivationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultRouteInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activationState', 'defaultRouteInput_activationState' - If set to @ACTIVE@, traffic is forwarded to this route’s service after
-- the route is created.
newDefaultRouteInput ::
  DefaultRouteInput
newDefaultRouteInput =
  DefaultRouteInput'
    { activationState =
        Prelude.Nothing
    }

-- | If set to @ACTIVE@, traffic is forwarded to this route’s service after
-- the route is created.
defaultRouteInput_activationState :: Lens.Lens' DefaultRouteInput (Prelude.Maybe RouteActivationState)
defaultRouteInput_activationState = Lens.lens (\DefaultRouteInput' {activationState} -> activationState) (\s@DefaultRouteInput' {} a -> s {activationState = a} :: DefaultRouteInput)

instance Prelude.Hashable DefaultRouteInput where
  hashWithSalt _salt DefaultRouteInput' {..} =
    _salt `Prelude.hashWithSalt` activationState

instance Prelude.NFData DefaultRouteInput where
  rnf DefaultRouteInput' {..} =
    Prelude.rnf activationState

instance Data.ToJSON DefaultRouteInput where
  toJSON DefaultRouteInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActivationState" Data..=)
              Prelude.<$> activationState
          ]
      )
