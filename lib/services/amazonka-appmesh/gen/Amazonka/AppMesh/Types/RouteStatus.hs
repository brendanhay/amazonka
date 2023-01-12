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
-- Module      : Amazonka.AppMesh.Types.RouteStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.RouteStatus where

import Amazonka.AppMesh.Types.RouteStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the current status of a route.
--
-- /See:/ 'newRouteStatus' smart constructor.
data RouteStatus = RouteStatus'
  { -- | The current status for the route.
    status :: RouteStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'routeStatus_status' - The current status for the route.
newRouteStatus ::
  -- | 'status'
  RouteStatusCode ->
  RouteStatus
newRouteStatus pStatus_ =
  RouteStatus' {status = pStatus_}

-- | The current status for the route.
routeStatus_status :: Lens.Lens' RouteStatus RouteStatusCode
routeStatus_status = Lens.lens (\RouteStatus' {status} -> status) (\s@RouteStatus' {} a -> s {status = a} :: RouteStatus)

instance Data.FromJSON RouteStatus where
  parseJSON =
    Data.withObject
      "RouteStatus"
      ( \x ->
          RouteStatus' Prelude.<$> (x Data..: "status")
      )

instance Prelude.Hashable RouteStatus where
  hashWithSalt _salt RouteStatus' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData RouteStatus where
  rnf RouteStatus' {..} = Prelude.rnf status
