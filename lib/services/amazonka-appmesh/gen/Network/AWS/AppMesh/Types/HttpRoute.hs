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
-- Module      : Network.AWS.AppMesh.Types.HttpRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types.HttpRoute where

import Network.AWS.AppMesh.Types.HttpRetryPolicy
import Network.AWS.AppMesh.Types.HttpRouteAction
import Network.AWS.AppMesh.Types.HttpRouteMatch
import Network.AWS.AppMesh.Types.HttpTimeout
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that represents an HTTP or HTTP\/2 route type.
--
-- /See:/ 'newHttpRoute' smart constructor.
data HttpRoute = HttpRoute'
  { -- | An object that represents a retry policy.
    retryPolicy :: Prelude.Maybe HttpRetryPolicy,
    -- | An object that represents types of timeouts.
    timeout :: Prelude.Maybe HttpTimeout,
    -- | An object that represents the action to take if a match is determined.
    action :: HttpRouteAction,
    -- | An object that represents the criteria for determining a request match.
    match :: HttpRouteMatch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retryPolicy', 'httpRoute_retryPolicy' - An object that represents a retry policy.
--
-- 'timeout', 'httpRoute_timeout' - An object that represents types of timeouts.
--
-- 'action', 'httpRoute_action' - An object that represents the action to take if a match is determined.
--
-- 'match', 'httpRoute_match' - An object that represents the criteria for determining a request match.
newHttpRoute ::
  -- | 'action'
  HttpRouteAction ->
  -- | 'match'
  HttpRouteMatch ->
  HttpRoute
newHttpRoute pAction_ pMatch_ =
  HttpRoute'
    { retryPolicy = Prelude.Nothing,
      timeout = Prelude.Nothing,
      action = pAction_,
      match = pMatch_
    }

-- | An object that represents a retry policy.
httpRoute_retryPolicy :: Lens.Lens' HttpRoute (Prelude.Maybe HttpRetryPolicy)
httpRoute_retryPolicy = Lens.lens (\HttpRoute' {retryPolicy} -> retryPolicy) (\s@HttpRoute' {} a -> s {retryPolicy = a} :: HttpRoute)

-- | An object that represents types of timeouts.
httpRoute_timeout :: Lens.Lens' HttpRoute (Prelude.Maybe HttpTimeout)
httpRoute_timeout = Lens.lens (\HttpRoute' {timeout} -> timeout) (\s@HttpRoute' {} a -> s {timeout = a} :: HttpRoute)

-- | An object that represents the action to take if a match is determined.
httpRoute_action :: Lens.Lens' HttpRoute HttpRouteAction
httpRoute_action = Lens.lens (\HttpRoute' {action} -> action) (\s@HttpRoute' {} a -> s {action = a} :: HttpRoute)

-- | An object that represents the criteria for determining a request match.
httpRoute_match :: Lens.Lens' HttpRoute HttpRouteMatch
httpRoute_match = Lens.lens (\HttpRoute' {match} -> match) (\s@HttpRoute' {} a -> s {match = a} :: HttpRoute)

instance Core.FromJSON HttpRoute where
  parseJSON =
    Core.withObject
      "HttpRoute"
      ( \x ->
          HttpRoute'
            Prelude.<$> (x Core..:? "retryPolicy")
            Prelude.<*> (x Core..:? "timeout")
            Prelude.<*> (x Core..: "action")
            Prelude.<*> (x Core..: "match")
      )

instance Prelude.Hashable HttpRoute

instance Prelude.NFData HttpRoute

instance Core.ToJSON HttpRoute where
  toJSON HttpRoute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("retryPolicy" Core..=) Prelude.<$> retryPolicy,
            ("timeout" Core..=) Prelude.<$> timeout,
            Prelude.Just ("action" Core..= action),
            Prelude.Just ("match" Core..= match)
          ]
      )
