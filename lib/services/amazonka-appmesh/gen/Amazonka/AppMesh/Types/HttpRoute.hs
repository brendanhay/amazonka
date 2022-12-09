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
-- Module      : Amazonka.AppMesh.Types.HttpRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpRoute where

import Amazonka.AppMesh.Types.HttpRetryPolicy
import Amazonka.AppMesh.Types.HttpRouteAction
import Amazonka.AppMesh.Types.HttpRouteMatch
import Amazonka.AppMesh.Types.HttpTimeout
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON HttpRoute where
  parseJSON =
    Data.withObject
      "HttpRoute"
      ( \x ->
          HttpRoute'
            Prelude.<$> (x Data..:? "retryPolicy")
            Prelude.<*> (x Data..:? "timeout")
            Prelude.<*> (x Data..: "action")
            Prelude.<*> (x Data..: "match")
      )

instance Prelude.Hashable HttpRoute where
  hashWithSalt _salt HttpRoute' {..} =
    _salt `Prelude.hashWithSalt` retryPolicy
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` match

instance Prelude.NFData HttpRoute where
  rnf HttpRoute' {..} =
    Prelude.rnf retryPolicy
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf match

instance Data.ToJSON HttpRoute where
  toJSON HttpRoute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("retryPolicy" Data..=) Prelude.<$> retryPolicy,
            ("timeout" Data..=) Prelude.<$> timeout,
            Prelude.Just ("action" Data..= action),
            Prelude.Just ("match" Data..= match)
          ]
      )
