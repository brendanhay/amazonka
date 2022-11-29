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
-- Module      : Amazonka.AppMesh.Types.GrpcRetryPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcRetryPolicy where

import Amazonka.AppMesh.Types.Duration
import Amazonka.AppMesh.Types.GrpcRetryPolicyEvent
import Amazonka.AppMesh.Types.TcpRetryPolicyEvent
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a retry policy. Specify at least one value for
-- at least one of the types of @RetryEvents@, a value for @maxRetries@,
-- and a value for @perRetryTimeout@. Both @server-error@ and
-- @gateway-error@ under @httpRetryEvents@ include the Envoy @reset@
-- policy. For more information on the @reset@ policy, see the
-- <https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/router_filter#x-envoy-retry-on Envoy documentation>.
--
-- /See:/ 'newGrpcRetryPolicy' smart constructor.
data GrpcRetryPolicy = GrpcRetryPolicy'
  { -- | Specify at least one of the valid values.
    grpcRetryEvents :: Prelude.Maybe (Prelude.NonEmpty GrpcRetryPolicyEvent),
    -- | Specify at least one of the following values.
    --
    -- -   __server-error__ – HTTP status codes 500, 501, 502, 503, 504, 505,
    --     506, 507, 508, 510, and 511
    --
    -- -   __gateway-error__ – HTTP status codes 502, 503, and 504
    --
    -- -   __client-error__ – HTTP status code 409
    --
    -- -   __stream-error__ – Retry on refused stream
    httpRetryEvents :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Specify a valid value. The event occurs before any processing of a
    -- request has started and is encountered when the upstream is temporarily
    -- or permanently unavailable.
    tcpRetryEvents :: Prelude.Maybe (Prelude.NonEmpty TcpRetryPolicyEvent),
    -- | The maximum number of retry attempts.
    maxRetries :: Prelude.Natural,
    -- | The timeout for each retry attempt.
    perRetryTimeout :: Duration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrpcRetryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grpcRetryEvents', 'grpcRetryPolicy_grpcRetryEvents' - Specify at least one of the valid values.
--
-- 'httpRetryEvents', 'grpcRetryPolicy_httpRetryEvents' - Specify at least one of the following values.
--
-- -   __server-error__ – HTTP status codes 500, 501, 502, 503, 504, 505,
--     506, 507, 508, 510, and 511
--
-- -   __gateway-error__ – HTTP status codes 502, 503, and 504
--
-- -   __client-error__ – HTTP status code 409
--
-- -   __stream-error__ – Retry on refused stream
--
-- 'tcpRetryEvents', 'grpcRetryPolicy_tcpRetryEvents' - Specify a valid value. The event occurs before any processing of a
-- request has started and is encountered when the upstream is temporarily
-- or permanently unavailable.
--
-- 'maxRetries', 'grpcRetryPolicy_maxRetries' - The maximum number of retry attempts.
--
-- 'perRetryTimeout', 'grpcRetryPolicy_perRetryTimeout' - The timeout for each retry attempt.
newGrpcRetryPolicy ::
  -- | 'maxRetries'
  Prelude.Natural ->
  -- | 'perRetryTimeout'
  Duration ->
  GrpcRetryPolicy
newGrpcRetryPolicy pMaxRetries_ pPerRetryTimeout_ =
  GrpcRetryPolicy'
    { grpcRetryEvents = Prelude.Nothing,
      httpRetryEvents = Prelude.Nothing,
      tcpRetryEvents = Prelude.Nothing,
      maxRetries = pMaxRetries_,
      perRetryTimeout = pPerRetryTimeout_
    }

-- | Specify at least one of the valid values.
grpcRetryPolicy_grpcRetryEvents :: Lens.Lens' GrpcRetryPolicy (Prelude.Maybe (Prelude.NonEmpty GrpcRetryPolicyEvent))
grpcRetryPolicy_grpcRetryEvents = Lens.lens (\GrpcRetryPolicy' {grpcRetryEvents} -> grpcRetryEvents) (\s@GrpcRetryPolicy' {} a -> s {grpcRetryEvents = a} :: GrpcRetryPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Specify at least one of the following values.
--
-- -   __server-error__ – HTTP status codes 500, 501, 502, 503, 504, 505,
--     506, 507, 508, 510, and 511
--
-- -   __gateway-error__ – HTTP status codes 502, 503, and 504
--
-- -   __client-error__ – HTTP status code 409
--
-- -   __stream-error__ – Retry on refused stream
grpcRetryPolicy_httpRetryEvents :: Lens.Lens' GrpcRetryPolicy (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
grpcRetryPolicy_httpRetryEvents = Lens.lens (\GrpcRetryPolicy' {httpRetryEvents} -> httpRetryEvents) (\s@GrpcRetryPolicy' {} a -> s {httpRetryEvents = a} :: GrpcRetryPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Specify a valid value. The event occurs before any processing of a
-- request has started and is encountered when the upstream is temporarily
-- or permanently unavailable.
grpcRetryPolicy_tcpRetryEvents :: Lens.Lens' GrpcRetryPolicy (Prelude.Maybe (Prelude.NonEmpty TcpRetryPolicyEvent))
grpcRetryPolicy_tcpRetryEvents = Lens.lens (\GrpcRetryPolicy' {tcpRetryEvents} -> tcpRetryEvents) (\s@GrpcRetryPolicy' {} a -> s {tcpRetryEvents = a} :: GrpcRetryPolicy) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of retry attempts.
grpcRetryPolicy_maxRetries :: Lens.Lens' GrpcRetryPolicy Prelude.Natural
grpcRetryPolicy_maxRetries = Lens.lens (\GrpcRetryPolicy' {maxRetries} -> maxRetries) (\s@GrpcRetryPolicy' {} a -> s {maxRetries = a} :: GrpcRetryPolicy)

-- | The timeout for each retry attempt.
grpcRetryPolicy_perRetryTimeout :: Lens.Lens' GrpcRetryPolicy Duration
grpcRetryPolicy_perRetryTimeout = Lens.lens (\GrpcRetryPolicy' {perRetryTimeout} -> perRetryTimeout) (\s@GrpcRetryPolicy' {} a -> s {perRetryTimeout = a} :: GrpcRetryPolicy)

instance Core.FromJSON GrpcRetryPolicy where
  parseJSON =
    Core.withObject
      "GrpcRetryPolicy"
      ( \x ->
          GrpcRetryPolicy'
            Prelude.<$> (x Core..:? "grpcRetryEvents")
            Prelude.<*> (x Core..:? "httpRetryEvents")
            Prelude.<*> (x Core..:? "tcpRetryEvents")
            Prelude.<*> (x Core..: "maxRetries")
            Prelude.<*> (x Core..: "perRetryTimeout")
      )

instance Prelude.Hashable GrpcRetryPolicy where
  hashWithSalt _salt GrpcRetryPolicy' {..} =
    _salt `Prelude.hashWithSalt` grpcRetryEvents
      `Prelude.hashWithSalt` httpRetryEvents
      `Prelude.hashWithSalt` tcpRetryEvents
      `Prelude.hashWithSalt` maxRetries
      `Prelude.hashWithSalt` perRetryTimeout

instance Prelude.NFData GrpcRetryPolicy where
  rnf GrpcRetryPolicy' {..} =
    Prelude.rnf grpcRetryEvents
      `Prelude.seq` Prelude.rnf httpRetryEvents
      `Prelude.seq` Prelude.rnf tcpRetryEvents
      `Prelude.seq` Prelude.rnf maxRetries
      `Prelude.seq` Prelude.rnf perRetryTimeout

instance Core.ToJSON GrpcRetryPolicy where
  toJSON GrpcRetryPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("grpcRetryEvents" Core..=)
              Prelude.<$> grpcRetryEvents,
            ("httpRetryEvents" Core..=)
              Prelude.<$> httpRetryEvents,
            ("tcpRetryEvents" Core..=)
              Prelude.<$> tcpRetryEvents,
            Prelude.Just ("maxRetries" Core..= maxRetries),
            Prelude.Just
              ("perRetryTimeout" Core..= perRetryTimeout)
          ]
      )
