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
-- Module      : Amazonka.AppMesh.Types.GrpcTimeout
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcTimeout where

import Amazonka.AppMesh.Types.Duration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents types of timeouts.
--
-- /See:/ 'newGrpcTimeout' smart constructor.
data GrpcTimeout = GrpcTimeout'
  { -- | An object that represents a per request timeout. The default value is 15
    -- seconds. If you set a higher timeout, then make sure that the higher
    -- value is set for each App Mesh resource in a conversation. For example,
    -- if a virtual node backend uses a virtual router provider to route to
    -- another virtual node, then the timeout should be greater than 15 seconds
    -- for the source and destination virtual node and the route.
    perRequest :: Prelude.Maybe Duration,
    -- | An object that represents an idle timeout. An idle timeout bounds the
    -- amount of time that a connection may be idle. The default value is none.
    idle :: Prelude.Maybe Duration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrpcTimeout' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'perRequest', 'grpcTimeout_perRequest' - An object that represents a per request timeout. The default value is 15
-- seconds. If you set a higher timeout, then make sure that the higher
-- value is set for each App Mesh resource in a conversation. For example,
-- if a virtual node backend uses a virtual router provider to route to
-- another virtual node, then the timeout should be greater than 15 seconds
-- for the source and destination virtual node and the route.
--
-- 'idle', 'grpcTimeout_idle' - An object that represents an idle timeout. An idle timeout bounds the
-- amount of time that a connection may be idle. The default value is none.
newGrpcTimeout ::
  GrpcTimeout
newGrpcTimeout =
  GrpcTimeout'
    { perRequest = Prelude.Nothing,
      idle = Prelude.Nothing
    }

-- | An object that represents a per request timeout. The default value is 15
-- seconds. If you set a higher timeout, then make sure that the higher
-- value is set for each App Mesh resource in a conversation. For example,
-- if a virtual node backend uses a virtual router provider to route to
-- another virtual node, then the timeout should be greater than 15 seconds
-- for the source and destination virtual node and the route.
grpcTimeout_perRequest :: Lens.Lens' GrpcTimeout (Prelude.Maybe Duration)
grpcTimeout_perRequest = Lens.lens (\GrpcTimeout' {perRequest} -> perRequest) (\s@GrpcTimeout' {} a -> s {perRequest = a} :: GrpcTimeout)

-- | An object that represents an idle timeout. An idle timeout bounds the
-- amount of time that a connection may be idle. The default value is none.
grpcTimeout_idle :: Lens.Lens' GrpcTimeout (Prelude.Maybe Duration)
grpcTimeout_idle = Lens.lens (\GrpcTimeout' {idle} -> idle) (\s@GrpcTimeout' {} a -> s {idle = a} :: GrpcTimeout)

instance Core.FromJSON GrpcTimeout where
  parseJSON =
    Core.withObject
      "GrpcTimeout"
      ( \x ->
          GrpcTimeout'
            Prelude.<$> (x Core..:? "perRequest")
            Prelude.<*> (x Core..:? "idle")
      )

instance Prelude.Hashable GrpcTimeout where
  hashWithSalt _salt GrpcTimeout' {..} =
    _salt `Prelude.hashWithSalt` perRequest
      `Prelude.hashWithSalt` idle

instance Prelude.NFData GrpcTimeout where
  rnf GrpcTimeout' {..} =
    Prelude.rnf perRequest
      `Prelude.seq` Prelude.rnf idle

instance Core.ToJSON GrpcTimeout where
  toJSON GrpcTimeout' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("perRequest" Core..=) Prelude.<$> perRequest,
            ("idle" Core..=) Prelude.<$> idle
          ]
      )
