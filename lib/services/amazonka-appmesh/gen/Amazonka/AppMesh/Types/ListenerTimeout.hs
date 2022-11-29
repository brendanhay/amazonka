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
-- Module      : Amazonka.AppMesh.Types.ListenerTimeout
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ListenerTimeout where

import Amazonka.AppMesh.Types.GrpcTimeout
import Amazonka.AppMesh.Types.HttpTimeout
import Amazonka.AppMesh.Types.TcpTimeout
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents timeouts for different protocols.
--
-- /See:/ 'newListenerTimeout' smart constructor.
data ListenerTimeout = ListenerTimeout'
  { -- | An object that represents types of timeouts.
    http :: Prelude.Maybe HttpTimeout,
    -- | An object that represents types of timeouts.
    http2 :: Prelude.Maybe HttpTimeout,
    -- | An object that represents types of timeouts.
    tcp :: Prelude.Maybe TcpTimeout,
    -- | An object that represents types of timeouts.
    grpc :: Prelude.Maybe GrpcTimeout
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListenerTimeout' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'http', 'listenerTimeout_http' - An object that represents types of timeouts.
--
-- 'http2', 'listenerTimeout_http2' - An object that represents types of timeouts.
--
-- 'tcp', 'listenerTimeout_tcp' - An object that represents types of timeouts.
--
-- 'grpc', 'listenerTimeout_grpc' - An object that represents types of timeouts.
newListenerTimeout ::
  ListenerTimeout
newListenerTimeout =
  ListenerTimeout'
    { http = Prelude.Nothing,
      http2 = Prelude.Nothing,
      tcp = Prelude.Nothing,
      grpc = Prelude.Nothing
    }

-- | An object that represents types of timeouts.
listenerTimeout_http :: Lens.Lens' ListenerTimeout (Prelude.Maybe HttpTimeout)
listenerTimeout_http = Lens.lens (\ListenerTimeout' {http} -> http) (\s@ListenerTimeout' {} a -> s {http = a} :: ListenerTimeout)

-- | An object that represents types of timeouts.
listenerTimeout_http2 :: Lens.Lens' ListenerTimeout (Prelude.Maybe HttpTimeout)
listenerTimeout_http2 = Lens.lens (\ListenerTimeout' {http2} -> http2) (\s@ListenerTimeout' {} a -> s {http2 = a} :: ListenerTimeout)

-- | An object that represents types of timeouts.
listenerTimeout_tcp :: Lens.Lens' ListenerTimeout (Prelude.Maybe TcpTimeout)
listenerTimeout_tcp = Lens.lens (\ListenerTimeout' {tcp} -> tcp) (\s@ListenerTimeout' {} a -> s {tcp = a} :: ListenerTimeout)

-- | An object that represents types of timeouts.
listenerTimeout_grpc :: Lens.Lens' ListenerTimeout (Prelude.Maybe GrpcTimeout)
listenerTimeout_grpc = Lens.lens (\ListenerTimeout' {grpc} -> grpc) (\s@ListenerTimeout' {} a -> s {grpc = a} :: ListenerTimeout)

instance Core.FromJSON ListenerTimeout where
  parseJSON =
    Core.withObject
      "ListenerTimeout"
      ( \x ->
          ListenerTimeout'
            Prelude.<$> (x Core..:? "http")
            Prelude.<*> (x Core..:? "http2")
            Prelude.<*> (x Core..:? "tcp")
            Prelude.<*> (x Core..:? "grpc")
      )

instance Prelude.Hashable ListenerTimeout where
  hashWithSalt _salt ListenerTimeout' {..} =
    _salt `Prelude.hashWithSalt` http
      `Prelude.hashWithSalt` http2
      `Prelude.hashWithSalt` tcp
      `Prelude.hashWithSalt` grpc

instance Prelude.NFData ListenerTimeout where
  rnf ListenerTimeout' {..} =
    Prelude.rnf http
      `Prelude.seq` Prelude.rnf http2
      `Prelude.seq` Prelude.rnf tcp
      `Prelude.seq` Prelude.rnf grpc

instance Core.ToJSON ListenerTimeout where
  toJSON ListenerTimeout' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("http" Core..=) Prelude.<$> http,
            ("http2" Core..=) Prelude.<$> http2,
            ("tcp" Core..=) Prelude.<$> tcp,
            ("grpc" Core..=) Prelude.<$> grpc
          ]
      )
