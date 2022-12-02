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
-- Module      : Amazonka.AppMesh.Types.TcpTimeout
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.TcpTimeout where

import Amazonka.AppMesh.Types.Duration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents types of timeouts.
--
-- /See:/ 'newTcpTimeout' smart constructor.
data TcpTimeout = TcpTimeout'
  { -- | An object that represents an idle timeout. An idle timeout bounds the
    -- amount of time that a connection may be idle. The default value is none.
    idle :: Prelude.Maybe Duration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TcpTimeout' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idle', 'tcpTimeout_idle' - An object that represents an idle timeout. An idle timeout bounds the
-- amount of time that a connection may be idle. The default value is none.
newTcpTimeout ::
  TcpTimeout
newTcpTimeout = TcpTimeout' {idle = Prelude.Nothing}

-- | An object that represents an idle timeout. An idle timeout bounds the
-- amount of time that a connection may be idle. The default value is none.
tcpTimeout_idle :: Lens.Lens' TcpTimeout (Prelude.Maybe Duration)
tcpTimeout_idle = Lens.lens (\TcpTimeout' {idle} -> idle) (\s@TcpTimeout' {} a -> s {idle = a} :: TcpTimeout)

instance Data.FromJSON TcpTimeout where
  parseJSON =
    Data.withObject
      "TcpTimeout"
      (\x -> TcpTimeout' Prelude.<$> (x Data..:? "idle"))

instance Prelude.Hashable TcpTimeout where
  hashWithSalt _salt TcpTimeout' {..} =
    _salt `Prelude.hashWithSalt` idle

instance Prelude.NFData TcpTimeout where
  rnf TcpTimeout' {..} = Prelude.rnf idle

instance Data.ToJSON TcpTimeout where
  toJSON TcpTimeout' {..} =
    Data.object
      ( Prelude.catMaybes
          [("idle" Data..=) Prelude.<$> idle]
      )
