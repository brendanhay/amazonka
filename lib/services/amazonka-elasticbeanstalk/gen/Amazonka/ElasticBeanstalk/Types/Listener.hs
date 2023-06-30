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
-- Module      : Amazonka.ElasticBeanstalk.Types.Listener
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.Listener where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the properties of a Listener for the LoadBalancer.
--
-- /See:/ 'newListener' smart constructor.
data Listener = Listener'
  { -- | The port that is used by the Listener.
    port :: Prelude.Maybe Prelude.Int,
    -- | The protocol that is used by the Listener.
    protocol :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Listener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'listener_port' - The port that is used by the Listener.
--
-- 'protocol', 'listener_protocol' - The protocol that is used by the Listener.
newListener ::
  Listener
newListener =
  Listener'
    { port = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The port that is used by the Listener.
listener_port :: Lens.Lens' Listener (Prelude.Maybe Prelude.Int)
listener_port = Lens.lens (\Listener' {port} -> port) (\s@Listener' {} a -> s {port = a} :: Listener)

-- | The protocol that is used by the Listener.
listener_protocol :: Lens.Lens' Listener (Prelude.Maybe Prelude.Text)
listener_protocol = Lens.lens (\Listener' {protocol} -> protocol) (\s@Listener' {} a -> s {protocol = a} :: Listener)

instance Data.FromXML Listener where
  parseXML x =
    Listener'
      Prelude.<$> (x Data..@? "Port")
      Prelude.<*> (x Data..@? "Protocol")

instance Prelude.Hashable Listener where
  hashWithSalt _salt Listener' {..} =
    _salt
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData Listener where
  rnf Listener' {..} =
    Prelude.rnf port `Prelude.seq` Prelude.rnf protocol
