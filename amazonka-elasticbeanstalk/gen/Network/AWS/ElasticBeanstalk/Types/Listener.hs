{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticBeanstalk.Types.Listener
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Listener where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the properties of a Listener for the LoadBalancer.
--
-- /See:/ 'newListener' smart constructor.
data Listener = Listener'
  { -- | The port that is used by the Listener.
    port :: Prelude.Maybe Prelude.Int,
    -- | The protocol that is used by the Listener.
    protocol :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromXML Listener where
  parseXML x =
    Listener'
      Prelude.<$> (x Prelude..@? "Port")
      Prelude.<*> (x Prelude..@? "Protocol")

instance Prelude.Hashable Listener

instance Prelude.NFData Listener
