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
-- Module      : Network.AWS.FMS.Types.App
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.App where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An individual AWS Firewall Manager application.
--
-- /See:/ 'newApp' smart constructor.
data App = App'
  { -- | The application\'s name.
    appName :: Prelude.Text,
    -- | The IP protocol name or number. The name can be one of @tcp@, @udp@, or
    -- @icmp@. For information on possible numbers, see
    -- <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>.
    protocol :: Prelude.Text,
    -- | The application\'s port number, for example @80@.
    port :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'App' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appName', 'app_appName' - The application\'s name.
--
-- 'protocol', 'app_protocol' - The IP protocol name or number. The name can be one of @tcp@, @udp@, or
-- @icmp@. For information on possible numbers, see
-- <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>.
--
-- 'port', 'app_port' - The application\'s port number, for example @80@.
newApp ::
  -- | 'appName'
  Prelude.Text ->
  -- | 'protocol'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  App
newApp pAppName_ pProtocol_ pPort_ =
  App'
    { appName = pAppName_,
      protocol = pProtocol_,
      port = pPort_
    }

-- | The application\'s name.
app_appName :: Lens.Lens' App Prelude.Text
app_appName = Lens.lens (\App' {appName} -> appName) (\s@App' {} a -> s {appName = a} :: App)

-- | The IP protocol name or number. The name can be one of @tcp@, @udp@, or
-- @icmp@. For information on possible numbers, see
-- <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers>.
app_protocol :: Lens.Lens' App Prelude.Text
app_protocol = Lens.lens (\App' {protocol} -> protocol) (\s@App' {} a -> s {protocol = a} :: App)

-- | The application\'s port number, for example @80@.
app_port :: Lens.Lens' App Prelude.Natural
app_port = Lens.lens (\App' {port} -> port) (\s@App' {} a -> s {port = a} :: App)

instance Core.FromJSON App where
  parseJSON =
    Core.withObject
      "App"
      ( \x ->
          App'
            Prelude.<$> (x Core..: "AppName")
            Prelude.<*> (x Core..: "Protocol")
            Prelude.<*> (x Core..: "Port")
      )

instance Prelude.Hashable App

instance Prelude.NFData App

instance Core.ToJSON App where
  toJSON App' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AppName" Core..= appName),
            Prelude.Just ("Protocol" Core..= protocol),
            Prelude.Just ("Port" Core..= port)
          ]
      )
