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
-- Module      : Amazonka.NetworkManager.Types.ConnectAttachmentOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ConnectAttachmentOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.TunnelProtocol
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network Connect attachment options.
--
-- /See:/ 'newConnectAttachmentOptions' smart constructor.
data ConnectAttachmentOptions = ConnectAttachmentOptions'
  { -- | The protocol used for the attachment connection.
    protocol :: Prelude.Maybe TunnelProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectAttachmentOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocol', 'connectAttachmentOptions_protocol' - The protocol used for the attachment connection.
newConnectAttachmentOptions ::
  ConnectAttachmentOptions
newConnectAttachmentOptions =
  ConnectAttachmentOptions'
    { protocol =
        Prelude.Nothing
    }

-- | The protocol used for the attachment connection.
connectAttachmentOptions_protocol :: Lens.Lens' ConnectAttachmentOptions (Prelude.Maybe TunnelProtocol)
connectAttachmentOptions_protocol = Lens.lens (\ConnectAttachmentOptions' {protocol} -> protocol) (\s@ConnectAttachmentOptions' {} a -> s {protocol = a} :: ConnectAttachmentOptions)

instance Data.FromJSON ConnectAttachmentOptions where
  parseJSON =
    Data.withObject
      "ConnectAttachmentOptions"
      ( \x ->
          ConnectAttachmentOptions'
            Prelude.<$> (x Data..:? "Protocol")
      )

instance Prelude.Hashable ConnectAttachmentOptions where
  hashWithSalt _salt ConnectAttachmentOptions' {..} =
    _salt `Prelude.hashWithSalt` protocol

instance Prelude.NFData ConnectAttachmentOptions where
  rnf ConnectAttachmentOptions' {..} =
    Prelude.rnf protocol

instance Data.ToJSON ConnectAttachmentOptions where
  toJSON ConnectAttachmentOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Protocol" Data..=) Prelude.<$> protocol]
      )
