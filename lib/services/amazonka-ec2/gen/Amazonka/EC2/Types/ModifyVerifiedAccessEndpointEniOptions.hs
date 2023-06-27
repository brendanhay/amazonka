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
-- Module      : Amazonka.EC2.Types.ModifyVerifiedAccessEndpointEniOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ModifyVerifiedAccessEndpointEniOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessEndpointProtocol
import qualified Amazonka.Prelude as Prelude

-- | Describes the options when modifying a Verified Access endpoint with the
-- @network-interface@ type.
--
-- /See:/ 'newModifyVerifiedAccessEndpointEniOptions' smart constructor.
data ModifyVerifiedAccessEndpointEniOptions = ModifyVerifiedAccessEndpointEniOptions'
  { -- | The IP port number.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The IP protocol.
    protocol :: Prelude.Maybe VerifiedAccessEndpointProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessEndpointEniOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'modifyVerifiedAccessEndpointEniOptions_port' - The IP port number.
--
-- 'protocol', 'modifyVerifiedAccessEndpointEniOptions_protocol' - The IP protocol.
newModifyVerifiedAccessEndpointEniOptions ::
  ModifyVerifiedAccessEndpointEniOptions
newModifyVerifiedAccessEndpointEniOptions =
  ModifyVerifiedAccessEndpointEniOptions'
    { port =
        Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The IP port number.
modifyVerifiedAccessEndpointEniOptions_port :: Lens.Lens' ModifyVerifiedAccessEndpointEniOptions (Prelude.Maybe Prelude.Natural)
modifyVerifiedAccessEndpointEniOptions_port = Lens.lens (\ModifyVerifiedAccessEndpointEniOptions' {port} -> port) (\s@ModifyVerifiedAccessEndpointEniOptions' {} a -> s {port = a} :: ModifyVerifiedAccessEndpointEniOptions)

-- | The IP protocol.
modifyVerifiedAccessEndpointEniOptions_protocol :: Lens.Lens' ModifyVerifiedAccessEndpointEniOptions (Prelude.Maybe VerifiedAccessEndpointProtocol)
modifyVerifiedAccessEndpointEniOptions_protocol = Lens.lens (\ModifyVerifiedAccessEndpointEniOptions' {protocol} -> protocol) (\s@ModifyVerifiedAccessEndpointEniOptions' {} a -> s {protocol = a} :: ModifyVerifiedAccessEndpointEniOptions)

instance
  Prelude.Hashable
    ModifyVerifiedAccessEndpointEniOptions
  where
  hashWithSalt
    _salt
    ModifyVerifiedAccessEndpointEniOptions' {..} =
      _salt
        `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` protocol

instance
  Prelude.NFData
    ModifyVerifiedAccessEndpointEniOptions
  where
  rnf ModifyVerifiedAccessEndpointEniOptions' {..} =
    Prelude.rnf port `Prelude.seq` Prelude.rnf protocol

instance
  Data.ToQuery
    ModifyVerifiedAccessEndpointEniOptions
  where
  toQuery ModifyVerifiedAccessEndpointEniOptions' {..} =
    Prelude.mconcat
      ["Port" Data.=: port, "Protocol" Data.=: protocol]
