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
-- Module      : Amazonka.EC2.Types.PathStatementRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PathStatementRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PacketHeaderStatementRequest
import Amazonka.EC2.Types.ResourceStatementRequest
import qualified Amazonka.Prelude as Prelude

-- | Describes a path statement.
--
-- /See:/ 'newPathStatementRequest' smart constructor.
data PathStatementRequest = PathStatementRequest'
  { -- | The packet header statement.
    packetHeaderStatement :: Prelude.Maybe PacketHeaderStatementRequest,
    -- | The resource statement.
    resourceStatement :: Prelude.Maybe ResourceStatementRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathStatementRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packetHeaderStatement', 'pathStatementRequest_packetHeaderStatement' - The packet header statement.
--
-- 'resourceStatement', 'pathStatementRequest_resourceStatement' - The resource statement.
newPathStatementRequest ::
  PathStatementRequest
newPathStatementRequest =
  PathStatementRequest'
    { packetHeaderStatement =
        Prelude.Nothing,
      resourceStatement = Prelude.Nothing
    }

-- | The packet header statement.
pathStatementRequest_packetHeaderStatement :: Lens.Lens' PathStatementRequest (Prelude.Maybe PacketHeaderStatementRequest)
pathStatementRequest_packetHeaderStatement = Lens.lens (\PathStatementRequest' {packetHeaderStatement} -> packetHeaderStatement) (\s@PathStatementRequest' {} a -> s {packetHeaderStatement = a} :: PathStatementRequest)

-- | The resource statement.
pathStatementRequest_resourceStatement :: Lens.Lens' PathStatementRequest (Prelude.Maybe ResourceStatementRequest)
pathStatementRequest_resourceStatement = Lens.lens (\PathStatementRequest' {resourceStatement} -> resourceStatement) (\s@PathStatementRequest' {} a -> s {resourceStatement = a} :: PathStatementRequest)

instance Prelude.Hashable PathStatementRequest where
  hashWithSalt _salt PathStatementRequest' {..} =
    _salt
      `Prelude.hashWithSalt` packetHeaderStatement
      `Prelude.hashWithSalt` resourceStatement

instance Prelude.NFData PathStatementRequest where
  rnf PathStatementRequest' {..} =
    Prelude.rnf packetHeaderStatement
      `Prelude.seq` Prelude.rnf resourceStatement

instance Data.ToQuery PathStatementRequest where
  toQuery PathStatementRequest' {..} =
    Prelude.mconcat
      [ "PacketHeaderStatement"
          Data.=: packetHeaderStatement,
        "ResourceStatement" Data.=: resourceStatement
      ]
