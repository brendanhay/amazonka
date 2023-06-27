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
-- Module      : Amazonka.EC2.Types.PathStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PathStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PacketHeaderStatement
import Amazonka.EC2.Types.ResourceStatement
import qualified Amazonka.Prelude as Prelude

-- | Describes a path statement.
--
-- /See:/ 'newPathStatement' smart constructor.
data PathStatement = PathStatement'
  { -- | The packet header statement.
    packetHeaderStatement :: Prelude.Maybe PacketHeaderStatement,
    -- | The resource statement.
    resourceStatement :: Prelude.Maybe ResourceStatement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packetHeaderStatement', 'pathStatement_packetHeaderStatement' - The packet header statement.
--
-- 'resourceStatement', 'pathStatement_resourceStatement' - The resource statement.
newPathStatement ::
  PathStatement
newPathStatement =
  PathStatement'
    { packetHeaderStatement =
        Prelude.Nothing,
      resourceStatement = Prelude.Nothing
    }

-- | The packet header statement.
pathStatement_packetHeaderStatement :: Lens.Lens' PathStatement (Prelude.Maybe PacketHeaderStatement)
pathStatement_packetHeaderStatement = Lens.lens (\PathStatement' {packetHeaderStatement} -> packetHeaderStatement) (\s@PathStatement' {} a -> s {packetHeaderStatement = a} :: PathStatement)

-- | The resource statement.
pathStatement_resourceStatement :: Lens.Lens' PathStatement (Prelude.Maybe ResourceStatement)
pathStatement_resourceStatement = Lens.lens (\PathStatement' {resourceStatement} -> resourceStatement) (\s@PathStatement' {} a -> s {resourceStatement = a} :: PathStatement)

instance Data.FromXML PathStatement where
  parseXML x =
    PathStatement'
      Prelude.<$> (x Data..@? "packetHeaderStatement")
      Prelude.<*> (x Data..@? "resourceStatement")

instance Prelude.Hashable PathStatement where
  hashWithSalt _salt PathStatement' {..} =
    _salt
      `Prelude.hashWithSalt` packetHeaderStatement
      `Prelude.hashWithSalt` resourceStatement

instance Prelude.NFData PathStatement where
  rnf PathStatement' {..} =
    Prelude.rnf packetHeaderStatement
      `Prelude.seq` Prelude.rnf resourceStatement
