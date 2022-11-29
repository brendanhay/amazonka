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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PathStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PacketHeaderStatement
import Amazonka.EC2.Types.ResourceStatement
import qualified Amazonka.Prelude as Prelude

-- | Describes a path statement.
--
-- /See:/ 'newPathStatement' smart constructor.
data PathStatement = PathStatement'
  { -- | The resource statement.
    resourceStatement :: Prelude.Maybe ResourceStatement,
    -- | The packet header statement.
    packetHeaderStatement :: Prelude.Maybe PacketHeaderStatement
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
-- 'resourceStatement', 'pathStatement_resourceStatement' - The resource statement.
--
-- 'packetHeaderStatement', 'pathStatement_packetHeaderStatement' - The packet header statement.
newPathStatement ::
  PathStatement
newPathStatement =
  PathStatement'
    { resourceStatement = Prelude.Nothing,
      packetHeaderStatement = Prelude.Nothing
    }

-- | The resource statement.
pathStatement_resourceStatement :: Lens.Lens' PathStatement (Prelude.Maybe ResourceStatement)
pathStatement_resourceStatement = Lens.lens (\PathStatement' {resourceStatement} -> resourceStatement) (\s@PathStatement' {} a -> s {resourceStatement = a} :: PathStatement)

-- | The packet header statement.
pathStatement_packetHeaderStatement :: Lens.Lens' PathStatement (Prelude.Maybe PacketHeaderStatement)
pathStatement_packetHeaderStatement = Lens.lens (\PathStatement' {packetHeaderStatement} -> packetHeaderStatement) (\s@PathStatement' {} a -> s {packetHeaderStatement = a} :: PathStatement)

instance Core.FromXML PathStatement where
  parseXML x =
    PathStatement'
      Prelude.<$> (x Core..@? "resourceStatement")
      Prelude.<*> (x Core..@? "packetHeaderStatement")

instance Prelude.Hashable PathStatement where
  hashWithSalt _salt PathStatement' {..} =
    _salt `Prelude.hashWithSalt` resourceStatement
      `Prelude.hashWithSalt` packetHeaderStatement

instance Prelude.NFData PathStatement where
  rnf PathStatement' {..} =
    Prelude.rnf resourceStatement
      `Prelude.seq` Prelude.rnf packetHeaderStatement
