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
-- Module      : Amazonka.EC2.Types.ThroughResourcesStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ThroughResourcesStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ResourceStatement
import qualified Amazonka.Prelude as Prelude

-- | Describes a through resource statement.
--
-- /See:/ 'newThroughResourcesStatement' smart constructor.
data ThroughResourcesStatement = ThroughResourcesStatement'
  { -- | The resource statement.
    resourceStatement :: Prelude.Maybe ResourceStatement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThroughResourcesStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceStatement', 'throughResourcesStatement_resourceStatement' - The resource statement.
newThroughResourcesStatement ::
  ThroughResourcesStatement
newThroughResourcesStatement =
  ThroughResourcesStatement'
    { resourceStatement =
        Prelude.Nothing
    }

-- | The resource statement.
throughResourcesStatement_resourceStatement :: Lens.Lens' ThroughResourcesStatement (Prelude.Maybe ResourceStatement)
throughResourcesStatement_resourceStatement = Lens.lens (\ThroughResourcesStatement' {resourceStatement} -> resourceStatement) (\s@ThroughResourcesStatement' {} a -> s {resourceStatement = a} :: ThroughResourcesStatement)

instance Data.FromXML ThroughResourcesStatement where
  parseXML x =
    ThroughResourcesStatement'
      Prelude.<$> (x Data..@? "resourceStatement")

instance Prelude.Hashable ThroughResourcesStatement where
  hashWithSalt _salt ThroughResourcesStatement' {..} =
    _salt `Prelude.hashWithSalt` resourceStatement

instance Prelude.NFData ThroughResourcesStatement where
  rnf ThroughResourcesStatement' {..} =
    Prelude.rnf resourceStatement
