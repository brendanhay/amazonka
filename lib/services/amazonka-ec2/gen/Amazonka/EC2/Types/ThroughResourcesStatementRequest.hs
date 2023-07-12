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
-- Module      : Amazonka.EC2.Types.ThroughResourcesStatementRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ThroughResourcesStatementRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ResourceStatementRequest
import qualified Amazonka.Prelude as Prelude

-- | Describes a through resource statement.
--
-- /See:/ 'newThroughResourcesStatementRequest' smart constructor.
data ThroughResourcesStatementRequest = ThroughResourcesStatementRequest'
  { -- | The resource statement.
    resourceStatement :: Prelude.Maybe ResourceStatementRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThroughResourcesStatementRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceStatement', 'throughResourcesStatementRequest_resourceStatement' - The resource statement.
newThroughResourcesStatementRequest ::
  ThroughResourcesStatementRequest
newThroughResourcesStatementRequest =
  ThroughResourcesStatementRequest'
    { resourceStatement =
        Prelude.Nothing
    }

-- | The resource statement.
throughResourcesStatementRequest_resourceStatement :: Lens.Lens' ThroughResourcesStatementRequest (Prelude.Maybe ResourceStatementRequest)
throughResourcesStatementRequest_resourceStatement = Lens.lens (\ThroughResourcesStatementRequest' {resourceStatement} -> resourceStatement) (\s@ThroughResourcesStatementRequest' {} a -> s {resourceStatement = a} :: ThroughResourcesStatementRequest)

instance
  Prelude.Hashable
    ThroughResourcesStatementRequest
  where
  hashWithSalt
    _salt
    ThroughResourcesStatementRequest' {..} =
      _salt `Prelude.hashWithSalt` resourceStatement

instance
  Prelude.NFData
    ThroughResourcesStatementRequest
  where
  rnf ThroughResourcesStatementRequest' {..} =
    Prelude.rnf resourceStatement

instance
  Data.ToQuery
    ThroughResourcesStatementRequest
  where
  toQuery ThroughResourcesStatementRequest' {..} =
    Prelude.mconcat
      ["ResourceStatement" Data.=: resourceStatement]
