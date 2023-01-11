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
-- Module      : Amazonka.QLDBSession.Types.StartSessionRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.StartSessionRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a request to start a new session.
--
-- /See:/ 'newStartSessionRequest' smart constructor.
data StartSessionRequest = StartSessionRequest'
  { -- | The name of the ledger to start a new session against.
    ledgerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSessionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ledgerName', 'startSessionRequest_ledgerName' - The name of the ledger to start a new session against.
newStartSessionRequest ::
  -- | 'ledgerName'
  Prelude.Text ->
  StartSessionRequest
newStartSessionRequest pLedgerName_ =
  StartSessionRequest' {ledgerName = pLedgerName_}

-- | The name of the ledger to start a new session against.
startSessionRequest_ledgerName :: Lens.Lens' StartSessionRequest Prelude.Text
startSessionRequest_ledgerName = Lens.lens (\StartSessionRequest' {ledgerName} -> ledgerName) (\s@StartSessionRequest' {} a -> s {ledgerName = a} :: StartSessionRequest)

instance Prelude.Hashable StartSessionRequest where
  hashWithSalt _salt StartSessionRequest' {..} =
    _salt `Prelude.hashWithSalt` ledgerName

instance Prelude.NFData StartSessionRequest where
  rnf StartSessionRequest' {..} = Prelude.rnf ledgerName

instance Data.ToJSON StartSessionRequest where
  toJSON StartSessionRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LedgerName" Data..= ledgerName)]
      )
