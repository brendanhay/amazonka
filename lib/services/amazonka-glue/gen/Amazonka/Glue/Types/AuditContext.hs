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
-- Module      : Amazonka.Glue.Types.AuditContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.AuditContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure containing the Lake Formation audit context.
--
-- /See:/ 'newAuditContext' smart constructor.
data AuditContext = AuditContext'
  { -- | A string containing the additional audit context information.
    additionalAuditContext :: Prelude.Maybe Prelude.Text,
    -- | All columns request for audit.
    allColumnsRequested :: Prelude.Maybe Prelude.Bool,
    -- | The requested columns for audit.
    requestedColumns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalAuditContext', 'auditContext_additionalAuditContext' - A string containing the additional audit context information.
--
-- 'allColumnsRequested', 'auditContext_allColumnsRequested' - All columns request for audit.
--
-- 'requestedColumns', 'auditContext_requestedColumns' - The requested columns for audit.
newAuditContext ::
  AuditContext
newAuditContext =
  AuditContext'
    { additionalAuditContext =
        Prelude.Nothing,
      allColumnsRequested = Prelude.Nothing,
      requestedColumns = Prelude.Nothing
    }

-- | A string containing the additional audit context information.
auditContext_additionalAuditContext :: Lens.Lens' AuditContext (Prelude.Maybe Prelude.Text)
auditContext_additionalAuditContext = Lens.lens (\AuditContext' {additionalAuditContext} -> additionalAuditContext) (\s@AuditContext' {} a -> s {additionalAuditContext = a} :: AuditContext)

-- | All columns request for audit.
auditContext_allColumnsRequested :: Lens.Lens' AuditContext (Prelude.Maybe Prelude.Bool)
auditContext_allColumnsRequested = Lens.lens (\AuditContext' {allColumnsRequested} -> allColumnsRequested) (\s@AuditContext' {} a -> s {allColumnsRequested = a} :: AuditContext)

-- | The requested columns for audit.
auditContext_requestedColumns :: Lens.Lens' AuditContext (Prelude.Maybe [Prelude.Text])
auditContext_requestedColumns = Lens.lens (\AuditContext' {requestedColumns} -> requestedColumns) (\s@AuditContext' {} a -> s {requestedColumns = a} :: AuditContext) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable AuditContext where
  hashWithSalt _salt AuditContext' {..} =
    _salt
      `Prelude.hashWithSalt` additionalAuditContext
      `Prelude.hashWithSalt` allColumnsRequested
      `Prelude.hashWithSalt` requestedColumns

instance Prelude.NFData AuditContext where
  rnf AuditContext' {..} =
    Prelude.rnf additionalAuditContext
      `Prelude.seq` Prelude.rnf allColumnsRequested
      `Prelude.seq` Prelude.rnf requestedColumns

instance Data.ToJSON AuditContext where
  toJSON AuditContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalAuditContext" Data..=)
              Prelude.<$> additionalAuditContext,
            ("AllColumnsRequested" Data..=)
              Prelude.<$> allColumnsRequested,
            ("RequestedColumns" Data..=)
              Prelude.<$> requestedColumns
          ]
      )
