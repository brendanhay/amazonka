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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.AuditContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure containing information for audit.
--
-- /See:/ 'newAuditContext' smart constructor.
data AuditContext = AuditContext'
  { -- | The requested columns for audit.
    requestedColumns :: Prelude.Maybe [Prelude.Text],
    -- | The context for the audit..
    additionalAuditContext :: Prelude.Maybe Prelude.Text,
    -- | All columns request for audit.
    allColumnsRequested :: Prelude.Maybe Prelude.Bool
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
-- 'requestedColumns', 'auditContext_requestedColumns' - The requested columns for audit.
--
-- 'additionalAuditContext', 'auditContext_additionalAuditContext' - The context for the audit..
--
-- 'allColumnsRequested', 'auditContext_allColumnsRequested' - All columns request for audit.
newAuditContext ::
  AuditContext
newAuditContext =
  AuditContext'
    { requestedColumns = Prelude.Nothing,
      additionalAuditContext = Prelude.Nothing,
      allColumnsRequested = Prelude.Nothing
    }

-- | The requested columns for audit.
auditContext_requestedColumns :: Lens.Lens' AuditContext (Prelude.Maybe [Prelude.Text])
auditContext_requestedColumns = Lens.lens (\AuditContext' {requestedColumns} -> requestedColumns) (\s@AuditContext' {} a -> s {requestedColumns = a} :: AuditContext) Prelude.. Lens.mapping Lens.coerced

-- | The context for the audit..
auditContext_additionalAuditContext :: Lens.Lens' AuditContext (Prelude.Maybe Prelude.Text)
auditContext_additionalAuditContext = Lens.lens (\AuditContext' {additionalAuditContext} -> additionalAuditContext) (\s@AuditContext' {} a -> s {additionalAuditContext = a} :: AuditContext)

-- | All columns request for audit.
auditContext_allColumnsRequested :: Lens.Lens' AuditContext (Prelude.Maybe Prelude.Bool)
auditContext_allColumnsRequested = Lens.lens (\AuditContext' {allColumnsRequested} -> allColumnsRequested) (\s@AuditContext' {} a -> s {allColumnsRequested = a} :: AuditContext)

instance Prelude.Hashable AuditContext where
  hashWithSalt _salt AuditContext' {..} =
    _salt `Prelude.hashWithSalt` requestedColumns
      `Prelude.hashWithSalt` additionalAuditContext
      `Prelude.hashWithSalt` allColumnsRequested

instance Prelude.NFData AuditContext where
  rnf AuditContext' {..} =
    Prelude.rnf requestedColumns
      `Prelude.seq` Prelude.rnf additionalAuditContext
      `Prelude.seq` Prelude.rnf allColumnsRequested

instance Core.ToJSON AuditContext where
  toJSON AuditContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RequestedColumns" Core..=)
              Prelude.<$> requestedColumns,
            ("AdditionalAuditContext" Core..=)
              Prelude.<$> additionalAuditContext,
            ("AllColumnsRequested" Core..=)
              Prelude.<$> allColumnsRequested
          ]
      )
