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
-- Module      : Amazonka.LakeFormation.Types.AuditContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.AuditContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure used to include auditing information on the privileged API.
--
-- /See:/ 'newAuditContext' smart constructor.
data AuditContext = AuditContext'
  { -- | The filter engine can populate the \'AdditionalAuditContext\'
    -- information with the request ID for you to track. This information will
    -- be displayed in CloudTrail log in your account.
    additionalAuditContext :: Prelude.Maybe Prelude.Text
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
-- 'additionalAuditContext', 'auditContext_additionalAuditContext' - The filter engine can populate the \'AdditionalAuditContext\'
-- information with the request ID for you to track. This information will
-- be displayed in CloudTrail log in your account.
newAuditContext ::
  AuditContext
newAuditContext =
  AuditContext'
    { additionalAuditContext =
        Prelude.Nothing
    }

-- | The filter engine can populate the \'AdditionalAuditContext\'
-- information with the request ID for you to track. This information will
-- be displayed in CloudTrail log in your account.
auditContext_additionalAuditContext :: Lens.Lens' AuditContext (Prelude.Maybe Prelude.Text)
auditContext_additionalAuditContext = Lens.lens (\AuditContext' {additionalAuditContext} -> additionalAuditContext) (\s@AuditContext' {} a -> s {additionalAuditContext = a} :: AuditContext)

instance Prelude.Hashable AuditContext where
  hashWithSalt _salt AuditContext' {..} =
    _salt `Prelude.hashWithSalt` additionalAuditContext

instance Prelude.NFData AuditContext where
  rnf AuditContext' {..} =
    Prelude.rnf additionalAuditContext

instance Data.ToJSON AuditContext where
  toJSON AuditContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalAuditContext" Data..=)
              Prelude.<$> additionalAuditContext
          ]
      )
