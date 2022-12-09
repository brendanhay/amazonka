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
-- Module      : Amazonka.SSM.Types.ComplianceItemEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ComplianceItemEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.ComplianceSeverity
import Amazonka.SSM.Types.ComplianceStatus

-- | Information about a compliance item.
--
-- /See:/ 'newComplianceItemEntry' smart constructor.
data ComplianceItemEntry = ComplianceItemEntry'
  { -- | A \"Key\": \"Value\" tag combination for the compliance item.
    details :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The compliance item ID. For example, if the compliance item is a Windows
    -- patch, the ID could be the number of the KB article.
    id :: Prelude.Maybe Prelude.Text,
    -- | The title of the compliance item. For example, if the compliance item is
    -- a Windows patch, the title could be the title of the KB article for the
    -- patch; for example: Security Update for Active Directory Federation
    -- Services.
    title :: Prelude.Maybe Prelude.Text,
    -- | The severity of the compliance status. Severity can be one of the
    -- following: Critical, High, Medium, Low, Informational, Unspecified.
    severity :: ComplianceSeverity,
    -- | The status of the compliance item. An item is either COMPLIANT or
    -- NON_COMPLIANT.
    status :: ComplianceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComplianceItemEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'complianceItemEntry_details' - A \"Key\": \"Value\" tag combination for the compliance item.
--
-- 'id', 'complianceItemEntry_id' - The compliance item ID. For example, if the compliance item is a Windows
-- patch, the ID could be the number of the KB article.
--
-- 'title', 'complianceItemEntry_title' - The title of the compliance item. For example, if the compliance item is
-- a Windows patch, the title could be the title of the KB article for the
-- patch; for example: Security Update for Active Directory Federation
-- Services.
--
-- 'severity', 'complianceItemEntry_severity' - The severity of the compliance status. Severity can be one of the
-- following: Critical, High, Medium, Low, Informational, Unspecified.
--
-- 'status', 'complianceItemEntry_status' - The status of the compliance item. An item is either COMPLIANT or
-- NON_COMPLIANT.
newComplianceItemEntry ::
  -- | 'severity'
  ComplianceSeverity ->
  -- | 'status'
  ComplianceStatus ->
  ComplianceItemEntry
newComplianceItemEntry pSeverity_ pStatus_ =
  ComplianceItemEntry'
    { details = Prelude.Nothing,
      id = Prelude.Nothing,
      title = Prelude.Nothing,
      severity = pSeverity_,
      status = pStatus_
    }

-- | A \"Key\": \"Value\" tag combination for the compliance item.
complianceItemEntry_details :: Lens.Lens' ComplianceItemEntry (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
complianceItemEntry_details = Lens.lens (\ComplianceItemEntry' {details} -> details) (\s@ComplianceItemEntry' {} a -> s {details = a} :: ComplianceItemEntry) Prelude.. Lens.mapping Lens.coerced

-- | The compliance item ID. For example, if the compliance item is a Windows
-- patch, the ID could be the number of the KB article.
complianceItemEntry_id :: Lens.Lens' ComplianceItemEntry (Prelude.Maybe Prelude.Text)
complianceItemEntry_id = Lens.lens (\ComplianceItemEntry' {id} -> id) (\s@ComplianceItemEntry' {} a -> s {id = a} :: ComplianceItemEntry)

-- | The title of the compliance item. For example, if the compliance item is
-- a Windows patch, the title could be the title of the KB article for the
-- patch; for example: Security Update for Active Directory Federation
-- Services.
complianceItemEntry_title :: Lens.Lens' ComplianceItemEntry (Prelude.Maybe Prelude.Text)
complianceItemEntry_title = Lens.lens (\ComplianceItemEntry' {title} -> title) (\s@ComplianceItemEntry' {} a -> s {title = a} :: ComplianceItemEntry)

-- | The severity of the compliance status. Severity can be one of the
-- following: Critical, High, Medium, Low, Informational, Unspecified.
complianceItemEntry_severity :: Lens.Lens' ComplianceItemEntry ComplianceSeverity
complianceItemEntry_severity = Lens.lens (\ComplianceItemEntry' {severity} -> severity) (\s@ComplianceItemEntry' {} a -> s {severity = a} :: ComplianceItemEntry)

-- | The status of the compliance item. An item is either COMPLIANT or
-- NON_COMPLIANT.
complianceItemEntry_status :: Lens.Lens' ComplianceItemEntry ComplianceStatus
complianceItemEntry_status = Lens.lens (\ComplianceItemEntry' {status} -> status) (\s@ComplianceItemEntry' {} a -> s {status = a} :: ComplianceItemEntry)

instance Prelude.Hashable ComplianceItemEntry where
  hashWithSalt _salt ComplianceItemEntry' {..} =
    _salt `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` status

instance Prelude.NFData ComplianceItemEntry where
  rnf ComplianceItemEntry' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON ComplianceItemEntry where
  toJSON ComplianceItemEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Details" Data..=) Prelude.<$> details,
            ("Id" Data..=) Prelude.<$> id,
            ("Title" Data..=) Prelude.<$> title,
            Prelude.Just ("Severity" Data..= severity),
            Prelude.Just ("Status" Data..= status)
          ]
      )
