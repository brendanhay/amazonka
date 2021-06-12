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
-- Module      : Network.AWS.SSM.Types.ComplianceItemEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceItemEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.ComplianceSeverity
import Network.AWS.SSM.Types.ComplianceStatus

-- | Information about a compliance item.
--
-- /See:/ 'newComplianceItemEntry' smart constructor.
data ComplianceItemEntry = ComplianceItemEntry'
  { -- | The title of the compliance item. For example, if the compliance item is
    -- a Windows patch, the title could be the title of the KB article for the
    -- patch; for example: Security Update for Active Directory Federation
    -- Services.
    title :: Core.Maybe Core.Text,
    -- | The compliance item ID. For example, if the compliance item is a Windows
    -- patch, the ID could be the number of the KB article.
    id :: Core.Maybe Core.Text,
    -- | A \"Key\": \"Value\" tag combination for the compliance item.
    details :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The severity of the compliance status. Severity can be one of the
    -- following: Critical, High, Medium, Low, Informational, Unspecified.
    severity :: ComplianceSeverity,
    -- | The status of the compliance item. An item is either COMPLIANT or
    -- NON_COMPLIANT.
    status :: ComplianceStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ComplianceItemEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'title', 'complianceItemEntry_title' - The title of the compliance item. For example, if the compliance item is
-- a Windows patch, the title could be the title of the KB article for the
-- patch; for example: Security Update for Active Directory Federation
-- Services.
--
-- 'id', 'complianceItemEntry_id' - The compliance item ID. For example, if the compliance item is a Windows
-- patch, the ID could be the number of the KB article.
--
-- 'details', 'complianceItemEntry_details' - A \"Key\": \"Value\" tag combination for the compliance item.
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
    { title = Core.Nothing,
      id = Core.Nothing,
      details = Core.Nothing,
      severity = pSeverity_,
      status = pStatus_
    }

-- | The title of the compliance item. For example, if the compliance item is
-- a Windows patch, the title could be the title of the KB article for the
-- patch; for example: Security Update for Active Directory Federation
-- Services.
complianceItemEntry_title :: Lens.Lens' ComplianceItemEntry (Core.Maybe Core.Text)
complianceItemEntry_title = Lens.lens (\ComplianceItemEntry' {title} -> title) (\s@ComplianceItemEntry' {} a -> s {title = a} :: ComplianceItemEntry)

-- | The compliance item ID. For example, if the compliance item is a Windows
-- patch, the ID could be the number of the KB article.
complianceItemEntry_id :: Lens.Lens' ComplianceItemEntry (Core.Maybe Core.Text)
complianceItemEntry_id = Lens.lens (\ComplianceItemEntry' {id} -> id) (\s@ComplianceItemEntry' {} a -> s {id = a} :: ComplianceItemEntry)

-- | A \"Key\": \"Value\" tag combination for the compliance item.
complianceItemEntry_details :: Lens.Lens' ComplianceItemEntry (Core.Maybe (Core.HashMap Core.Text Core.Text))
complianceItemEntry_details = Lens.lens (\ComplianceItemEntry' {details} -> details) (\s@ComplianceItemEntry' {} a -> s {details = a} :: ComplianceItemEntry) Core.. Lens.mapping Lens._Coerce

-- | The severity of the compliance status. Severity can be one of the
-- following: Critical, High, Medium, Low, Informational, Unspecified.
complianceItemEntry_severity :: Lens.Lens' ComplianceItemEntry ComplianceSeverity
complianceItemEntry_severity = Lens.lens (\ComplianceItemEntry' {severity} -> severity) (\s@ComplianceItemEntry' {} a -> s {severity = a} :: ComplianceItemEntry)

-- | The status of the compliance item. An item is either COMPLIANT or
-- NON_COMPLIANT.
complianceItemEntry_status :: Lens.Lens' ComplianceItemEntry ComplianceStatus
complianceItemEntry_status = Lens.lens (\ComplianceItemEntry' {status} -> status) (\s@ComplianceItemEntry' {} a -> s {status = a} :: ComplianceItemEntry)

instance Core.Hashable ComplianceItemEntry

instance Core.NFData ComplianceItemEntry

instance Core.ToJSON ComplianceItemEntry where
  toJSON ComplianceItemEntry' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Title" Core..=) Core.<$> title,
            ("Id" Core..=) Core.<$> id,
            ("Details" Core..=) Core.<$> details,
            Core.Just ("Severity" Core..= severity),
            Core.Just ("Status" Core..= status)
          ]
      )
