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
-- Module      : Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information that shows whether a resource is compliant with the
-- effective tag policy, including details on any noncompliant tag keys.
--
-- /See:/ 'newComplianceDetails' smart constructor.
data ComplianceDetails = ComplianceDetails'
  { -- | Whether a resource is compliant with the effective tag policy.
    complianceStatus :: Core.Maybe Core.Bool,
    -- | These tag keys on the resource are noncompliant with the effective tag
    -- policy.
    noncompliantKeys :: Core.Maybe [Core.Text],
    -- | These are keys defined in the effective policy that are on the resource
    -- with either incorrect case treatment or noncompliant values.
    keysWithNoncompliantValues :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ComplianceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceStatus', 'complianceDetails_complianceStatus' - Whether a resource is compliant with the effective tag policy.
--
-- 'noncompliantKeys', 'complianceDetails_noncompliantKeys' - These tag keys on the resource are noncompliant with the effective tag
-- policy.
--
-- 'keysWithNoncompliantValues', 'complianceDetails_keysWithNoncompliantValues' - These are keys defined in the effective policy that are on the resource
-- with either incorrect case treatment or noncompliant values.
newComplianceDetails ::
  ComplianceDetails
newComplianceDetails =
  ComplianceDetails'
    { complianceStatus = Core.Nothing,
      noncompliantKeys = Core.Nothing,
      keysWithNoncompliantValues = Core.Nothing
    }

-- | Whether a resource is compliant with the effective tag policy.
complianceDetails_complianceStatus :: Lens.Lens' ComplianceDetails (Core.Maybe Core.Bool)
complianceDetails_complianceStatus = Lens.lens (\ComplianceDetails' {complianceStatus} -> complianceStatus) (\s@ComplianceDetails' {} a -> s {complianceStatus = a} :: ComplianceDetails)

-- | These tag keys on the resource are noncompliant with the effective tag
-- policy.
complianceDetails_noncompliantKeys :: Lens.Lens' ComplianceDetails (Core.Maybe [Core.Text])
complianceDetails_noncompliantKeys = Lens.lens (\ComplianceDetails' {noncompliantKeys} -> noncompliantKeys) (\s@ComplianceDetails' {} a -> s {noncompliantKeys = a} :: ComplianceDetails) Core.. Lens.mapping Lens._Coerce

-- | These are keys defined in the effective policy that are on the resource
-- with either incorrect case treatment or noncompliant values.
complianceDetails_keysWithNoncompliantValues :: Lens.Lens' ComplianceDetails (Core.Maybe [Core.Text])
complianceDetails_keysWithNoncompliantValues = Lens.lens (\ComplianceDetails' {keysWithNoncompliantValues} -> keysWithNoncompliantValues) (\s@ComplianceDetails' {} a -> s {keysWithNoncompliantValues = a} :: ComplianceDetails) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ComplianceDetails where
  parseJSON =
    Core.withObject
      "ComplianceDetails"
      ( \x ->
          ComplianceDetails'
            Core.<$> (x Core..:? "ComplianceStatus")
            Core.<*> (x Core..:? "NoncompliantKeys" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "KeysWithNoncompliantValues"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable ComplianceDetails

instance Core.NFData ComplianceDetails
