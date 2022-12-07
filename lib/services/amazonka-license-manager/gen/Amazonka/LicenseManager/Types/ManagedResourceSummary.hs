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
-- Module      : Amazonka.LicenseManager.Types.ManagedResourceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ManagedResourceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a managed resource.
--
-- /See:/ 'newManagedResourceSummary' smart constructor.
data ManagedResourceSummary = ManagedResourceSummary'
  { -- | Type of resource associated with a license.
    resourceType :: Prelude.Maybe ResourceType,
    -- | Number of resources associated with licenses.
    associationCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedResourceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'managedResourceSummary_resourceType' - Type of resource associated with a license.
--
-- 'associationCount', 'managedResourceSummary_associationCount' - Number of resources associated with licenses.
newManagedResourceSummary ::
  ManagedResourceSummary
newManagedResourceSummary =
  ManagedResourceSummary'
    { resourceType =
        Prelude.Nothing,
      associationCount = Prelude.Nothing
    }

-- | Type of resource associated with a license.
managedResourceSummary_resourceType :: Lens.Lens' ManagedResourceSummary (Prelude.Maybe ResourceType)
managedResourceSummary_resourceType = Lens.lens (\ManagedResourceSummary' {resourceType} -> resourceType) (\s@ManagedResourceSummary' {} a -> s {resourceType = a} :: ManagedResourceSummary)

-- | Number of resources associated with licenses.
managedResourceSummary_associationCount :: Lens.Lens' ManagedResourceSummary (Prelude.Maybe Prelude.Integer)
managedResourceSummary_associationCount = Lens.lens (\ManagedResourceSummary' {associationCount} -> associationCount) (\s@ManagedResourceSummary' {} a -> s {associationCount = a} :: ManagedResourceSummary)

instance Data.FromJSON ManagedResourceSummary where
  parseJSON =
    Data.withObject
      "ManagedResourceSummary"
      ( \x ->
          ManagedResourceSummary'
            Prelude.<$> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "AssociationCount")
      )

instance Prelude.Hashable ManagedResourceSummary where
  hashWithSalt _salt ManagedResourceSummary' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` associationCount

instance Prelude.NFData ManagedResourceSummary where
  rnf ManagedResourceSummary' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf associationCount
