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
-- Module      : Amazonka.IoTSiteWise.Types.AssetRelationshipSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetRelationshipSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.AssetHierarchyInfo
import Amazonka.IoTSiteWise.Types.AssetRelationshipType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about assets that are related to one another.
--
-- /See:/ 'newAssetRelationshipSummary' smart constructor.
data AssetRelationshipSummary = AssetRelationshipSummary'
  { -- | The assets that are related through an asset hierarchy.
    --
    -- This object is present if the @relationshipType@ is @HIERARCHY@.
    hierarchyInfo :: Prelude.Maybe AssetHierarchyInfo,
    -- | The relationship type of the assets in this relationship. This value is
    -- one of the following:
    --
    -- -   @HIERARCHY@ – The assets are related through an asset hierarchy. If
    --     you specify this relationship type, this asset relationship includes
    --     the @hierarchyInfo@ object.
    relationshipType :: AssetRelationshipType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetRelationshipSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyInfo', 'assetRelationshipSummary_hierarchyInfo' - The assets that are related through an asset hierarchy.
--
-- This object is present if the @relationshipType@ is @HIERARCHY@.
--
-- 'relationshipType', 'assetRelationshipSummary_relationshipType' - The relationship type of the assets in this relationship. This value is
-- one of the following:
--
-- -   @HIERARCHY@ – The assets are related through an asset hierarchy. If
--     you specify this relationship type, this asset relationship includes
--     the @hierarchyInfo@ object.
newAssetRelationshipSummary ::
  -- | 'relationshipType'
  AssetRelationshipType ->
  AssetRelationshipSummary
newAssetRelationshipSummary pRelationshipType_ =
  AssetRelationshipSummary'
    { hierarchyInfo =
        Prelude.Nothing,
      relationshipType = pRelationshipType_
    }

-- | The assets that are related through an asset hierarchy.
--
-- This object is present if the @relationshipType@ is @HIERARCHY@.
assetRelationshipSummary_hierarchyInfo :: Lens.Lens' AssetRelationshipSummary (Prelude.Maybe AssetHierarchyInfo)
assetRelationshipSummary_hierarchyInfo = Lens.lens (\AssetRelationshipSummary' {hierarchyInfo} -> hierarchyInfo) (\s@AssetRelationshipSummary' {} a -> s {hierarchyInfo = a} :: AssetRelationshipSummary)

-- | The relationship type of the assets in this relationship. This value is
-- one of the following:
--
-- -   @HIERARCHY@ – The assets are related through an asset hierarchy. If
--     you specify this relationship type, this asset relationship includes
--     the @hierarchyInfo@ object.
assetRelationshipSummary_relationshipType :: Lens.Lens' AssetRelationshipSummary AssetRelationshipType
assetRelationshipSummary_relationshipType = Lens.lens (\AssetRelationshipSummary' {relationshipType} -> relationshipType) (\s@AssetRelationshipSummary' {} a -> s {relationshipType = a} :: AssetRelationshipSummary)

instance Data.FromJSON AssetRelationshipSummary where
  parseJSON =
    Data.withObject
      "AssetRelationshipSummary"
      ( \x ->
          AssetRelationshipSummary'
            Prelude.<$> (x Data..:? "hierarchyInfo")
            Prelude.<*> (x Data..: "relationshipType")
      )

instance Prelude.Hashable AssetRelationshipSummary where
  hashWithSalt _salt AssetRelationshipSummary' {..} =
    _salt
      `Prelude.hashWithSalt` hierarchyInfo
      `Prelude.hashWithSalt` relationshipType

instance Prelude.NFData AssetRelationshipSummary where
  rnf AssetRelationshipSummary' {..} =
    Prelude.rnf hierarchyInfo
      `Prelude.seq` Prelude.rnf relationshipType
