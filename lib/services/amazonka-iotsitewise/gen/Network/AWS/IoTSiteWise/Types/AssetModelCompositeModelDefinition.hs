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
-- Module      : Network.AWS.IoTSiteWise.Types.AssetModelCompositeModelDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSiteWise.Types.AssetModelCompositeModelDefinition where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTSiteWise.Types.AssetModelPropertyDefinition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains a composite model definition in an asset model. This composite
-- model definition is applied to all assets created from the asset model.
--
-- /See:/ 'newAssetModelCompositeModelDefinition' smart constructor.
data AssetModelCompositeModelDefinition = AssetModelCompositeModelDefinition'
  { -- | The description of the composite model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The asset property definitions for this composite model.
    properties :: Prelude.Maybe [AssetModelPropertyDefinition],
    -- | The name of the composite model.
    name :: Prelude.Text,
    -- | The type of the composite model. For alarm composite models, this type
    -- is @AWS\/ALARM@.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetModelCompositeModelDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'assetModelCompositeModelDefinition_description' - The description of the composite model.
--
-- 'properties', 'assetModelCompositeModelDefinition_properties' - The asset property definitions for this composite model.
--
-- 'name', 'assetModelCompositeModelDefinition_name' - The name of the composite model.
--
-- 'type'', 'assetModelCompositeModelDefinition_type' - The type of the composite model. For alarm composite models, this type
-- is @AWS\/ALARM@.
newAssetModelCompositeModelDefinition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  AssetModelCompositeModelDefinition
newAssetModelCompositeModelDefinition pName_ pType_ =
  AssetModelCompositeModelDefinition'
    { description =
        Prelude.Nothing,
      properties = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | The description of the composite model.
assetModelCompositeModelDefinition_description :: Lens.Lens' AssetModelCompositeModelDefinition (Prelude.Maybe Prelude.Text)
assetModelCompositeModelDefinition_description = Lens.lens (\AssetModelCompositeModelDefinition' {description} -> description) (\s@AssetModelCompositeModelDefinition' {} a -> s {description = a} :: AssetModelCompositeModelDefinition)

-- | The asset property definitions for this composite model.
assetModelCompositeModelDefinition_properties :: Lens.Lens' AssetModelCompositeModelDefinition (Prelude.Maybe [AssetModelPropertyDefinition])
assetModelCompositeModelDefinition_properties = Lens.lens (\AssetModelCompositeModelDefinition' {properties} -> properties) (\s@AssetModelCompositeModelDefinition' {} a -> s {properties = a} :: AssetModelCompositeModelDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The name of the composite model.
assetModelCompositeModelDefinition_name :: Lens.Lens' AssetModelCompositeModelDefinition Prelude.Text
assetModelCompositeModelDefinition_name = Lens.lens (\AssetModelCompositeModelDefinition' {name} -> name) (\s@AssetModelCompositeModelDefinition' {} a -> s {name = a} :: AssetModelCompositeModelDefinition)

-- | The type of the composite model. For alarm composite models, this type
-- is @AWS\/ALARM@.
assetModelCompositeModelDefinition_type :: Lens.Lens' AssetModelCompositeModelDefinition Prelude.Text
assetModelCompositeModelDefinition_type = Lens.lens (\AssetModelCompositeModelDefinition' {type'} -> type') (\s@AssetModelCompositeModelDefinition' {} a -> s {type' = a} :: AssetModelCompositeModelDefinition)

instance
  Prelude.Hashable
    AssetModelCompositeModelDefinition

instance
  Prelude.NFData
    AssetModelCompositeModelDefinition

instance
  Core.ToJSON
    AssetModelCompositeModelDefinition
  where
  toJSON AssetModelCompositeModelDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            ("properties" Core..=) Prelude.<$> properties,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("type" Core..= type')
          ]
      )
