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
-- Module      : Amazonka.IoTSiteWise.Types.CompositeModelProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.CompositeModelProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.Property
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a composite model property on an asset.
--
-- /See:/ 'newCompositeModelProperty' smart constructor.
data CompositeModelProperty = CompositeModelProperty'
  { -- | The ID of the composite model that contains the property.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the property.
    name :: Prelude.Text,
    -- | The type of the composite model that defines this property.
    type' :: Prelude.Text,
    assetProperty :: Property
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompositeModelProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'compositeModelProperty_id' - The ID of the composite model that contains the property.
--
-- 'name', 'compositeModelProperty_name' - The name of the property.
--
-- 'type'', 'compositeModelProperty_type' - The type of the composite model that defines this property.
--
-- 'assetProperty', 'compositeModelProperty_assetProperty' - Undocumented member.
newCompositeModelProperty ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  -- | 'assetProperty'
  Property ->
  CompositeModelProperty
newCompositeModelProperty
  pName_
  pType_
  pAssetProperty_ =
    CompositeModelProperty'
      { id = Prelude.Nothing,
        name = pName_,
        type' = pType_,
        assetProperty = pAssetProperty_
      }

-- | The ID of the composite model that contains the property.
compositeModelProperty_id :: Lens.Lens' CompositeModelProperty (Prelude.Maybe Prelude.Text)
compositeModelProperty_id = Lens.lens (\CompositeModelProperty' {id} -> id) (\s@CompositeModelProperty' {} a -> s {id = a} :: CompositeModelProperty)

-- | The name of the property.
compositeModelProperty_name :: Lens.Lens' CompositeModelProperty Prelude.Text
compositeModelProperty_name = Lens.lens (\CompositeModelProperty' {name} -> name) (\s@CompositeModelProperty' {} a -> s {name = a} :: CompositeModelProperty)

-- | The type of the composite model that defines this property.
compositeModelProperty_type :: Lens.Lens' CompositeModelProperty Prelude.Text
compositeModelProperty_type = Lens.lens (\CompositeModelProperty' {type'} -> type') (\s@CompositeModelProperty' {} a -> s {type' = a} :: CompositeModelProperty)

-- | Undocumented member.
compositeModelProperty_assetProperty :: Lens.Lens' CompositeModelProperty Property
compositeModelProperty_assetProperty = Lens.lens (\CompositeModelProperty' {assetProperty} -> assetProperty) (\s@CompositeModelProperty' {} a -> s {assetProperty = a} :: CompositeModelProperty)

instance Data.FromJSON CompositeModelProperty where
  parseJSON =
    Data.withObject
      "CompositeModelProperty"
      ( \x ->
          CompositeModelProperty'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "assetProperty")
      )

instance Prelude.Hashable CompositeModelProperty where
  hashWithSalt _salt CompositeModelProperty' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` assetProperty

instance Prelude.NFData CompositeModelProperty where
  rnf CompositeModelProperty' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf assetProperty
