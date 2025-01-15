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
-- Module      : Amazonka.ServiceCatalog.Types.ResourceTargetDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ResourceTargetDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.RequiresRecreation
import Amazonka.ServiceCatalog.Types.ResourceAttribute

-- | Information about a change to a resource attribute.
--
-- /See:/ 'newResourceTargetDefinition' smart constructor.
data ResourceTargetDefinition = ResourceTargetDefinition'
  { -- | The attribute to be changed.
    attribute :: Prelude.Maybe ResourceAttribute,
    -- | If the attribute is @Properties@, the value is the name of the property.
    -- Otherwise, the value is null.
    name :: Prelude.Maybe Prelude.Text,
    -- | If the attribute is @Properties@, indicates whether a change to this
    -- property causes the resource to be re-created.
    requiresRecreation :: Prelude.Maybe RequiresRecreation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceTargetDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'resourceTargetDefinition_attribute' - The attribute to be changed.
--
-- 'name', 'resourceTargetDefinition_name' - If the attribute is @Properties@, the value is the name of the property.
-- Otherwise, the value is null.
--
-- 'requiresRecreation', 'resourceTargetDefinition_requiresRecreation' - If the attribute is @Properties@, indicates whether a change to this
-- property causes the resource to be re-created.
newResourceTargetDefinition ::
  ResourceTargetDefinition
newResourceTargetDefinition =
  ResourceTargetDefinition'
    { attribute =
        Prelude.Nothing,
      name = Prelude.Nothing,
      requiresRecreation = Prelude.Nothing
    }

-- | The attribute to be changed.
resourceTargetDefinition_attribute :: Lens.Lens' ResourceTargetDefinition (Prelude.Maybe ResourceAttribute)
resourceTargetDefinition_attribute = Lens.lens (\ResourceTargetDefinition' {attribute} -> attribute) (\s@ResourceTargetDefinition' {} a -> s {attribute = a} :: ResourceTargetDefinition)

-- | If the attribute is @Properties@, the value is the name of the property.
-- Otherwise, the value is null.
resourceTargetDefinition_name :: Lens.Lens' ResourceTargetDefinition (Prelude.Maybe Prelude.Text)
resourceTargetDefinition_name = Lens.lens (\ResourceTargetDefinition' {name} -> name) (\s@ResourceTargetDefinition' {} a -> s {name = a} :: ResourceTargetDefinition)

-- | If the attribute is @Properties@, indicates whether a change to this
-- property causes the resource to be re-created.
resourceTargetDefinition_requiresRecreation :: Lens.Lens' ResourceTargetDefinition (Prelude.Maybe RequiresRecreation)
resourceTargetDefinition_requiresRecreation = Lens.lens (\ResourceTargetDefinition' {requiresRecreation} -> requiresRecreation) (\s@ResourceTargetDefinition' {} a -> s {requiresRecreation = a} :: ResourceTargetDefinition)

instance Data.FromJSON ResourceTargetDefinition where
  parseJSON =
    Data.withObject
      "ResourceTargetDefinition"
      ( \x ->
          ResourceTargetDefinition'
            Prelude.<$> (x Data..:? "Attribute")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RequiresRecreation")
      )

instance Prelude.Hashable ResourceTargetDefinition where
  hashWithSalt _salt ResourceTargetDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` requiresRecreation

instance Prelude.NFData ResourceTargetDefinition where
  rnf ResourceTargetDefinition' {..} =
    Prelude.rnf attribute `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf requiresRecreation
