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
-- Module      : Amazonka.IotTwinMaker.Types.EntityPropertyReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.EntityPropertyReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that uniquely identifies an entity property.
--
-- /See:/ 'newEntityPropertyReference' smart constructor.
data EntityPropertyReference = EntityPropertyReference'
  { -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the entity.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | A mapping of external IDs to property names. External IDs uniquely
    -- identify properties from external data stores.
    externalIdProperty :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the property.
    propertyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityPropertyReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentName', 'entityPropertyReference_componentName' - The name of the component.
--
-- 'entityId', 'entityPropertyReference_entityId' - The ID of the entity.
--
-- 'externalIdProperty', 'entityPropertyReference_externalIdProperty' - A mapping of external IDs to property names. External IDs uniquely
-- identify properties from external data stores.
--
-- 'propertyName', 'entityPropertyReference_propertyName' - The name of the property.
newEntityPropertyReference ::
  -- | 'propertyName'
  Prelude.Text ->
  EntityPropertyReference
newEntityPropertyReference pPropertyName_ =
  EntityPropertyReference'
    { componentName =
        Prelude.Nothing,
      entityId = Prelude.Nothing,
      externalIdProperty = Prelude.Nothing,
      propertyName = pPropertyName_
    }

-- | The name of the component.
entityPropertyReference_componentName :: Lens.Lens' EntityPropertyReference (Prelude.Maybe Prelude.Text)
entityPropertyReference_componentName = Lens.lens (\EntityPropertyReference' {componentName} -> componentName) (\s@EntityPropertyReference' {} a -> s {componentName = a} :: EntityPropertyReference)

-- | The ID of the entity.
entityPropertyReference_entityId :: Lens.Lens' EntityPropertyReference (Prelude.Maybe Prelude.Text)
entityPropertyReference_entityId = Lens.lens (\EntityPropertyReference' {entityId} -> entityId) (\s@EntityPropertyReference' {} a -> s {entityId = a} :: EntityPropertyReference)

-- | A mapping of external IDs to property names. External IDs uniquely
-- identify properties from external data stores.
entityPropertyReference_externalIdProperty :: Lens.Lens' EntityPropertyReference (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
entityPropertyReference_externalIdProperty = Lens.lens (\EntityPropertyReference' {externalIdProperty} -> externalIdProperty) (\s@EntityPropertyReference' {} a -> s {externalIdProperty = a} :: EntityPropertyReference) Prelude.. Lens.mapping Lens.coerced

-- | The name of the property.
entityPropertyReference_propertyName :: Lens.Lens' EntityPropertyReference Prelude.Text
entityPropertyReference_propertyName = Lens.lens (\EntityPropertyReference' {propertyName} -> propertyName) (\s@EntityPropertyReference' {} a -> s {propertyName = a} :: EntityPropertyReference)

instance Data.FromJSON EntityPropertyReference where
  parseJSON =
    Data.withObject
      "EntityPropertyReference"
      ( \x ->
          EntityPropertyReference'
            Prelude.<$> (x Data..:? "componentName")
            Prelude.<*> (x Data..:? "entityId")
            Prelude.<*> ( x
                            Data..:? "externalIdProperty"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "propertyName")
      )

instance Prelude.Hashable EntityPropertyReference where
  hashWithSalt _salt EntityPropertyReference' {..} =
    _salt
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` externalIdProperty
      `Prelude.hashWithSalt` propertyName

instance Prelude.NFData EntityPropertyReference where
  rnf EntityPropertyReference' {..} =
    Prelude.rnf componentName `Prelude.seq`
      Prelude.rnf entityId `Prelude.seq`
        Prelude.rnf externalIdProperty `Prelude.seq`
          Prelude.rnf propertyName

instance Data.ToJSON EntityPropertyReference where
  toJSON EntityPropertyReference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("componentName" Data..=) Prelude.<$> componentName,
            ("entityId" Data..=) Prelude.<$> entityId,
            ("externalIdProperty" Data..=)
              Prelude.<$> externalIdProperty,
            Prelude.Just ("propertyName" Data..= propertyName)
          ]
      )
