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
-- Module      : Amazonka.AppFlow.Types.CustomConnectorSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.CustomConnectorSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when the custom connector is being used
-- as a source.
--
-- /See:/ 'newCustomConnectorSourceProperties' smart constructor.
data CustomConnectorSourceProperties = CustomConnectorSourceProperties'
  { -- | Custom properties that are required to use the custom connector as a
    -- source.
    customProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The entity specified in the custom connector as a source in the flow.
    entityName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomConnectorSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customProperties', 'customConnectorSourceProperties_customProperties' - Custom properties that are required to use the custom connector as a
-- source.
--
-- 'entityName', 'customConnectorSourceProperties_entityName' - The entity specified in the custom connector as a source in the flow.
newCustomConnectorSourceProperties ::
  -- | 'entityName'
  Prelude.Text ->
  CustomConnectorSourceProperties
newCustomConnectorSourceProperties pEntityName_ =
  CustomConnectorSourceProperties'
    { customProperties =
        Prelude.Nothing,
      entityName = pEntityName_
    }

-- | Custom properties that are required to use the custom connector as a
-- source.
customConnectorSourceProperties_customProperties :: Lens.Lens' CustomConnectorSourceProperties (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
customConnectorSourceProperties_customProperties = Lens.lens (\CustomConnectorSourceProperties' {customProperties} -> customProperties) (\s@CustomConnectorSourceProperties' {} a -> s {customProperties = a} :: CustomConnectorSourceProperties) Prelude.. Lens.mapping Lens.coerced

-- | The entity specified in the custom connector as a source in the flow.
customConnectorSourceProperties_entityName :: Lens.Lens' CustomConnectorSourceProperties Prelude.Text
customConnectorSourceProperties_entityName = Lens.lens (\CustomConnectorSourceProperties' {entityName} -> entityName) (\s@CustomConnectorSourceProperties' {} a -> s {entityName = a} :: CustomConnectorSourceProperties)

instance
  Core.FromJSON
    CustomConnectorSourceProperties
  where
  parseJSON =
    Core.withObject
      "CustomConnectorSourceProperties"
      ( \x ->
          CustomConnectorSourceProperties'
            Prelude.<$> ( x Core..:? "customProperties"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "entityName")
      )

instance
  Prelude.Hashable
    CustomConnectorSourceProperties
  where
  hashWithSalt
    _salt
    CustomConnectorSourceProperties' {..} =
      _salt `Prelude.hashWithSalt` customProperties
        `Prelude.hashWithSalt` entityName

instance
  Prelude.NFData
    CustomConnectorSourceProperties
  where
  rnf CustomConnectorSourceProperties' {..} =
    Prelude.rnf customProperties
      `Prelude.seq` Prelude.rnf entityName

instance Core.ToJSON CustomConnectorSourceProperties where
  toJSON CustomConnectorSourceProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("customProperties" Core..=)
              Prelude.<$> customProperties,
            Prelude.Just ("entityName" Core..= entityName)
          ]
      )
