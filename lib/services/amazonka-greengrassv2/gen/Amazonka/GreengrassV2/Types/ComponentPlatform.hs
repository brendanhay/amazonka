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
-- Module      : Amazonka.GreengrassV2.Types.ComponentPlatform
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.ComponentPlatform where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a platform that a component supports.
--
-- /See:/ 'newComponentPlatform' smart constructor.
data ComponentPlatform = ComponentPlatform'
  { -- | A dictionary of attributes for the platform. The IoT Greengrass Core
    -- software defines the @os@ and @architecture@ by default. You can specify
    -- additional platform attributes for a core device when you deploy the
    -- Greengrass nucleus component. For more information, see the
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-nucleus-component.html Greengrass nucleus component>
    -- in the /IoT Greengrass V2 Developer Guide/.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The friendly name of the platform. This name helps you identify the
    -- platform.
    --
    -- If you omit this parameter, IoT Greengrass creates a friendly name from
    -- the @os@ and @architecture@ of the platform.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentPlatform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'componentPlatform_attributes' - A dictionary of attributes for the platform. The IoT Greengrass Core
-- software defines the @os@ and @architecture@ by default. You can specify
-- additional platform attributes for a core device when you deploy the
-- Greengrass nucleus component. For more information, see the
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-nucleus-component.html Greengrass nucleus component>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- 'name', 'componentPlatform_name' - The friendly name of the platform. This name helps you identify the
-- platform.
--
-- If you omit this parameter, IoT Greengrass creates a friendly name from
-- the @os@ and @architecture@ of the platform.
newComponentPlatform ::
  ComponentPlatform
newComponentPlatform =
  ComponentPlatform'
    { attributes = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | A dictionary of attributes for the platform. The IoT Greengrass Core
-- software defines the @os@ and @architecture@ by default. You can specify
-- additional platform attributes for a core device when you deploy the
-- Greengrass nucleus component. For more information, see the
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-nucleus-component.html Greengrass nucleus component>
-- in the /IoT Greengrass V2 Developer Guide/.
componentPlatform_attributes :: Lens.Lens' ComponentPlatform (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
componentPlatform_attributes = Lens.lens (\ComponentPlatform' {attributes} -> attributes) (\s@ComponentPlatform' {} a -> s {attributes = a} :: ComponentPlatform) Prelude.. Lens.mapping Lens.coerced

-- | The friendly name of the platform. This name helps you identify the
-- platform.
--
-- If you omit this parameter, IoT Greengrass creates a friendly name from
-- the @os@ and @architecture@ of the platform.
componentPlatform_name :: Lens.Lens' ComponentPlatform (Prelude.Maybe Prelude.Text)
componentPlatform_name = Lens.lens (\ComponentPlatform' {name} -> name) (\s@ComponentPlatform' {} a -> s {name = a} :: ComponentPlatform)

instance Data.FromJSON ComponentPlatform where
  parseJSON =
    Data.withObject
      "ComponentPlatform"
      ( \x ->
          ComponentPlatform'
            Prelude.<$> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable ComponentPlatform where
  hashWithSalt _salt ComponentPlatform' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` name

instance Prelude.NFData ComponentPlatform where
  rnf ComponentPlatform' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON ComponentPlatform where
  toJSON ComponentPlatform' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("name" Data..=) Prelude.<$> name
          ]
      )
