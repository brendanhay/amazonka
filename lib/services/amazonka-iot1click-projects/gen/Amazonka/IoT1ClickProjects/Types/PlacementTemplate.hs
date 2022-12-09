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
-- Module      : Amazonka.IoT1ClickProjects.Types.PlacementTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickProjects.Types.PlacementTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT1ClickProjects.Types.DeviceTemplate
import qualified Amazonka.Prelude as Prelude

-- | An object defining the template for a placement.
--
-- /See:/ 'newPlacementTemplate' smart constructor.
data PlacementTemplate = PlacementTemplate'
  { -- | The default attributes (key\/value pairs) to be applied to all
    -- placements using this template.
    defaultAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An object specifying the DeviceTemplate for all placements using this
    -- (PlacementTemplate) template.
    deviceTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text DeviceTemplate)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlacementTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultAttributes', 'placementTemplate_defaultAttributes' - The default attributes (key\/value pairs) to be applied to all
-- placements using this template.
--
-- 'deviceTemplates', 'placementTemplate_deviceTemplates' - An object specifying the DeviceTemplate for all placements using this
-- (PlacementTemplate) template.
newPlacementTemplate ::
  PlacementTemplate
newPlacementTemplate =
  PlacementTemplate'
    { defaultAttributes =
        Prelude.Nothing,
      deviceTemplates = Prelude.Nothing
    }

-- | The default attributes (key\/value pairs) to be applied to all
-- placements using this template.
placementTemplate_defaultAttributes :: Lens.Lens' PlacementTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
placementTemplate_defaultAttributes = Lens.lens (\PlacementTemplate' {defaultAttributes} -> defaultAttributes) (\s@PlacementTemplate' {} a -> s {defaultAttributes = a} :: PlacementTemplate) Prelude.. Lens.mapping Lens.coerced

-- | An object specifying the DeviceTemplate for all placements using this
-- (PlacementTemplate) template.
placementTemplate_deviceTemplates :: Lens.Lens' PlacementTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text DeviceTemplate))
placementTemplate_deviceTemplates = Lens.lens (\PlacementTemplate' {deviceTemplates} -> deviceTemplates) (\s@PlacementTemplate' {} a -> s {deviceTemplates = a} :: PlacementTemplate) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PlacementTemplate where
  parseJSON =
    Data.withObject
      "PlacementTemplate"
      ( \x ->
          PlacementTemplate'
            Prelude.<$> ( x Data..:? "defaultAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "deviceTemplates"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PlacementTemplate where
  hashWithSalt _salt PlacementTemplate' {..} =
    _salt `Prelude.hashWithSalt` defaultAttributes
      `Prelude.hashWithSalt` deviceTemplates

instance Prelude.NFData PlacementTemplate where
  rnf PlacementTemplate' {..} =
    Prelude.rnf defaultAttributes
      `Prelude.seq` Prelude.rnf deviceTemplates

instance Data.ToJSON PlacementTemplate where
  toJSON PlacementTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("defaultAttributes" Data..=)
              Prelude.<$> defaultAttributes,
            ("deviceTemplates" Data..=)
              Prelude.<$> deviceTemplates
          ]
      )
