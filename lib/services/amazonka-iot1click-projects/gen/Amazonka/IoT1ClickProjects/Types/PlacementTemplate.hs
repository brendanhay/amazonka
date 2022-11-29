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
import Amazonka.IoT1ClickProjects.Types.DeviceTemplate
import qualified Amazonka.Prelude as Prelude

-- | An object defining the template for a placement.
--
-- /See:/ 'newPlacementTemplate' smart constructor.
data PlacementTemplate = PlacementTemplate'
  { -- | An object specifying the DeviceTemplate for all placements using this
    -- (PlacementTemplate) template.
    deviceTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text DeviceTemplate),
    -- | The default attributes (key\/value pairs) to be applied to all
    -- placements using this template.
    defaultAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'deviceTemplates', 'placementTemplate_deviceTemplates' - An object specifying the DeviceTemplate for all placements using this
-- (PlacementTemplate) template.
--
-- 'defaultAttributes', 'placementTemplate_defaultAttributes' - The default attributes (key\/value pairs) to be applied to all
-- placements using this template.
newPlacementTemplate ::
  PlacementTemplate
newPlacementTemplate =
  PlacementTemplate'
    { deviceTemplates =
        Prelude.Nothing,
      defaultAttributes = Prelude.Nothing
    }

-- | An object specifying the DeviceTemplate for all placements using this
-- (PlacementTemplate) template.
placementTemplate_deviceTemplates :: Lens.Lens' PlacementTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text DeviceTemplate))
placementTemplate_deviceTemplates = Lens.lens (\PlacementTemplate' {deviceTemplates} -> deviceTemplates) (\s@PlacementTemplate' {} a -> s {deviceTemplates = a} :: PlacementTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The default attributes (key\/value pairs) to be applied to all
-- placements using this template.
placementTemplate_defaultAttributes :: Lens.Lens' PlacementTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
placementTemplate_defaultAttributes = Lens.lens (\PlacementTemplate' {defaultAttributes} -> defaultAttributes) (\s@PlacementTemplate' {} a -> s {defaultAttributes = a} :: PlacementTemplate) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON PlacementTemplate where
  parseJSON =
    Core.withObject
      "PlacementTemplate"
      ( \x ->
          PlacementTemplate'
            Prelude.<$> ( x Core..:? "deviceTemplates"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "defaultAttributes"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PlacementTemplate where
  hashWithSalt _salt PlacementTemplate' {..} =
    _salt `Prelude.hashWithSalt` deviceTemplates
      `Prelude.hashWithSalt` defaultAttributes

instance Prelude.NFData PlacementTemplate where
  rnf PlacementTemplate' {..} =
    Prelude.rnf deviceTemplates
      `Prelude.seq` Prelude.rnf defaultAttributes

instance Core.ToJSON PlacementTemplate where
  toJSON PlacementTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("deviceTemplates" Core..=)
              Prelude.<$> deviceTemplates,
            ("defaultAttributes" Core..=)
              Prelude.<$> defaultAttributes
          ]
      )
