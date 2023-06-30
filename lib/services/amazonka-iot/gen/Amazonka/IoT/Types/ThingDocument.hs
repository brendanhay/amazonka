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
-- Module      : Amazonka.IoT.Types.ThingDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.ThingConnectivity
import qualified Amazonka.Prelude as Prelude

-- | The thing search index document.
--
-- /See:/ 'newThingDocument' smart constructor.
data ThingDocument = ThingDocument'
  { -- | The attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Indicates whether the thing is connected to the Amazon Web Services IoT
    -- Core service.
    connectivity :: Prelude.Maybe ThingConnectivity,
    -- | Contains Device Defender data.
    --
    -- For more information about Device Defender, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/device-defender.html Device Defender>.
    deviceDefender :: Prelude.Maybe Prelude.Text,
    -- | The unnamed shadow and named shadow.
    --
    -- For more information about shadows, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-device-shadows.html IoT Device Shadow service.>
    shadow :: Prelude.Maybe Prelude.Text,
    -- | Thing group names.
    thingGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The thing ID.
    thingId :: Prelude.Maybe Prelude.Text,
    -- | The thing name.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The thing type name.
    thingTypeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThingDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'thingDocument_attributes' - The attributes.
--
-- 'connectivity', 'thingDocument_connectivity' - Indicates whether the thing is connected to the Amazon Web Services IoT
-- Core service.
--
-- 'deviceDefender', 'thingDocument_deviceDefender' - Contains Device Defender data.
--
-- For more information about Device Defender, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/device-defender.html Device Defender>.
--
-- 'shadow', 'thingDocument_shadow' - The unnamed shadow and named shadow.
--
-- For more information about shadows, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-device-shadows.html IoT Device Shadow service.>
--
-- 'thingGroupNames', 'thingDocument_thingGroupNames' - Thing group names.
--
-- 'thingId', 'thingDocument_thingId' - The thing ID.
--
-- 'thingName', 'thingDocument_thingName' - The thing name.
--
-- 'thingTypeName', 'thingDocument_thingTypeName' - The thing type name.
newThingDocument ::
  ThingDocument
newThingDocument =
  ThingDocument'
    { attributes = Prelude.Nothing,
      connectivity = Prelude.Nothing,
      deviceDefender = Prelude.Nothing,
      shadow = Prelude.Nothing,
      thingGroupNames = Prelude.Nothing,
      thingId = Prelude.Nothing,
      thingName = Prelude.Nothing,
      thingTypeName = Prelude.Nothing
    }

-- | The attributes.
thingDocument_attributes :: Lens.Lens' ThingDocument (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
thingDocument_attributes = Lens.lens (\ThingDocument' {attributes} -> attributes) (\s@ThingDocument' {} a -> s {attributes = a} :: ThingDocument) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the thing is connected to the Amazon Web Services IoT
-- Core service.
thingDocument_connectivity :: Lens.Lens' ThingDocument (Prelude.Maybe ThingConnectivity)
thingDocument_connectivity = Lens.lens (\ThingDocument' {connectivity} -> connectivity) (\s@ThingDocument' {} a -> s {connectivity = a} :: ThingDocument)

-- | Contains Device Defender data.
--
-- For more information about Device Defender, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/device-defender.html Device Defender>.
thingDocument_deviceDefender :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_deviceDefender = Lens.lens (\ThingDocument' {deviceDefender} -> deviceDefender) (\s@ThingDocument' {} a -> s {deviceDefender = a} :: ThingDocument)

-- | The unnamed shadow and named shadow.
--
-- For more information about shadows, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-device-shadows.html IoT Device Shadow service.>
thingDocument_shadow :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_shadow = Lens.lens (\ThingDocument' {shadow} -> shadow) (\s@ThingDocument' {} a -> s {shadow = a} :: ThingDocument)

-- | Thing group names.
thingDocument_thingGroupNames :: Lens.Lens' ThingDocument (Prelude.Maybe [Prelude.Text])
thingDocument_thingGroupNames = Lens.lens (\ThingDocument' {thingGroupNames} -> thingGroupNames) (\s@ThingDocument' {} a -> s {thingGroupNames = a} :: ThingDocument) Prelude.. Lens.mapping Lens.coerced

-- | The thing ID.
thingDocument_thingId :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_thingId = Lens.lens (\ThingDocument' {thingId} -> thingId) (\s@ThingDocument' {} a -> s {thingId = a} :: ThingDocument)

-- | The thing name.
thingDocument_thingName :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_thingName = Lens.lens (\ThingDocument' {thingName} -> thingName) (\s@ThingDocument' {} a -> s {thingName = a} :: ThingDocument)

-- | The thing type name.
thingDocument_thingTypeName :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_thingTypeName = Lens.lens (\ThingDocument' {thingTypeName} -> thingTypeName) (\s@ThingDocument' {} a -> s {thingTypeName = a} :: ThingDocument)

instance Data.FromJSON ThingDocument where
  parseJSON =
    Data.withObject
      "ThingDocument"
      ( \x ->
          ThingDocument'
            Prelude.<$> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "connectivity")
            Prelude.<*> (x Data..:? "deviceDefender")
            Prelude.<*> (x Data..:? "shadow")
            Prelude.<*> ( x
                            Data..:? "thingGroupNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "thingId")
            Prelude.<*> (x Data..:? "thingName")
            Prelude.<*> (x Data..:? "thingTypeName")
      )

instance Prelude.Hashable ThingDocument where
  hashWithSalt _salt ThingDocument' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` connectivity
      `Prelude.hashWithSalt` deviceDefender
      `Prelude.hashWithSalt` shadow
      `Prelude.hashWithSalt` thingGroupNames
      `Prelude.hashWithSalt` thingId
      `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` thingTypeName

instance Prelude.NFData ThingDocument where
  rnf ThingDocument' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf connectivity
      `Prelude.seq` Prelude.rnf deviceDefender
      `Prelude.seq` Prelude.rnf shadow
      `Prelude.seq` Prelude.rnf thingGroupNames
      `Prelude.seq` Prelude.rnf thingId
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf thingTypeName
