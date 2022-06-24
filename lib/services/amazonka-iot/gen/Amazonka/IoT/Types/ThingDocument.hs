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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingDocument where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.ThingConnectivity
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The thing search index document.
--
-- /See:/ 'newThingDocument' smart constructor.
data ThingDocument = ThingDocument'
  { -- | The thing name.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The thing ID.
    thingId :: Prelude.Maybe Prelude.Text,
    -- | The shadow.
    shadow :: Prelude.Maybe Prelude.Text,
    -- | The thing type name.
    thingTypeName :: Prelude.Maybe Prelude.Text,
    -- | Thing group names.
    thingGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Indicates whether the thing is connected to the Amazon Web Services IoT
    -- Core service.
    connectivity :: Prelude.Maybe ThingConnectivity
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
-- 'thingName', 'thingDocument_thingName' - The thing name.
--
-- 'thingId', 'thingDocument_thingId' - The thing ID.
--
-- 'shadow', 'thingDocument_shadow' - The shadow.
--
-- 'thingTypeName', 'thingDocument_thingTypeName' - The thing type name.
--
-- 'thingGroupNames', 'thingDocument_thingGroupNames' - Thing group names.
--
-- 'attributes', 'thingDocument_attributes' - The attributes.
--
-- 'connectivity', 'thingDocument_connectivity' - Indicates whether the thing is connected to the Amazon Web Services IoT
-- Core service.
newThingDocument ::
  ThingDocument
newThingDocument =
  ThingDocument'
    { thingName = Prelude.Nothing,
      thingId = Prelude.Nothing,
      shadow = Prelude.Nothing,
      thingTypeName = Prelude.Nothing,
      thingGroupNames = Prelude.Nothing,
      attributes = Prelude.Nothing,
      connectivity = Prelude.Nothing
    }

-- | The thing name.
thingDocument_thingName :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_thingName = Lens.lens (\ThingDocument' {thingName} -> thingName) (\s@ThingDocument' {} a -> s {thingName = a} :: ThingDocument)

-- | The thing ID.
thingDocument_thingId :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_thingId = Lens.lens (\ThingDocument' {thingId} -> thingId) (\s@ThingDocument' {} a -> s {thingId = a} :: ThingDocument)

-- | The shadow.
thingDocument_shadow :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_shadow = Lens.lens (\ThingDocument' {shadow} -> shadow) (\s@ThingDocument' {} a -> s {shadow = a} :: ThingDocument)

-- | The thing type name.
thingDocument_thingTypeName :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_thingTypeName = Lens.lens (\ThingDocument' {thingTypeName} -> thingTypeName) (\s@ThingDocument' {} a -> s {thingTypeName = a} :: ThingDocument)

-- | Thing group names.
thingDocument_thingGroupNames :: Lens.Lens' ThingDocument (Prelude.Maybe [Prelude.Text])
thingDocument_thingGroupNames = Lens.lens (\ThingDocument' {thingGroupNames} -> thingGroupNames) (\s@ThingDocument' {} a -> s {thingGroupNames = a} :: ThingDocument) Prelude.. Lens.mapping Lens.coerced

-- | The attributes.
thingDocument_attributes :: Lens.Lens' ThingDocument (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
thingDocument_attributes = Lens.lens (\ThingDocument' {attributes} -> attributes) (\s@ThingDocument' {} a -> s {attributes = a} :: ThingDocument) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the thing is connected to the Amazon Web Services IoT
-- Core service.
thingDocument_connectivity :: Lens.Lens' ThingDocument (Prelude.Maybe ThingConnectivity)
thingDocument_connectivity = Lens.lens (\ThingDocument' {connectivity} -> connectivity) (\s@ThingDocument' {} a -> s {connectivity = a} :: ThingDocument)

instance Core.FromJSON ThingDocument where
  parseJSON =
    Core.withObject
      "ThingDocument"
      ( \x ->
          ThingDocument'
            Prelude.<$> (x Core..:? "thingName")
            Prelude.<*> (x Core..:? "thingId")
            Prelude.<*> (x Core..:? "shadow")
            Prelude.<*> (x Core..:? "thingTypeName")
            Prelude.<*> ( x Core..:? "thingGroupNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "connectivity")
      )

instance Prelude.Hashable ThingDocument where
  hashWithSalt _salt ThingDocument' {..} =
    _salt `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` thingId
      `Prelude.hashWithSalt` shadow
      `Prelude.hashWithSalt` thingTypeName
      `Prelude.hashWithSalt` thingGroupNames
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` connectivity

instance Prelude.NFData ThingDocument where
  rnf ThingDocument' {..} =
    Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf thingId
      `Prelude.seq` Prelude.rnf shadow
      `Prelude.seq` Prelude.rnf thingTypeName
      `Prelude.seq` Prelude.rnf thingGroupNames
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf connectivity
