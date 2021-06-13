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
-- Module      : Network.AWS.IoT.Types.ThingDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingDocument where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.ThingConnectivity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The thing search index document.
--
-- /See:/ 'newThingDocument' smart constructor.
data ThingDocument = ThingDocument'
  { -- | The thing ID.
    thingId :: Prelude.Maybe Prelude.Text,
    -- | The thing name.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the thing is connected to the AWS IoT service.
    connectivity :: Prelude.Maybe ThingConnectivity,
    -- | The attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Thing group names.
    thingGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The shadow.
    shadow :: Prelude.Maybe Prelude.Text,
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
-- 'thingId', 'thingDocument_thingId' - The thing ID.
--
-- 'thingName', 'thingDocument_thingName' - The thing name.
--
-- 'connectivity', 'thingDocument_connectivity' - Indicates whether the thing is connected to the AWS IoT service.
--
-- 'attributes', 'thingDocument_attributes' - The attributes.
--
-- 'thingGroupNames', 'thingDocument_thingGroupNames' - Thing group names.
--
-- 'shadow', 'thingDocument_shadow' - The shadow.
--
-- 'thingTypeName', 'thingDocument_thingTypeName' - The thing type name.
newThingDocument ::
  ThingDocument
newThingDocument =
  ThingDocument'
    { thingId = Prelude.Nothing,
      thingName = Prelude.Nothing,
      connectivity = Prelude.Nothing,
      attributes = Prelude.Nothing,
      thingGroupNames = Prelude.Nothing,
      shadow = Prelude.Nothing,
      thingTypeName = Prelude.Nothing
    }

-- | The thing ID.
thingDocument_thingId :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_thingId = Lens.lens (\ThingDocument' {thingId} -> thingId) (\s@ThingDocument' {} a -> s {thingId = a} :: ThingDocument)

-- | The thing name.
thingDocument_thingName :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_thingName = Lens.lens (\ThingDocument' {thingName} -> thingName) (\s@ThingDocument' {} a -> s {thingName = a} :: ThingDocument)

-- | Indicates whether the thing is connected to the AWS IoT service.
thingDocument_connectivity :: Lens.Lens' ThingDocument (Prelude.Maybe ThingConnectivity)
thingDocument_connectivity = Lens.lens (\ThingDocument' {connectivity} -> connectivity) (\s@ThingDocument' {} a -> s {connectivity = a} :: ThingDocument)

-- | The attributes.
thingDocument_attributes :: Lens.Lens' ThingDocument (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
thingDocument_attributes = Lens.lens (\ThingDocument' {attributes} -> attributes) (\s@ThingDocument' {} a -> s {attributes = a} :: ThingDocument) Prelude.. Lens.mapping Lens._Coerce

-- | Thing group names.
thingDocument_thingGroupNames :: Lens.Lens' ThingDocument (Prelude.Maybe [Prelude.Text])
thingDocument_thingGroupNames = Lens.lens (\ThingDocument' {thingGroupNames} -> thingGroupNames) (\s@ThingDocument' {} a -> s {thingGroupNames = a} :: ThingDocument) Prelude.. Lens.mapping Lens._Coerce

-- | The shadow.
thingDocument_shadow :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_shadow = Lens.lens (\ThingDocument' {shadow} -> shadow) (\s@ThingDocument' {} a -> s {shadow = a} :: ThingDocument)

-- | The thing type name.
thingDocument_thingTypeName :: Lens.Lens' ThingDocument (Prelude.Maybe Prelude.Text)
thingDocument_thingTypeName = Lens.lens (\ThingDocument' {thingTypeName} -> thingTypeName) (\s@ThingDocument' {} a -> s {thingTypeName = a} :: ThingDocument)

instance Core.FromJSON ThingDocument where
  parseJSON =
    Core.withObject
      "ThingDocument"
      ( \x ->
          ThingDocument'
            Prelude.<$> (x Core..:? "thingId")
            Prelude.<*> (x Core..:? "thingName")
            Prelude.<*> (x Core..:? "connectivity")
            Prelude.<*> (x Core..:? "attributes" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "thingGroupNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "shadow")
            Prelude.<*> (x Core..:? "thingTypeName")
      )

instance Prelude.Hashable ThingDocument

instance Prelude.NFData ThingDocument
