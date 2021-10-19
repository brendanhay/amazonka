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
-- Module      : Network.AWS.IoT.Types.ThingGroupDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupDocument where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The thing group search index document.
--
-- /See:/ 'newThingGroupDocument' smart constructor.
data ThingGroupDocument = ThingGroupDocument'
  { -- | Parent group names.
    parentGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The thing group ID.
    thingGroupId :: Prelude.Maybe Prelude.Text,
    -- | The thing group name.
    thingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The thing group attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The thing group description.
    thingGroupDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThingGroupDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentGroupNames', 'thingGroupDocument_parentGroupNames' - Parent group names.
--
-- 'thingGroupId', 'thingGroupDocument_thingGroupId' - The thing group ID.
--
-- 'thingGroupName', 'thingGroupDocument_thingGroupName' - The thing group name.
--
-- 'attributes', 'thingGroupDocument_attributes' - The thing group attributes.
--
-- 'thingGroupDescription', 'thingGroupDocument_thingGroupDescription' - The thing group description.
newThingGroupDocument ::
  ThingGroupDocument
newThingGroupDocument =
  ThingGroupDocument'
    { parentGroupNames =
        Prelude.Nothing,
      thingGroupId = Prelude.Nothing,
      thingGroupName = Prelude.Nothing,
      attributes = Prelude.Nothing,
      thingGroupDescription = Prelude.Nothing
    }

-- | Parent group names.
thingGroupDocument_parentGroupNames :: Lens.Lens' ThingGroupDocument (Prelude.Maybe [Prelude.Text])
thingGroupDocument_parentGroupNames = Lens.lens (\ThingGroupDocument' {parentGroupNames} -> parentGroupNames) (\s@ThingGroupDocument' {} a -> s {parentGroupNames = a} :: ThingGroupDocument) Prelude.. Lens.mapping Lens.coerced

-- | The thing group ID.
thingGroupDocument_thingGroupId :: Lens.Lens' ThingGroupDocument (Prelude.Maybe Prelude.Text)
thingGroupDocument_thingGroupId = Lens.lens (\ThingGroupDocument' {thingGroupId} -> thingGroupId) (\s@ThingGroupDocument' {} a -> s {thingGroupId = a} :: ThingGroupDocument)

-- | The thing group name.
thingGroupDocument_thingGroupName :: Lens.Lens' ThingGroupDocument (Prelude.Maybe Prelude.Text)
thingGroupDocument_thingGroupName = Lens.lens (\ThingGroupDocument' {thingGroupName} -> thingGroupName) (\s@ThingGroupDocument' {} a -> s {thingGroupName = a} :: ThingGroupDocument)

-- | The thing group attributes.
thingGroupDocument_attributes :: Lens.Lens' ThingGroupDocument (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
thingGroupDocument_attributes = Lens.lens (\ThingGroupDocument' {attributes} -> attributes) (\s@ThingGroupDocument' {} a -> s {attributes = a} :: ThingGroupDocument) Prelude.. Lens.mapping Lens.coerced

-- | The thing group description.
thingGroupDocument_thingGroupDescription :: Lens.Lens' ThingGroupDocument (Prelude.Maybe Prelude.Text)
thingGroupDocument_thingGroupDescription = Lens.lens (\ThingGroupDocument' {thingGroupDescription} -> thingGroupDescription) (\s@ThingGroupDocument' {} a -> s {thingGroupDescription = a} :: ThingGroupDocument)

instance Core.FromJSON ThingGroupDocument where
  parseJSON =
    Core.withObject
      "ThingGroupDocument"
      ( \x ->
          ThingGroupDocument'
            Prelude.<$> ( x Core..:? "parentGroupNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "thingGroupId")
            Prelude.<*> (x Core..:? "thingGroupName")
            Prelude.<*> (x Core..:? "attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "thingGroupDescription")
      )

instance Prelude.Hashable ThingGroupDocument

instance Prelude.NFData ThingGroupDocument
