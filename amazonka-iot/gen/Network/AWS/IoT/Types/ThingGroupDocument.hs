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

-- | The thing group search index document.
--
-- /See:/ 'newThingGroupDocument' smart constructor.
data ThingGroupDocument = ThingGroupDocument'
  { -- | Parent group names.
    parentGroupNames :: Core.Maybe [Core.Text],
    -- | The thing group attributes.
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The thing group name.
    thingGroupName :: Core.Maybe Core.Text,
    -- | The thing group ID.
    thingGroupId :: Core.Maybe Core.Text,
    -- | The thing group description.
    thingGroupDescription :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'attributes', 'thingGroupDocument_attributes' - The thing group attributes.
--
-- 'thingGroupName', 'thingGroupDocument_thingGroupName' - The thing group name.
--
-- 'thingGroupId', 'thingGroupDocument_thingGroupId' - The thing group ID.
--
-- 'thingGroupDescription', 'thingGroupDocument_thingGroupDescription' - The thing group description.
newThingGroupDocument ::
  ThingGroupDocument
newThingGroupDocument =
  ThingGroupDocument'
    { parentGroupNames =
        Core.Nothing,
      attributes = Core.Nothing,
      thingGroupName = Core.Nothing,
      thingGroupId = Core.Nothing,
      thingGroupDescription = Core.Nothing
    }

-- | Parent group names.
thingGroupDocument_parentGroupNames :: Lens.Lens' ThingGroupDocument (Core.Maybe [Core.Text])
thingGroupDocument_parentGroupNames = Lens.lens (\ThingGroupDocument' {parentGroupNames} -> parentGroupNames) (\s@ThingGroupDocument' {} a -> s {parentGroupNames = a} :: ThingGroupDocument) Core.. Lens.mapping Lens._Coerce

-- | The thing group attributes.
thingGroupDocument_attributes :: Lens.Lens' ThingGroupDocument (Core.Maybe (Core.HashMap Core.Text Core.Text))
thingGroupDocument_attributes = Lens.lens (\ThingGroupDocument' {attributes} -> attributes) (\s@ThingGroupDocument' {} a -> s {attributes = a} :: ThingGroupDocument) Core.. Lens.mapping Lens._Coerce

-- | The thing group name.
thingGroupDocument_thingGroupName :: Lens.Lens' ThingGroupDocument (Core.Maybe Core.Text)
thingGroupDocument_thingGroupName = Lens.lens (\ThingGroupDocument' {thingGroupName} -> thingGroupName) (\s@ThingGroupDocument' {} a -> s {thingGroupName = a} :: ThingGroupDocument)

-- | The thing group ID.
thingGroupDocument_thingGroupId :: Lens.Lens' ThingGroupDocument (Core.Maybe Core.Text)
thingGroupDocument_thingGroupId = Lens.lens (\ThingGroupDocument' {thingGroupId} -> thingGroupId) (\s@ThingGroupDocument' {} a -> s {thingGroupId = a} :: ThingGroupDocument)

-- | The thing group description.
thingGroupDocument_thingGroupDescription :: Lens.Lens' ThingGroupDocument (Core.Maybe Core.Text)
thingGroupDocument_thingGroupDescription = Lens.lens (\ThingGroupDocument' {thingGroupDescription} -> thingGroupDescription) (\s@ThingGroupDocument' {} a -> s {thingGroupDescription = a} :: ThingGroupDocument)

instance Core.FromJSON ThingGroupDocument where
  parseJSON =
    Core.withObject
      "ThingGroupDocument"
      ( \x ->
          ThingGroupDocument'
            Core.<$> (x Core..:? "parentGroupNames" Core..!= Core.mempty)
            Core.<*> (x Core..:? "attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "thingGroupName")
            Core.<*> (x Core..:? "thingGroupId")
            Core.<*> (x Core..:? "thingGroupDescription")
      )

instance Core.Hashable ThingGroupDocument

instance Core.NFData ThingGroupDocument
