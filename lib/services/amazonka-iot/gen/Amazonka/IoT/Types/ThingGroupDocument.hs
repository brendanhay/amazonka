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
-- Module      : Amazonka.IoT.Types.ThingGroupDocument
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingGroupDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The thing group search index document.
--
-- /See:/ 'newThingGroupDocument' smart constructor.
data ThingGroupDocument = ThingGroupDocument'
  { -- | The thing group description.
    thingGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The thing group name.
    thingGroupName :: Prelude.Maybe Prelude.Text,
    -- | Parent group names.
    parentGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The thing group attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The thing group ID.
    thingGroupId :: Prelude.Maybe Prelude.Text
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
-- 'thingGroupDescription', 'thingGroupDocument_thingGroupDescription' - The thing group description.
--
-- 'thingGroupName', 'thingGroupDocument_thingGroupName' - The thing group name.
--
-- 'parentGroupNames', 'thingGroupDocument_parentGroupNames' - Parent group names.
--
-- 'attributes', 'thingGroupDocument_attributes' - The thing group attributes.
--
-- 'thingGroupId', 'thingGroupDocument_thingGroupId' - The thing group ID.
newThingGroupDocument ::
  ThingGroupDocument
newThingGroupDocument =
  ThingGroupDocument'
    { thingGroupDescription =
        Prelude.Nothing,
      thingGroupName = Prelude.Nothing,
      parentGroupNames = Prelude.Nothing,
      attributes = Prelude.Nothing,
      thingGroupId = Prelude.Nothing
    }

-- | The thing group description.
thingGroupDocument_thingGroupDescription :: Lens.Lens' ThingGroupDocument (Prelude.Maybe Prelude.Text)
thingGroupDocument_thingGroupDescription = Lens.lens (\ThingGroupDocument' {thingGroupDescription} -> thingGroupDescription) (\s@ThingGroupDocument' {} a -> s {thingGroupDescription = a} :: ThingGroupDocument)

-- | The thing group name.
thingGroupDocument_thingGroupName :: Lens.Lens' ThingGroupDocument (Prelude.Maybe Prelude.Text)
thingGroupDocument_thingGroupName = Lens.lens (\ThingGroupDocument' {thingGroupName} -> thingGroupName) (\s@ThingGroupDocument' {} a -> s {thingGroupName = a} :: ThingGroupDocument)

-- | Parent group names.
thingGroupDocument_parentGroupNames :: Lens.Lens' ThingGroupDocument (Prelude.Maybe [Prelude.Text])
thingGroupDocument_parentGroupNames = Lens.lens (\ThingGroupDocument' {parentGroupNames} -> parentGroupNames) (\s@ThingGroupDocument' {} a -> s {parentGroupNames = a} :: ThingGroupDocument) Prelude.. Lens.mapping Lens.coerced

-- | The thing group attributes.
thingGroupDocument_attributes :: Lens.Lens' ThingGroupDocument (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
thingGroupDocument_attributes = Lens.lens (\ThingGroupDocument' {attributes} -> attributes) (\s@ThingGroupDocument' {} a -> s {attributes = a} :: ThingGroupDocument) Prelude.. Lens.mapping Lens.coerced

-- | The thing group ID.
thingGroupDocument_thingGroupId :: Lens.Lens' ThingGroupDocument (Prelude.Maybe Prelude.Text)
thingGroupDocument_thingGroupId = Lens.lens (\ThingGroupDocument' {thingGroupId} -> thingGroupId) (\s@ThingGroupDocument' {} a -> s {thingGroupId = a} :: ThingGroupDocument)

instance Core.FromJSON ThingGroupDocument where
  parseJSON =
    Core.withObject
      "ThingGroupDocument"
      ( \x ->
          ThingGroupDocument'
            Prelude.<$> (x Core..:? "thingGroupDescription")
            Prelude.<*> (x Core..:? "thingGroupName")
            Prelude.<*> ( x Core..:? "parentGroupNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "thingGroupId")
      )

instance Prelude.Hashable ThingGroupDocument where
  hashWithSalt _salt ThingGroupDocument' {..} =
    _salt `Prelude.hashWithSalt` thingGroupDescription
      `Prelude.hashWithSalt` thingGroupName
      `Prelude.hashWithSalt` parentGroupNames
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` thingGroupId

instance Prelude.NFData ThingGroupDocument where
  rnf ThingGroupDocument' {..} =
    Prelude.rnf thingGroupDescription
      `Prelude.seq` Prelude.rnf thingGroupName
      `Prelude.seq` Prelude.rnf parentGroupNames
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf thingGroupId
