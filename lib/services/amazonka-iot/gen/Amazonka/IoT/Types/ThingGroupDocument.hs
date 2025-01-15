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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingGroupDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The thing group search index document.
--
-- /See:/ 'newThingGroupDocument' smart constructor.
data ThingGroupDocument = ThingGroupDocument'
  { -- | The thing group attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Parent group names.
    parentGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The thing group description.
    thingGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The thing group ID.
    thingGroupId :: Prelude.Maybe Prelude.Text,
    -- | The thing group name.
    thingGroupName :: Prelude.Maybe Prelude.Text
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
-- 'attributes', 'thingGroupDocument_attributes' - The thing group attributes.
--
-- 'parentGroupNames', 'thingGroupDocument_parentGroupNames' - Parent group names.
--
-- 'thingGroupDescription', 'thingGroupDocument_thingGroupDescription' - The thing group description.
--
-- 'thingGroupId', 'thingGroupDocument_thingGroupId' - The thing group ID.
--
-- 'thingGroupName', 'thingGroupDocument_thingGroupName' - The thing group name.
newThingGroupDocument ::
  ThingGroupDocument
newThingGroupDocument =
  ThingGroupDocument'
    { attributes = Prelude.Nothing,
      parentGroupNames = Prelude.Nothing,
      thingGroupDescription = Prelude.Nothing,
      thingGroupId = Prelude.Nothing,
      thingGroupName = Prelude.Nothing
    }

-- | The thing group attributes.
thingGroupDocument_attributes :: Lens.Lens' ThingGroupDocument (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
thingGroupDocument_attributes = Lens.lens (\ThingGroupDocument' {attributes} -> attributes) (\s@ThingGroupDocument' {} a -> s {attributes = a} :: ThingGroupDocument) Prelude.. Lens.mapping Lens.coerced

-- | Parent group names.
thingGroupDocument_parentGroupNames :: Lens.Lens' ThingGroupDocument (Prelude.Maybe [Prelude.Text])
thingGroupDocument_parentGroupNames = Lens.lens (\ThingGroupDocument' {parentGroupNames} -> parentGroupNames) (\s@ThingGroupDocument' {} a -> s {parentGroupNames = a} :: ThingGroupDocument) Prelude.. Lens.mapping Lens.coerced

-- | The thing group description.
thingGroupDocument_thingGroupDescription :: Lens.Lens' ThingGroupDocument (Prelude.Maybe Prelude.Text)
thingGroupDocument_thingGroupDescription = Lens.lens (\ThingGroupDocument' {thingGroupDescription} -> thingGroupDescription) (\s@ThingGroupDocument' {} a -> s {thingGroupDescription = a} :: ThingGroupDocument)

-- | The thing group ID.
thingGroupDocument_thingGroupId :: Lens.Lens' ThingGroupDocument (Prelude.Maybe Prelude.Text)
thingGroupDocument_thingGroupId = Lens.lens (\ThingGroupDocument' {thingGroupId} -> thingGroupId) (\s@ThingGroupDocument' {} a -> s {thingGroupId = a} :: ThingGroupDocument)

-- | The thing group name.
thingGroupDocument_thingGroupName :: Lens.Lens' ThingGroupDocument (Prelude.Maybe Prelude.Text)
thingGroupDocument_thingGroupName = Lens.lens (\ThingGroupDocument' {thingGroupName} -> thingGroupName) (\s@ThingGroupDocument' {} a -> s {thingGroupName = a} :: ThingGroupDocument)

instance Data.FromJSON ThingGroupDocument where
  parseJSON =
    Data.withObject
      "ThingGroupDocument"
      ( \x ->
          ThingGroupDocument'
            Prelude.<$> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "parentGroupNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "thingGroupDescription")
            Prelude.<*> (x Data..:? "thingGroupId")
            Prelude.<*> (x Data..:? "thingGroupName")
      )

instance Prelude.Hashable ThingGroupDocument where
  hashWithSalt _salt ThingGroupDocument' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` parentGroupNames
      `Prelude.hashWithSalt` thingGroupDescription
      `Prelude.hashWithSalt` thingGroupId
      `Prelude.hashWithSalt` thingGroupName

instance Prelude.NFData ThingGroupDocument where
  rnf ThingGroupDocument' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf parentGroupNames `Prelude.seq`
        Prelude.rnf thingGroupDescription `Prelude.seq`
          Prelude.rnf thingGroupId `Prelude.seq`
            Prelude.rnf thingGroupName
