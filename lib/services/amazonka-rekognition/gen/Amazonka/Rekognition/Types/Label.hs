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
-- Module      : Amazonka.Rekognition.Types.Label
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Label where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.Instance
import Amazonka.Rekognition.Types.LabelAlias
import Amazonka.Rekognition.Types.LabelCategory
import Amazonka.Rekognition.Types.Parent

-- | Structure containing details about the detected label, including the
-- name, detected instances, parent labels, and level of confidence.
--
-- /See:/ 'newLabel' smart constructor.
data Label = Label'
  { -- | A list of potential aliases for a given label.
    aliases :: Prelude.Maybe [LabelAlias],
    -- | A list of the categories associated with a given label.
    categories :: Prelude.Maybe [LabelCategory],
    -- | Level of confidence.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | If @Label@ represents an object, @Instances@ contains the bounding boxes
    -- for each instance of the detected object. Bounding boxes are returned
    -- for common object labels such as people, cars, furniture, apparel or
    -- pets.
    instances :: Prelude.Maybe [Instance],
    -- | The name (label) of the object or scene.
    name :: Prelude.Maybe Prelude.Text,
    -- | The parent labels for a label. The response includes all ancestor
    -- labels.
    parents :: Prelude.Maybe [Parent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Label' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliases', 'label_aliases' - A list of potential aliases for a given label.
--
-- 'categories', 'label_categories' - A list of the categories associated with a given label.
--
-- 'confidence', 'label_confidence' - Level of confidence.
--
-- 'instances', 'label_instances' - If @Label@ represents an object, @Instances@ contains the bounding boxes
-- for each instance of the detected object. Bounding boxes are returned
-- for common object labels such as people, cars, furniture, apparel or
-- pets.
--
-- 'name', 'label_name' - The name (label) of the object or scene.
--
-- 'parents', 'label_parents' - The parent labels for a label. The response includes all ancestor
-- labels.
newLabel ::
  Label
newLabel =
  Label'
    { aliases = Prelude.Nothing,
      categories = Prelude.Nothing,
      confidence = Prelude.Nothing,
      instances = Prelude.Nothing,
      name = Prelude.Nothing,
      parents = Prelude.Nothing
    }

-- | A list of potential aliases for a given label.
label_aliases :: Lens.Lens' Label (Prelude.Maybe [LabelAlias])
label_aliases = Lens.lens (\Label' {aliases} -> aliases) (\s@Label' {} a -> s {aliases = a} :: Label) Prelude.. Lens.mapping Lens.coerced

-- | A list of the categories associated with a given label.
label_categories :: Lens.Lens' Label (Prelude.Maybe [LabelCategory])
label_categories = Lens.lens (\Label' {categories} -> categories) (\s@Label' {} a -> s {categories = a} :: Label) Prelude.. Lens.mapping Lens.coerced

-- | Level of confidence.
label_confidence :: Lens.Lens' Label (Prelude.Maybe Prelude.Double)
label_confidence = Lens.lens (\Label' {confidence} -> confidence) (\s@Label' {} a -> s {confidence = a} :: Label)

-- | If @Label@ represents an object, @Instances@ contains the bounding boxes
-- for each instance of the detected object. Bounding boxes are returned
-- for common object labels such as people, cars, furniture, apparel or
-- pets.
label_instances :: Lens.Lens' Label (Prelude.Maybe [Instance])
label_instances = Lens.lens (\Label' {instances} -> instances) (\s@Label' {} a -> s {instances = a} :: Label) Prelude.. Lens.mapping Lens.coerced

-- | The name (label) of the object or scene.
label_name :: Lens.Lens' Label (Prelude.Maybe Prelude.Text)
label_name = Lens.lens (\Label' {name} -> name) (\s@Label' {} a -> s {name = a} :: Label)

-- | The parent labels for a label. The response includes all ancestor
-- labels.
label_parents :: Lens.Lens' Label (Prelude.Maybe [Parent])
label_parents = Lens.lens (\Label' {parents} -> parents) (\s@Label' {} a -> s {parents = a} :: Label) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Label where
  parseJSON =
    Data.withObject
      "Label"
      ( \x ->
          Label'
            Prelude.<$> (x Data..:? "Aliases" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Categories" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Instances" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Parents" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Label where
  hashWithSalt _salt Label' {..} =
    _salt `Prelude.hashWithSalt` aliases
      `Prelude.hashWithSalt` categories
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` parents

instance Prelude.NFData Label where
  rnf Label' {..} =
    Prelude.rnf aliases
      `Prelude.seq` Prelude.rnf categories
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf instances
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf parents
