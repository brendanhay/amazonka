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
-- Module      : Network.AWS.Rekognition.Types.Label
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Label where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.Instance
import Network.AWS.Rekognition.Types.Parent

-- | Structure containing details about the detected label, including the
-- name, detected instances, parent labels, and level of confidence.
--
-- /See:/ 'newLabel' smart constructor.
data Label = Label'
  { -- | The parent labels for a label. The response includes all ancestor
    -- labels.
    parents :: Core.Maybe [Parent],
    -- | If @Label@ represents an object, @Instances@ contains the bounding boxes
    -- for each instance of the detected object. Bounding boxes are returned
    -- for common object labels such as people, cars, furniture, apparel or
    -- pets.
    instances :: Core.Maybe [Instance],
    -- | The name (label) of the object or scene.
    name :: Core.Maybe Core.Text,
    -- | Level of confidence.
    confidence :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Label' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parents', 'label_parents' - The parent labels for a label. The response includes all ancestor
-- labels.
--
-- 'instances', 'label_instances' - If @Label@ represents an object, @Instances@ contains the bounding boxes
-- for each instance of the detected object. Bounding boxes are returned
-- for common object labels such as people, cars, furniture, apparel or
-- pets.
--
-- 'name', 'label_name' - The name (label) of the object or scene.
--
-- 'confidence', 'label_confidence' - Level of confidence.
newLabel ::
  Label
newLabel =
  Label'
    { parents = Core.Nothing,
      instances = Core.Nothing,
      name = Core.Nothing,
      confidence = Core.Nothing
    }

-- | The parent labels for a label. The response includes all ancestor
-- labels.
label_parents :: Lens.Lens' Label (Core.Maybe [Parent])
label_parents = Lens.lens (\Label' {parents} -> parents) (\s@Label' {} a -> s {parents = a} :: Label) Core.. Lens.mapping Lens._Coerce

-- | If @Label@ represents an object, @Instances@ contains the bounding boxes
-- for each instance of the detected object. Bounding boxes are returned
-- for common object labels such as people, cars, furniture, apparel or
-- pets.
label_instances :: Lens.Lens' Label (Core.Maybe [Instance])
label_instances = Lens.lens (\Label' {instances} -> instances) (\s@Label' {} a -> s {instances = a} :: Label) Core.. Lens.mapping Lens._Coerce

-- | The name (label) of the object or scene.
label_name :: Lens.Lens' Label (Core.Maybe Core.Text)
label_name = Lens.lens (\Label' {name} -> name) (\s@Label' {} a -> s {name = a} :: Label)

-- | Level of confidence.
label_confidence :: Lens.Lens' Label (Core.Maybe Core.Double)
label_confidence = Lens.lens (\Label' {confidence} -> confidence) (\s@Label' {} a -> s {confidence = a} :: Label)

instance Core.FromJSON Label where
  parseJSON =
    Core.withObject
      "Label"
      ( \x ->
          Label'
            Core.<$> (x Core..:? "Parents" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Instances" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Confidence")
      )

instance Core.Hashable Label

instance Core.NFData Label
