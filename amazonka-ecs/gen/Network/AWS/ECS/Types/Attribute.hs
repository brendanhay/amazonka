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
-- Module      : Network.AWS.ECS.Types.Attribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Attribute where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.TargetType
import qualified Network.AWS.Lens as Lens

-- | An attribute is a name-value pair associated with an Amazon ECS object.
-- Attributes enable you to extend the Amazon ECS data model by adding
-- custom metadata to your resources. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The ID of the target. You can specify the short form ID for a resource
    -- or the full Amazon Resource Name (ARN).
    targetId :: Core.Maybe Core.Text,
    -- | The type of the target with which to attach the attribute. This
    -- parameter is required if you use the short form ID for a resource
    -- instead of the full ARN.
    targetType :: Core.Maybe TargetType,
    -- | The value of the attribute. The @value@ must contain between 1 and 128
    -- characters and may contain letters (uppercase and lowercase), numbers,
    -- hyphens, underscores, periods, at signs (\@), forward slashes, back
    -- slashes, colons, or spaces. The value cannot contain any leading or
    -- trailing whitespace.
    value :: Core.Maybe Core.Text,
    -- | The name of the attribute. The @name@ must contain between 1 and 128
    -- characters and name may contain letters (uppercase and lowercase),
    -- numbers, hyphens, underscores, forward slashes, back slashes, or
    -- periods.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Attribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetId', 'attribute_targetId' - The ID of the target. You can specify the short form ID for a resource
-- or the full Amazon Resource Name (ARN).
--
-- 'targetType', 'attribute_targetType' - The type of the target with which to attach the attribute. This
-- parameter is required if you use the short form ID for a resource
-- instead of the full ARN.
--
-- 'value', 'attribute_value' - The value of the attribute. The @value@ must contain between 1 and 128
-- characters and may contain letters (uppercase and lowercase), numbers,
-- hyphens, underscores, periods, at signs (\@), forward slashes, back
-- slashes, colons, or spaces. The value cannot contain any leading or
-- trailing whitespace.
--
-- 'name', 'attribute_name' - The name of the attribute. The @name@ must contain between 1 and 128
-- characters and name may contain letters (uppercase and lowercase),
-- numbers, hyphens, underscores, forward slashes, back slashes, or
-- periods.
newAttribute ::
  -- | 'name'
  Core.Text ->
  Attribute
newAttribute pName_ =
  Attribute'
    { targetId = Core.Nothing,
      targetType = Core.Nothing,
      value = Core.Nothing,
      name = pName_
    }

-- | The ID of the target. You can specify the short form ID for a resource
-- or the full Amazon Resource Name (ARN).
attribute_targetId :: Lens.Lens' Attribute (Core.Maybe Core.Text)
attribute_targetId = Lens.lens (\Attribute' {targetId} -> targetId) (\s@Attribute' {} a -> s {targetId = a} :: Attribute)

-- | The type of the target with which to attach the attribute. This
-- parameter is required if you use the short form ID for a resource
-- instead of the full ARN.
attribute_targetType :: Lens.Lens' Attribute (Core.Maybe TargetType)
attribute_targetType = Lens.lens (\Attribute' {targetType} -> targetType) (\s@Attribute' {} a -> s {targetType = a} :: Attribute)

-- | The value of the attribute. The @value@ must contain between 1 and 128
-- characters and may contain letters (uppercase and lowercase), numbers,
-- hyphens, underscores, periods, at signs (\@), forward slashes, back
-- slashes, colons, or spaces. The value cannot contain any leading or
-- trailing whitespace.
attribute_value :: Lens.Lens' Attribute (Core.Maybe Core.Text)
attribute_value = Lens.lens (\Attribute' {value} -> value) (\s@Attribute' {} a -> s {value = a} :: Attribute)

-- | The name of the attribute. The @name@ must contain between 1 and 128
-- characters and name may contain letters (uppercase and lowercase),
-- numbers, hyphens, underscores, forward slashes, back slashes, or
-- periods.
attribute_name :: Lens.Lens' Attribute Core.Text
attribute_name = Lens.lens (\Attribute' {name} -> name) (\s@Attribute' {} a -> s {name = a} :: Attribute)

instance Core.FromJSON Attribute where
  parseJSON =
    Core.withObject
      "Attribute"
      ( \x ->
          Attribute'
            Core.<$> (x Core..:? "targetId")
            Core.<*> (x Core..:? "targetType")
            Core.<*> (x Core..:? "value")
            Core.<*> (x Core..: "name")
      )

instance Core.Hashable Attribute

instance Core.NFData Attribute

instance Core.ToJSON Attribute where
  toJSON Attribute' {..} =
    Core.object
      ( Core.catMaybes
          [ ("targetId" Core..=) Core.<$> targetId,
            ("targetType" Core..=) Core.<$> targetType,
            ("value" Core..=) Core.<$> value,
            Core.Just ("name" Core..= name)
          ]
      )
