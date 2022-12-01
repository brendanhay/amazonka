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
-- Module      : Amazonka.ECS.Types.Attribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Attribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECS.Types.TargetType
import qualified Amazonka.Prelude as Prelude

-- | An attribute is a name-value pair that\'s associated with an Amazon ECS
-- object. Use attributes to extend the Amazon ECS data model by adding
-- custom metadata to your resources. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The ID of the target. You can specify the short form ID for a resource
    -- or the full Amazon Resource Name (ARN).
    targetId :: Prelude.Maybe Prelude.Text,
    -- | The type of the target to attach the attribute with. This parameter is
    -- required if you use the short form ID for a resource instead of the full
    -- ARN.
    targetType :: Prelude.Maybe TargetType,
    -- | The value of the attribute. The @value@ must contain between 1 and 128
    -- characters. It can contain letters (uppercase and lowercase), numbers,
    -- hyphens (-), underscores (_), periods (.), at signs (\@), forward
    -- slashes (\/), back slashes (\\), colons (:), or spaces. The value can\'t
    -- can\'t start or end with a space.
    value :: Prelude.Maybe Prelude.Text,
    -- | The name of the attribute. The @name@ must contain between 1 and 128
    -- characters. The name may contain letters (uppercase and lowercase),
    -- numbers, hyphens (-), underscores (_), forward slashes (\/), back
    -- slashes (\\), or periods (.).
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'targetType', 'attribute_targetType' - The type of the target to attach the attribute with. This parameter is
-- required if you use the short form ID for a resource instead of the full
-- ARN.
--
-- 'value', 'attribute_value' - The value of the attribute. The @value@ must contain between 1 and 128
-- characters. It can contain letters (uppercase and lowercase), numbers,
-- hyphens (-), underscores (_), periods (.), at signs (\@), forward
-- slashes (\/), back slashes (\\), colons (:), or spaces. The value can\'t
-- can\'t start or end with a space.
--
-- 'name', 'attribute_name' - The name of the attribute. The @name@ must contain between 1 and 128
-- characters. The name may contain letters (uppercase and lowercase),
-- numbers, hyphens (-), underscores (_), forward slashes (\/), back
-- slashes (\\), or periods (.).
newAttribute ::
  -- | 'name'
  Prelude.Text ->
  Attribute
newAttribute pName_ =
  Attribute'
    { targetId = Prelude.Nothing,
      targetType = Prelude.Nothing,
      value = Prelude.Nothing,
      name = pName_
    }

-- | The ID of the target. You can specify the short form ID for a resource
-- or the full Amazon Resource Name (ARN).
attribute_targetId :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Text)
attribute_targetId = Lens.lens (\Attribute' {targetId} -> targetId) (\s@Attribute' {} a -> s {targetId = a} :: Attribute)

-- | The type of the target to attach the attribute with. This parameter is
-- required if you use the short form ID for a resource instead of the full
-- ARN.
attribute_targetType :: Lens.Lens' Attribute (Prelude.Maybe TargetType)
attribute_targetType = Lens.lens (\Attribute' {targetType} -> targetType) (\s@Attribute' {} a -> s {targetType = a} :: Attribute)

-- | The value of the attribute. The @value@ must contain between 1 and 128
-- characters. It can contain letters (uppercase and lowercase), numbers,
-- hyphens (-), underscores (_), periods (.), at signs (\@), forward
-- slashes (\/), back slashes (\\), colons (:), or spaces. The value can\'t
-- can\'t start or end with a space.
attribute_value :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Text)
attribute_value = Lens.lens (\Attribute' {value} -> value) (\s@Attribute' {} a -> s {value = a} :: Attribute)

-- | The name of the attribute. The @name@ must contain between 1 and 128
-- characters. The name may contain letters (uppercase and lowercase),
-- numbers, hyphens (-), underscores (_), forward slashes (\/), back
-- slashes (\\), or periods (.).
attribute_name :: Lens.Lens' Attribute Prelude.Text
attribute_name = Lens.lens (\Attribute' {name} -> name) (\s@Attribute' {} a -> s {name = a} :: Attribute)

instance Core.FromJSON Attribute where
  parseJSON =
    Core.withObject
      "Attribute"
      ( \x ->
          Attribute'
            Prelude.<$> (x Core..:? "targetId")
            Prelude.<*> (x Core..:? "targetType")
            Prelude.<*> (x Core..:? "value")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable Attribute where
  hashWithSalt _salt Attribute' {..} =
    _salt `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` name

instance Prelude.NFData Attribute where
  rnf Attribute' {..} =
    Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf targetType
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf name

instance Core.ToJSON Attribute where
  toJSON Attribute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("targetId" Core..=) Prelude.<$> targetId,
            ("targetType" Core..=) Prelude.<$> targetType,
            ("value" Core..=) Prelude.<$> value,
            Prelude.Just ("name" Core..= name)
          ]
      )
