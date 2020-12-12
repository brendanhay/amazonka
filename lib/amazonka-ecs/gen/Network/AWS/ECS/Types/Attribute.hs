{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Attribute
  ( Attribute (..),

    -- * Smart constructor
    mkAttribute,

    -- * Lenses
    aTargetId,
    aValue,
    aTargetType,
    aName,
  )
where

import Network.AWS.ECS.Types.TargetType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An attribute is a name-value pair associated with an Amazon ECS object. Attributes enable you to extend the Amazon ECS data model by adding custom metadata to your resources. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkAttribute' smart constructor.
data Attribute = Attribute'
  { targetId :: Lude.Maybe Lude.Text,
    value :: Lude.Maybe Lude.Text,
    targetType :: Lude.Maybe TargetType,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- * 'name' - The name of the attribute. The @name@ must contain between 1 and 128 characters and name may contain letters (uppercase and lowercase), numbers, hyphens, underscores, forward slashes, back slashes, or periods.
-- * 'targetId' - The ID of the target. You can specify the short form ID for a resource or the full Amazon Resource Name (ARN).
-- * 'targetType' - The type of the target with which to attach the attribute. This parameter is required if you use the short form ID for a resource instead of the full ARN.
-- * 'value' - The value of the attribute. The @value@ must contain between 1 and 128 characters and may contain letters (uppercase and lowercase), numbers, hyphens, underscores, periods, at signs (@), forward slashes, back slashes, colons, or spaces. The value cannot contain any leading or trailing whitespace.
mkAttribute ::
  -- | 'name'
  Lude.Text ->
  Attribute
mkAttribute pName_ =
  Attribute'
    { targetId = Lude.Nothing,
      value = Lude.Nothing,
      targetType = Lude.Nothing,
      name = pName_
    }

-- | The ID of the target. You can specify the short form ID for a resource or the full Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTargetId :: Lens.Lens' Attribute (Lude.Maybe Lude.Text)
aTargetId = Lens.lens (targetId :: Attribute -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: Attribute)
{-# DEPRECATED aTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The value of the attribute. The @value@ must contain between 1 and 128 characters and may contain letters (uppercase and lowercase), numbers, hyphens, underscores, periods, at signs (@), forward slashes, back slashes, colons, or spaces. The value cannot contain any leading or trailing whitespace.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValue :: Lens.Lens' Attribute (Lude.Maybe Lude.Text)
aValue = Lens.lens (value :: Attribute -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Attribute)
{-# DEPRECATED aValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of the target with which to attach the attribute. This parameter is required if you use the short form ID for a resource instead of the full ARN.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTargetType :: Lens.Lens' Attribute (Lude.Maybe TargetType)
aTargetType = Lens.lens (targetType :: Attribute -> Lude.Maybe TargetType) (\s a -> s {targetType = a} :: Attribute)
{-# DEPRECATED aTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The name of the attribute. The @name@ must contain between 1 and 128 characters and name may contain letters (uppercase and lowercase), numbers, hyphens, underscores, forward slashes, back slashes, or periods.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Attribute Lude.Text
aName = Lens.lens (name :: Attribute -> Lude.Text) (\s a -> s {name = a} :: Attribute)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON Attribute where
  parseJSON =
    Lude.withObject
      "Attribute"
      ( \x ->
          Attribute'
            Lude.<$> (x Lude..:? "targetId")
            Lude.<*> (x Lude..:? "value")
            Lude.<*> (x Lude..:? "targetType")
            Lude.<*> (x Lude..: "name")
      )

instance Lude.ToJSON Attribute where
  toJSON Attribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("targetId" Lude..=) Lude.<$> targetId,
            ("value" Lude..=) Lude.<$> value,
            ("targetType" Lude..=) Lude.<$> targetType,
            Lude.Just ("name" Lude..= name)
          ]
      )
