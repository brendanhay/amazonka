-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Resource
  ( Resource (..),

    -- * Smart constructor
    mkResource,

    -- * Lenses
    rStringSetValue,
    rIntegerValue,
    rDoubleValue,
    rLongValue,
    rName,
    rType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the resources available for a container instance.
--
-- /See:/ 'mkResource' smart constructor.
data Resource = Resource'
  { stringSetValue :: Lude.Maybe [Lude.Text],
    integerValue :: Lude.Maybe Lude.Int,
    doubleValue :: Lude.Maybe Lude.Double,
    longValue :: Lude.Maybe Lude.Integer,
    name :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- * 'doubleValue' - When the @doubleValue@ type is set, the value of the resource must be a double precision floating-point type.
-- * 'integerValue' - When the @integerValue@ type is set, the value of the resource must be an integer.
-- * 'longValue' - When the @longValue@ type is set, the value of the resource must be an extended precision floating-point type.
-- * 'name' - The name of the resource, such as @CPU@ , @MEMORY@ , @PORTS@ , @PORTS_UDP@ , or a user-defined resource.
-- * 'stringSetValue' - When the @stringSetValue@ type is set, the value of the resource must be a string type.
-- * 'type'' - The type of the resource, such as @INTEGER@ , @DOUBLE@ , @LONG@ , or @STRINGSET@ .
mkResource ::
  Resource
mkResource =
  Resource'
    { stringSetValue = Lude.Nothing,
      integerValue = Lude.Nothing,
      doubleValue = Lude.Nothing,
      longValue = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | When the @stringSetValue@ type is set, the value of the resource must be a string type.
--
-- /Note:/ Consider using 'stringSetValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStringSetValue :: Lens.Lens' Resource (Lude.Maybe [Lude.Text])
rStringSetValue = Lens.lens (stringSetValue :: Resource -> Lude.Maybe [Lude.Text]) (\s a -> s {stringSetValue = a} :: Resource)
{-# DEPRECATED rStringSetValue "Use generic-lens or generic-optics with 'stringSetValue' instead." #-}

-- | When the @integerValue@ type is set, the value of the resource must be an integer.
--
-- /Note:/ Consider using 'integerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rIntegerValue :: Lens.Lens' Resource (Lude.Maybe Lude.Int)
rIntegerValue = Lens.lens (integerValue :: Resource -> Lude.Maybe Lude.Int) (\s a -> s {integerValue = a} :: Resource)
{-# DEPRECATED rIntegerValue "Use generic-lens or generic-optics with 'integerValue' instead." #-}

-- | When the @doubleValue@ type is set, the value of the resource must be a double precision floating-point type.
--
-- /Note:/ Consider using 'doubleValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDoubleValue :: Lens.Lens' Resource (Lude.Maybe Lude.Double)
rDoubleValue = Lens.lens (doubleValue :: Resource -> Lude.Maybe Lude.Double) (\s a -> s {doubleValue = a} :: Resource)
{-# DEPRECATED rDoubleValue "Use generic-lens or generic-optics with 'doubleValue' instead." #-}

-- | When the @longValue@ type is set, the value of the resource must be an extended precision floating-point type.
--
-- /Note:/ Consider using 'longValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLongValue :: Lens.Lens' Resource (Lude.Maybe Lude.Integer)
rLongValue = Lens.lens (longValue :: Resource -> Lude.Maybe Lude.Integer) (\s a -> s {longValue = a} :: Resource)
{-# DEPRECATED rLongValue "Use generic-lens or generic-optics with 'longValue' instead." #-}

-- | The name of the resource, such as @CPU@ , @MEMORY@ , @PORTS@ , @PORTS_UDP@ , or a user-defined resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Resource (Lude.Maybe Lude.Text)
rName = Lens.lens (name :: Resource -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Resource)
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the resource, such as @INTEGER@ , @DOUBLE@ , @LONG@ , or @STRINGSET@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rType :: Lens.Lens' Resource (Lude.Maybe Lude.Text)
rType = Lens.lens (type' :: Resource -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: Resource)
{-# DEPRECATED rType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Resource where
  parseJSON =
    Lude.withObject
      "Resource"
      ( \x ->
          Resource'
            Lude.<$> (x Lude..:? "stringSetValue" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "integerValue")
            Lude.<*> (x Lude..:? "doubleValue")
            Lude.<*> (x Lude..:? "longValue")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "type")
      )

instance Lude.ToJSON Resource where
  toJSON Resource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stringSetValue" Lude..=) Lude.<$> stringSetValue,
            ("integerValue" Lude..=) Lude.<$> integerValue,
            ("doubleValue" Lude..=) Lude.<$> doubleValue,
            ("longValue" Lude..=) Lude.<$> longValue,
            ("name" Lude..=) Lude.<$> name,
            ("type" Lude..=) Lude.<$> type'
          ]
      )
