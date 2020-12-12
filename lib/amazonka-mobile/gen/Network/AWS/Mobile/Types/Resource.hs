{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.Resource
  ( Resource (..),

    -- * Smart constructor
    mkResource,

    -- * Lenses
    rFeature,
    rArn,
    rName,
    rAttributes,
    rType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an instance of an AWS resource associated with a project.
--
-- /See:/ 'mkResource' smart constructor.
data Resource = Resource'
  { feature :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
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
-- * 'arn' - Undocumented field.
-- * 'attributes' - Undocumented field.
-- * 'feature' - Undocumented field.
-- * 'name' - Undocumented field.
-- * 'type'' - Undocumented field.
mkResource ::
  Resource
mkResource =
  Resource'
    { feature = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      attributes = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'feature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rFeature :: Lens.Lens' Resource (Lude.Maybe Lude.Text)
rFeature = Lens.lens (feature :: Resource -> Lude.Maybe Lude.Text) (\s a -> s {feature = a} :: Resource)
{-# DEPRECATED rFeature "Use generic-lens or generic-optics with 'feature' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rArn :: Lens.Lens' Resource (Lude.Maybe Lude.Text)
rArn = Lens.lens (arn :: Resource -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Resource)
{-# DEPRECATED rArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Resource (Lude.Maybe Lude.Text)
rName = Lens.lens (name :: Resource -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Resource)
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAttributes :: Lens.Lens' Resource (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rAttributes = Lens.lens (attributes :: Resource -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: Resource)
{-# DEPRECATED rAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Undocumented field.
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
            Lude.<$> (x Lude..:? "feature")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "type")
      )
