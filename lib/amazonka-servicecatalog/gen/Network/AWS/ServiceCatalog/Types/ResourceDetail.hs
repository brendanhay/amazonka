-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceDetail
  ( ResourceDetail (..),

    -- * Smart constructor
    mkResourceDetail,

    -- * Lenses
    rARN,
    rCreatedTime,
    rName,
    rId,
    rDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a resource.
--
-- /See:/ 'mkResourceDetail' smart constructor.
data ResourceDetail = ResourceDetail'
  { arn :: Lude.Maybe Lude.Text,
    createdTime :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceDetail' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the resource.
-- * 'createdTime' - The creation time of the resource.
-- * 'description' - The description of the resource.
-- * 'id' - The identifier of the resource.
-- * 'name' - The name of the resource.
mkResourceDetail ::
  ResourceDetail
mkResourceDetail =
  ResourceDetail'
    { arn = Lude.Nothing,
      createdTime = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ARN of the resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rARN :: Lens.Lens' ResourceDetail (Lude.Maybe Lude.Text)
rARN = Lens.lens (arn :: ResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ResourceDetail)
{-# DEPRECATED rARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The creation time of the resource.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreatedTime :: Lens.Lens' ResourceDetail (Lude.Maybe Lude.Timestamp)
rCreatedTime = Lens.lens (createdTime :: ResourceDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: ResourceDetail)
{-# DEPRECATED rCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The name of the resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' ResourceDetail (Lude.Maybe Lude.Text)
rName = Lens.lens (name :: ResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ResourceDetail)
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rId :: Lens.Lens' ResourceDetail (Lude.Maybe Lude.Text)
rId = Lens.lens (id :: ResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ResourceDetail)
{-# DEPRECATED rId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The description of the resource.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDescription :: Lens.Lens' ResourceDetail (Lude.Maybe Lude.Text)
rDescription = Lens.lens (description :: ResourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ResourceDetail)
{-# DEPRECATED rDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ResourceDetail where
  parseJSON =
    Lude.withObject
      "ResourceDetail"
      ( \x ->
          ResourceDetail'
            Lude.<$> (x Lude..:? "ARN")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Description")
      )
