{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.ResourceIdentifier
  ( ResourceIdentifier (..),

    -- * Smart constructor
    mkResourceIdentifier,

    -- * Lenses
    riResourceType,
    riResourceARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The ARN of a resource, and its resource type.
--
-- /See:/ 'mkResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { resourceType ::
      Lude.Maybe Lude.Text,
    resourceARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceIdentifier' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN of a resource.
-- * 'resourceType' - The resource type of a resource, such as @AWS::EC2::Instance@ .
mkResourceIdentifier ::
  ResourceIdentifier
mkResourceIdentifier =
  ResourceIdentifier'
    { resourceType = Lude.Nothing,
      resourceARN = Lude.Nothing
    }

-- | The resource type of a resource, such as @AWS::EC2::Instance@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResourceType :: Lens.Lens' ResourceIdentifier (Lude.Maybe Lude.Text)
riResourceType = Lens.lens (resourceType :: ResourceIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ResourceIdentifier)
{-# DEPRECATED riResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ARN of a resource.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResourceARN :: Lens.Lens' ResourceIdentifier (Lude.Maybe Lude.Text)
riResourceARN = Lens.lens (resourceARN :: ResourceIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: ResourceIdentifier)
{-# DEPRECATED riResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.FromJSON ResourceIdentifier where
  parseJSON =
    Lude.withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            Lude.<$> (x Lude..:? "ResourceType") Lude.<*> (x Lude..:? "ResourceArn")
      )
