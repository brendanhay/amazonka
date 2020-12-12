{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ResourceGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ResourceGroup
  ( ResourceGroup (..),

    -- * Smart constructor
    mkResourceGroup,

    -- * Lenses
    rgArn,
    rgTags,
    rgCreatedAt,
  )
where

import Network.AWS.Inspector.Types.ResourceGroupTag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a resource group. The resource group defines a set of tags that, when queried, identify the AWS resources that make up the assessment target. This data type is used as the response element in the 'DescribeResourceGroups' action.
--
-- /See:/ 'mkResourceGroup' smart constructor.
data ResourceGroup = ResourceGroup'
  { arn :: Lude.Text,
    tags :: Lude.NonEmpty ResourceGroupTag,
    createdAt :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceGroup' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the resource group.
-- * 'createdAt' - The time at which resource group is created.
-- * 'tags' - The tags (key and value pairs) of the resource group. This data type property is used in the 'CreateResourceGroup' action.
mkResourceGroup ::
  -- | 'arn'
  Lude.Text ->
  -- | 'tags'
  Lude.NonEmpty ResourceGroupTag ->
  -- | 'createdAt'
  Lude.Timestamp ->
  ResourceGroup
mkResourceGroup pArn_ pTags_ pCreatedAt_ =
  ResourceGroup'
    { arn = pArn_,
      tags = pTags_,
      createdAt = pCreatedAt_
    }

-- | The ARN of the resource group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgArn :: Lens.Lens' ResourceGroup Lude.Text
rgArn = Lens.lens (arn :: ResourceGroup -> Lude.Text) (\s a -> s {arn = a} :: ResourceGroup)
{-# DEPRECATED rgArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The tags (key and value pairs) of the resource group. This data type property is used in the 'CreateResourceGroup' action.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgTags :: Lens.Lens' ResourceGroup (Lude.NonEmpty ResourceGroupTag)
rgTags = Lens.lens (tags :: ResourceGroup -> Lude.NonEmpty ResourceGroupTag) (\s a -> s {tags = a} :: ResourceGroup)
{-# DEPRECATED rgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The time at which resource group is created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgCreatedAt :: Lens.Lens' ResourceGroup Lude.Timestamp
rgCreatedAt = Lens.lens (createdAt :: ResourceGroup -> Lude.Timestamp) (\s a -> s {createdAt = a} :: ResourceGroup)
{-# DEPRECATED rgCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

instance Lude.FromJSON ResourceGroup where
  parseJSON =
    Lude.withObject
      "ResourceGroup"
      ( \x ->
          ResourceGroup'
            Lude.<$> (x Lude..: "arn")
            Lude.<*> (x Lude..: "tags")
            Lude.<*> (x Lude..: "createdAt")
      )
