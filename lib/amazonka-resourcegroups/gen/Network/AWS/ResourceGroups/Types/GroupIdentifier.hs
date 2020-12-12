{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupIdentifier
  ( GroupIdentifier (..),

    -- * Smart constructor
    mkGroupIdentifier,

    -- * Lenses
    giGroupARN,
    giGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The unique identifiers for a resource group.
--
-- /See:/ 'mkGroupIdentifier' smart constructor.
data GroupIdentifier = GroupIdentifier'
  { groupARN ::
      Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupIdentifier' with the minimum fields required to make a request.
--
-- * 'groupARN' - The ARN of the resource group.
-- * 'groupName' - The name of the resource group.
mkGroupIdentifier ::
  GroupIdentifier
mkGroupIdentifier =
  GroupIdentifier'
    { groupARN = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | The ARN of the resource group.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGroupARN :: Lens.Lens' GroupIdentifier (Lude.Maybe Lude.Text)
giGroupARN = Lens.lens (groupARN :: GroupIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: GroupIdentifier)
{-# DEPRECATED giGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The name of the resource group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGroupName :: Lens.Lens' GroupIdentifier (Lude.Maybe Lude.Text)
giGroupName = Lens.lens (groupName :: GroupIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: GroupIdentifier)
{-# DEPRECATED giGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromJSON GroupIdentifier where
  parseJSON =
    Lude.withObject
      "GroupIdentifier"
      ( \x ->
          GroupIdentifier'
            Lude.<$> (x Lude..:? "GroupArn") Lude.<*> (x Lude..:? "GroupName")
      )
