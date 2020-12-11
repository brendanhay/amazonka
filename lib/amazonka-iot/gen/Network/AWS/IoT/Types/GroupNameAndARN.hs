-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.GroupNameAndARN
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.GroupNameAndARN
  ( GroupNameAndARN (..),

    -- * Smart constructor
    mkGroupNameAndARN,

    -- * Lenses
    gnaaGroupARN,
    gnaaGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The name and ARN of a group.
--
-- /See:/ 'mkGroupNameAndARN' smart constructor.
data GroupNameAndARN = GroupNameAndARN'
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

-- | Creates a value of 'GroupNameAndARN' with the minimum fields required to make a request.
--
-- * 'groupARN' - The group ARN.
-- * 'groupName' - The group name.
mkGroupNameAndARN ::
  GroupNameAndARN
mkGroupNameAndARN =
  GroupNameAndARN'
    { groupARN = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | The group ARN.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnaaGroupARN :: Lens.Lens' GroupNameAndARN (Lude.Maybe Lude.Text)
gnaaGroupARN = Lens.lens (groupARN :: GroupNameAndARN -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: GroupNameAndARN)
{-# DEPRECATED gnaaGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The group name.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnaaGroupName :: Lens.Lens' GroupNameAndARN (Lude.Maybe Lude.Text)
gnaaGroupName = Lens.lens (groupName :: GroupNameAndARN -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: GroupNameAndARN)
{-# DEPRECATED gnaaGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromJSON GroupNameAndARN where
  parseJSON =
    Lude.withObject
      "GroupNameAndARN"
      ( \x ->
          GroupNameAndARN'
            Lude.<$> (x Lude..:? "groupArn") Lude.<*> (x Lude..:? "groupName")
      )
