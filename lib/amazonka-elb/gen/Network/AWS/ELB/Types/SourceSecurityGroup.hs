-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.SourceSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.SourceSecurityGroup
  ( SourceSecurityGroup (..),

    -- * Smart constructor
    mkSourceSecurityGroup,

    -- * Lenses
    ssgOwnerAlias,
    ssgGroupName,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a source security group.
--
-- /See:/ 'mkSourceSecurityGroup' smart constructor.
data SourceSecurityGroup = SourceSecurityGroup'
  { ownerAlias ::
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

-- | Creates a value of 'SourceSecurityGroup' with the minimum fields required to make a request.
--
-- * 'groupName' - The name of the security group.
-- * 'ownerAlias' - The owner of the security group.
mkSourceSecurityGroup ::
  SourceSecurityGroup
mkSourceSecurityGroup =
  SourceSecurityGroup'
    { ownerAlias = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | The owner of the security group.
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgOwnerAlias :: Lens.Lens' SourceSecurityGroup (Lude.Maybe Lude.Text)
ssgOwnerAlias = Lens.lens (ownerAlias :: SourceSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {ownerAlias = a} :: SourceSecurityGroup)
{-# DEPRECATED ssgOwnerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead." #-}

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgGroupName :: Lens.Lens' SourceSecurityGroup (Lude.Maybe Lude.Text)
ssgGroupName = Lens.lens (groupName :: SourceSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: SourceSecurityGroup)
{-# DEPRECATED ssgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromXML SourceSecurityGroup where
  parseXML x =
    SourceSecurityGroup'
      Lude.<$> (x Lude..@? "OwnerAlias") Lude.<*> (x Lude..@? "GroupName")
