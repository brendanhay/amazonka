-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.EC2TagSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.EC2TagSet
  ( EC2TagSet (..),

    -- * Smart constructor
    mkEC2TagSet,

    -- * Lenses
    etsEc2TagSetList,
  )
where

import Network.AWS.CodeDeploy.Types.EC2TagFilter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about groups of EC2 instance tags.
--
-- /See:/ 'mkEC2TagSet' smart constructor.
newtype EC2TagSet = EC2TagSet'
  { ec2TagSetList ::
      Lude.Maybe [[EC2TagFilter]]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EC2TagSet' with the minimum fields required to make a request.
--
-- * 'ec2TagSetList' - A list that contains other lists of EC2 instance tag groups. For an instance to be included in the deployment group, it must be identified by all of the tag groups in the list.
mkEC2TagSet ::
  EC2TagSet
mkEC2TagSet = EC2TagSet' {ec2TagSetList = Lude.Nothing}

-- | A list that contains other lists of EC2 instance tag groups. For an instance to be included in the deployment group, it must be identified by all of the tag groups in the list.
--
-- /Note:/ Consider using 'ec2TagSetList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etsEc2TagSetList :: Lens.Lens' EC2TagSet (Lude.Maybe [[EC2TagFilter]])
etsEc2TagSetList = Lens.lens (ec2TagSetList :: EC2TagSet -> Lude.Maybe [[EC2TagFilter]]) (\s a -> s {ec2TagSetList = a} :: EC2TagSet)
{-# DEPRECATED etsEc2TagSetList "Use generic-lens or generic-optics with 'ec2TagSetList' instead." #-}

instance Lude.FromJSON EC2TagSet where
  parseJSON =
    Lude.withObject
      "EC2TagSet"
      ( \x ->
          EC2TagSet'
            Lude.<$> (x Lude..:? "ec2TagSetList" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON EC2TagSet where
  toJSON EC2TagSet' {..} =
    Lude.object
      (Lude.catMaybes [("ec2TagSetList" Lude..=) Lude.<$> ec2TagSetList])
