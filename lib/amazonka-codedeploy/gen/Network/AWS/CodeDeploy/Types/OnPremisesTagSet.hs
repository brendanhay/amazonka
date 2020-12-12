{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.OnPremisesTagSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.OnPremisesTagSet
  ( OnPremisesTagSet (..),

    -- * Smart constructor
    mkOnPremisesTagSet,

    -- * Lenses
    optsOnPremisesTagSetList,
  )
where

import Network.AWS.CodeDeploy.Types.TagFilter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about groups of on-premises instance tags.
--
-- /See:/ 'mkOnPremisesTagSet' smart constructor.
newtype OnPremisesTagSet = OnPremisesTagSet'
  { onPremisesTagSetList ::
      Lude.Maybe [[TagFilter]]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OnPremisesTagSet' with the minimum fields required to make a request.
--
-- * 'onPremisesTagSetList' - A list that contains other lists of on-premises instance tag groups. For an instance to be included in the deployment group, it must be identified by all of the tag groups in the list.
mkOnPremisesTagSet ::
  OnPremisesTagSet
mkOnPremisesTagSet =
  OnPremisesTagSet' {onPremisesTagSetList = Lude.Nothing}

-- | A list that contains other lists of on-premises instance tag groups. For an instance to be included in the deployment group, it must be identified by all of the tag groups in the list.
--
-- /Note:/ Consider using 'onPremisesTagSetList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
optsOnPremisesTagSetList :: Lens.Lens' OnPremisesTagSet (Lude.Maybe [[TagFilter]])
optsOnPremisesTagSetList = Lens.lens (onPremisesTagSetList :: OnPremisesTagSet -> Lude.Maybe [[TagFilter]]) (\s a -> s {onPremisesTagSetList = a} :: OnPremisesTagSet)
{-# DEPRECATED optsOnPremisesTagSetList "Use generic-lens or generic-optics with 'onPremisesTagSetList' instead." #-}

instance Lude.FromJSON OnPremisesTagSet where
  parseJSON =
    Lude.withObject
      "OnPremisesTagSet"
      ( \x ->
          OnPremisesTagSet'
            Lude.<$> (x Lude..:? "onPremisesTagSetList" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON OnPremisesTagSet where
  toJSON OnPremisesTagSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [("onPremisesTagSetList" Lude..=) Lude.<$> onPremisesTagSetList]
      )
