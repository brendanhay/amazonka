{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.OnPremisesTagSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.OnPremisesTagSet
  ( OnPremisesTagSet (..)
  -- * Smart constructor
  , mkOnPremisesTagSet
  -- * Lenses
  , optsOnPremisesTagSetList
  ) where

import qualified Network.AWS.CodeDeploy.Types.TagFilter as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about groups of on-premises instance tags.
--
-- /See:/ 'mkOnPremisesTagSet' smart constructor.
newtype OnPremisesTagSet = OnPremisesTagSet'
  { onPremisesTagSetList :: Core.Maybe [[Types.TagFilter]]
    -- ^ A list that contains other lists of on-premises instance tag groups. For an instance to be included in the deployment group, it must be identified by all of the tag groups in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OnPremisesTagSet' value with any optional fields omitted.
mkOnPremisesTagSet
    :: OnPremisesTagSet
mkOnPremisesTagSet
  = OnPremisesTagSet'{onPremisesTagSetList = Core.Nothing}

-- | A list that contains other lists of on-premises instance tag groups. For an instance to be included in the deployment group, it must be identified by all of the tag groups in the list.
--
-- /Note:/ Consider using 'onPremisesTagSetList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
optsOnPremisesTagSetList :: Lens.Lens' OnPremisesTagSet (Core.Maybe [[Types.TagFilter]])
optsOnPremisesTagSetList = Lens.field @"onPremisesTagSetList"
{-# INLINEABLE optsOnPremisesTagSetList #-}
{-# DEPRECATED onPremisesTagSetList "Use generic-lens or generic-optics with 'onPremisesTagSetList' instead"  #-}

instance Core.FromJSON OnPremisesTagSet where
        toJSON OnPremisesTagSet{..}
          = Core.object
              (Core.catMaybes
                 [("onPremisesTagSetList" Core..=) Core.<$> onPremisesTagSetList])

instance Core.FromJSON OnPremisesTagSet where
        parseJSON
          = Core.withObject "OnPremisesTagSet" Core.$
              \ x ->
                OnPremisesTagSet' Core.<$> (x Core..:? "onPremisesTagSetList")
