{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.EC2TagSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.EC2TagSet
  ( EC2TagSet (..)
  -- * Smart constructor
  , mkEC2TagSet
  -- * Lenses
  , ectsEc2TagSetList
  ) where

import qualified Network.AWS.CodeDeploy.Types.EC2TagFilter as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about groups of EC2 instance tags.
--
-- /See:/ 'mkEC2TagSet' smart constructor.
newtype EC2TagSet = EC2TagSet'
  { ec2TagSetList :: Core.Maybe [[Types.EC2TagFilter]]
    -- ^ A list that contains other lists of EC2 instance tag groups. For an instance to be included in the deployment group, it must be identified by all of the tag groups in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EC2TagSet' value with any optional fields omitted.
mkEC2TagSet
    :: EC2TagSet
mkEC2TagSet = EC2TagSet'{ec2TagSetList = Core.Nothing}

-- | A list that contains other lists of EC2 instance tag groups. For an instance to be included in the deployment group, it must be identified by all of the tag groups in the list.
--
-- /Note:/ Consider using 'ec2TagSetList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ectsEc2TagSetList :: Lens.Lens' EC2TagSet (Core.Maybe [[Types.EC2TagFilter]])
ectsEc2TagSetList = Lens.field @"ec2TagSetList"
{-# INLINEABLE ectsEc2TagSetList #-}
{-# DEPRECATED ec2TagSetList "Use generic-lens or generic-optics with 'ec2TagSetList' instead"  #-}

instance Core.FromJSON EC2TagSet where
        toJSON EC2TagSet{..}
          = Core.object
              (Core.catMaybes [("ec2TagSetList" Core..=) Core.<$> ec2TagSetList])

instance Core.FromJSON EC2TagSet where
        parseJSON
          = Core.withObject "EC2TagSet" Core.$
              \ x -> EC2TagSet' Core.<$> (x Core..:? "ec2TagSetList")
