{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.Instance
  ( Instance (..)
  -- * Smart constructor
  , mkInstance
  -- * Lenses
  , iId
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.ResourceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The description of an Amazon EC2 instance.
--
-- /See:/ 'mkInstance' smart constructor.
newtype Instance = Instance'
  { id :: Core.Maybe Types.ResourceId
    -- ^ The ID of the Amazon EC2 instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance
    :: Instance
mkInstance = Instance'{id = Core.Nothing}

-- | The ID of the Amazon EC2 instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Instance (Core.Maybe Types.ResourceId)
iId = Lens.field @"id"
{-# INLINEABLE iId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromXML Instance where
        parseXML x = Instance' Core.<$> (x Core..@? "Id")
