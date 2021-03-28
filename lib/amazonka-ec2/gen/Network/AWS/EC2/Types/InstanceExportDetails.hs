{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceExportDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceExportDetails
  ( InstanceExportDetails (..)
  -- * Smart constructor
  , mkInstanceExportDetails
  -- * Lenses
  , iedInstanceId
  , iedTargetEnvironment
  ) where

import qualified Network.AWS.EC2.Types.ExportEnvironment as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance to export.
--
-- /See:/ 'mkInstanceExportDetails' smart constructor.
data InstanceExportDetails = InstanceExportDetails'
  { instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the resource being exported.
  , targetEnvironment :: Core.Maybe Types.ExportEnvironment
    -- ^ The target virtualization environment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceExportDetails' value with any optional fields omitted.
mkInstanceExportDetails
    :: InstanceExportDetails
mkInstanceExportDetails
  = InstanceExportDetails'{instanceId = Core.Nothing,
                           targetEnvironment = Core.Nothing}

-- | The ID of the resource being exported.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iedInstanceId :: Lens.Lens' InstanceExportDetails (Core.Maybe Core.Text)
iedInstanceId = Lens.field @"instanceId"
{-# INLINEABLE iedInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The target virtualization environment.
--
-- /Note:/ Consider using 'targetEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iedTargetEnvironment :: Lens.Lens' InstanceExportDetails (Core.Maybe Types.ExportEnvironment)
iedTargetEnvironment = Lens.field @"targetEnvironment"
{-# INLINEABLE iedTargetEnvironment #-}
{-# DEPRECATED targetEnvironment "Use generic-lens or generic-optics with 'targetEnvironment' instead"  #-}

instance Core.FromXML InstanceExportDetails where
        parseXML x
          = InstanceExportDetails' Core.<$>
              (x Core..@? "instanceId") Core.<*> x Core..@? "targetEnvironment"
