{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AwsEc2InstanceViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.AwsEc2InstanceViolation
  ( AwsEc2InstanceViolation (..)
  -- * Smart constructor
  , mkAwsEc2InstanceViolation
  -- * Lenses
  , aeivAwsEc2NetworkInterfaceViolations
  , aeivViolationTarget
  ) where

import qualified Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation as Types
import qualified Network.AWS.FMS.Types.ViolationTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Violations for an EC2 instance resource.
--
-- /See:/ 'mkAwsEc2InstanceViolation' smart constructor.
data AwsEc2InstanceViolation = AwsEc2InstanceViolation'
  { awsEc2NetworkInterfaceViolations :: Core.Maybe [Types.AwsEc2NetworkInterfaceViolation]
    -- ^ Violations for network interfaces associated with the EC2 instance.
  , violationTarget :: Core.Maybe Types.ViolationTarget
    -- ^ The resource ID of the EC2 instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AwsEc2InstanceViolation' value with any optional fields omitted.
mkAwsEc2InstanceViolation
    :: AwsEc2InstanceViolation
mkAwsEc2InstanceViolation
  = AwsEc2InstanceViolation'{awsEc2NetworkInterfaceViolations =
                               Core.Nothing,
                             violationTarget = Core.Nothing}

-- | Violations for network interfaces associated with the EC2 instance.
--
-- /Note:/ Consider using 'awsEc2NetworkInterfaceViolations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeivAwsEc2NetworkInterfaceViolations :: Lens.Lens' AwsEc2InstanceViolation (Core.Maybe [Types.AwsEc2NetworkInterfaceViolation])
aeivAwsEc2NetworkInterfaceViolations = Lens.field @"awsEc2NetworkInterfaceViolations"
{-# INLINEABLE aeivAwsEc2NetworkInterfaceViolations #-}
{-# DEPRECATED awsEc2NetworkInterfaceViolations "Use generic-lens or generic-optics with 'awsEc2NetworkInterfaceViolations' instead"  #-}

-- | The resource ID of the EC2 instance.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeivViolationTarget :: Lens.Lens' AwsEc2InstanceViolation (Core.Maybe Types.ViolationTarget)
aeivViolationTarget = Lens.field @"violationTarget"
{-# INLINEABLE aeivViolationTarget #-}
{-# DEPRECATED violationTarget "Use generic-lens or generic-optics with 'violationTarget' instead"  #-}

instance Core.FromJSON AwsEc2InstanceViolation where
        parseJSON
          = Core.withObject "AwsEc2InstanceViolation" Core.$
              \ x ->
                AwsEc2InstanceViolation' Core.<$>
                  (x Core..:? "AwsEc2NetworkInterfaceViolations") Core.<*>
                    x Core..:? "ViolationTarget"
