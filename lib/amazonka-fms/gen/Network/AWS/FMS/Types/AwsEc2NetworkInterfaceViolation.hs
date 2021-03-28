{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.AwsEc2NetworkInterfaceViolation
  ( AwsEc2NetworkInterfaceViolation (..)
  -- * Smart constructor
  , mkAwsEc2NetworkInterfaceViolation
  -- * Lenses
  , aenivViolatingSecurityGroups
  , aenivViolationTarget
  ) where

import qualified Network.AWS.FMS.Types.ResourceId as Types
import qualified Network.AWS.FMS.Types.ViolationTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Violations for network interfaces associated with an EC2 instance.
--
-- /See:/ 'mkAwsEc2NetworkInterfaceViolation' smart constructor.
data AwsEc2NetworkInterfaceViolation = AwsEc2NetworkInterfaceViolation'
  { violatingSecurityGroups :: Core.Maybe [Types.ResourceId]
    -- ^ List of security groups that violate the rules specified in the master security group of the AWS Firewall Manager policy.
  , violationTarget :: Core.Maybe Types.ViolationTarget
    -- ^ The resource ID of the network interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AwsEc2NetworkInterfaceViolation' value with any optional fields omitted.
mkAwsEc2NetworkInterfaceViolation
    :: AwsEc2NetworkInterfaceViolation
mkAwsEc2NetworkInterfaceViolation
  = AwsEc2NetworkInterfaceViolation'{violatingSecurityGroups =
                                       Core.Nothing,
                                     violationTarget = Core.Nothing}

-- | List of security groups that violate the rules specified in the master security group of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'violatingSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aenivViolatingSecurityGroups :: Lens.Lens' AwsEc2NetworkInterfaceViolation (Core.Maybe [Types.ResourceId])
aenivViolatingSecurityGroups = Lens.field @"violatingSecurityGroups"
{-# INLINEABLE aenivViolatingSecurityGroups #-}
{-# DEPRECATED violatingSecurityGroups "Use generic-lens or generic-optics with 'violatingSecurityGroups' instead"  #-}

-- | The resource ID of the network interface.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aenivViolationTarget :: Lens.Lens' AwsEc2NetworkInterfaceViolation (Core.Maybe Types.ViolationTarget)
aenivViolationTarget = Lens.field @"violationTarget"
{-# INLINEABLE aenivViolationTarget #-}
{-# DEPRECATED violationTarget "Use generic-lens or generic-optics with 'violationTarget' instead"  #-}

instance Core.FromJSON AwsEc2NetworkInterfaceViolation where
        parseJSON
          = Core.withObject "AwsEc2NetworkInterfaceViolation" Core.$
              \ x ->
                AwsEc2NetworkInterfaceViolation' Core.<$>
                  (x Core..:? "ViolatingSecurityGroups") Core.<*>
                    x Core..:? "ViolationTarget"
