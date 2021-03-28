{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AwsVPCSecurityGroupViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.AwsVPCSecurityGroupViolation
  ( AwsVPCSecurityGroupViolation (..)
  -- * Smart constructor
  , mkAwsVPCSecurityGroupViolation
  -- * Lenses
  , avpcsgvPartialMatches
  , avpcsgvPossibleSecurityGroupRemediationActions
  , avpcsgvViolationTarget
  , avpcsgvViolationTargetDescription
  ) where

import qualified Network.AWS.FMS.Types.LengthBoundedString as Types
import qualified Network.AWS.FMS.Types.PartialMatch as Types
import qualified Network.AWS.FMS.Types.SecurityGroupRemediationAction as Types
import qualified Network.AWS.FMS.Types.ViolationTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details of the rule violation in a security group when compared to the master security group of the AWS Firewall Manager policy.
--
-- /See:/ 'mkAwsVPCSecurityGroupViolation' smart constructor.
data AwsVPCSecurityGroupViolation = AwsVPCSecurityGroupViolation'
  { partialMatches :: Core.Maybe [Types.PartialMatch]
    -- ^ List of rules specified in the security group of the AWS Firewall Manager policy that partially match the @ViolationTarget@ rule.
  , possibleSecurityGroupRemediationActions :: Core.Maybe [Types.SecurityGroupRemediationAction]
    -- ^ Remediation options for the rule specified in the @ViolationTarget@ .
  , violationTarget :: Core.Maybe Types.ViolationTarget
    -- ^ The security group rule that is being evaluated.
  , violationTargetDescription :: Core.Maybe Types.LengthBoundedString
    -- ^ A description of the security group that violates the policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AwsVPCSecurityGroupViolation' value with any optional fields omitted.
mkAwsVPCSecurityGroupViolation
    :: AwsVPCSecurityGroupViolation
mkAwsVPCSecurityGroupViolation
  = AwsVPCSecurityGroupViolation'{partialMatches = Core.Nothing,
                                  possibleSecurityGroupRemediationActions = Core.Nothing,
                                  violationTarget = Core.Nothing,
                                  violationTargetDescription = Core.Nothing}

-- | List of rules specified in the security group of the AWS Firewall Manager policy that partially match the @ViolationTarget@ rule.
--
-- /Note:/ Consider using 'partialMatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcsgvPartialMatches :: Lens.Lens' AwsVPCSecurityGroupViolation (Core.Maybe [Types.PartialMatch])
avpcsgvPartialMatches = Lens.field @"partialMatches"
{-# INLINEABLE avpcsgvPartialMatches #-}
{-# DEPRECATED partialMatches "Use generic-lens or generic-optics with 'partialMatches' instead"  #-}

-- | Remediation options for the rule specified in the @ViolationTarget@ .
--
-- /Note:/ Consider using 'possibleSecurityGroupRemediationActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcsgvPossibleSecurityGroupRemediationActions :: Lens.Lens' AwsVPCSecurityGroupViolation (Core.Maybe [Types.SecurityGroupRemediationAction])
avpcsgvPossibleSecurityGroupRemediationActions = Lens.field @"possibleSecurityGroupRemediationActions"
{-# INLINEABLE avpcsgvPossibleSecurityGroupRemediationActions #-}
{-# DEPRECATED possibleSecurityGroupRemediationActions "Use generic-lens or generic-optics with 'possibleSecurityGroupRemediationActions' instead"  #-}

-- | The security group rule that is being evaluated.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcsgvViolationTarget :: Lens.Lens' AwsVPCSecurityGroupViolation (Core.Maybe Types.ViolationTarget)
avpcsgvViolationTarget = Lens.field @"violationTarget"
{-# INLINEABLE avpcsgvViolationTarget #-}
{-# DEPRECATED violationTarget "Use generic-lens or generic-optics with 'violationTarget' instead"  #-}

-- | A description of the security group that violates the policy.
--
-- /Note:/ Consider using 'violationTargetDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcsgvViolationTargetDescription :: Lens.Lens' AwsVPCSecurityGroupViolation (Core.Maybe Types.LengthBoundedString)
avpcsgvViolationTargetDescription = Lens.field @"violationTargetDescription"
{-# INLINEABLE avpcsgvViolationTargetDescription #-}
{-# DEPRECATED violationTargetDescription "Use generic-lens or generic-optics with 'violationTargetDescription' instead"  #-}

instance Core.FromJSON AwsVPCSecurityGroupViolation where
        parseJSON
          = Core.withObject "AwsVPCSecurityGroupViolation" Core.$
              \ x ->
                AwsVPCSecurityGroupViolation' Core.<$>
                  (x Core..:? "PartialMatches") Core.<*>
                    x Core..:? "PossibleSecurityGroupRemediationActions"
                    Core.<*> x Core..:? "ViolationTarget"
                    Core.<*> x Core..:? "ViolationTargetDescription"
