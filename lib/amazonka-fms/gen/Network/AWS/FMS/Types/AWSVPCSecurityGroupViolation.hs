{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AWSVPCSecurityGroupViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AWSVPCSecurityGroupViolation
  ( AWSVPCSecurityGroupViolation (..),

    -- * Smart constructor
    mkAWSVPCSecurityGroupViolation,

    -- * Lenses
    avsgvViolationTargetDescription,
    avsgvPossibleSecurityGroupRemediationActions,
    avsgvViolationTarget,
    avsgvPartialMatches,
  )
where

import Network.AWS.FMS.Types.PartialMatch
import Network.AWS.FMS.Types.SecurityGroupRemediationAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of the rule violation in a security group when compared to the master security group of the AWS Firewall Manager policy.
--
-- /See:/ 'mkAWSVPCSecurityGroupViolation' smart constructor.
data AWSVPCSecurityGroupViolation = AWSVPCSecurityGroupViolation'
  { -- | A description of the security group that violates the policy.
    violationTargetDescription :: Lude.Maybe Lude.Text,
    -- | Remediation options for the rule specified in the @ViolationTarget@ .
    possibleSecurityGroupRemediationActions :: Lude.Maybe [SecurityGroupRemediationAction],
    -- | The security group rule that is being evaluated.
    violationTarget :: Lude.Maybe Lude.Text,
    -- | List of rules specified in the security group of the AWS Firewall Manager policy that partially match the @ViolationTarget@ rule.
    partialMatches :: Lude.Maybe [PartialMatch]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSVPCSecurityGroupViolation' with the minimum fields required to make a request.
--
-- * 'violationTargetDescription' - A description of the security group that violates the policy.
-- * 'possibleSecurityGroupRemediationActions' - Remediation options for the rule specified in the @ViolationTarget@ .
-- * 'violationTarget' - The security group rule that is being evaluated.
-- * 'partialMatches' - List of rules specified in the security group of the AWS Firewall Manager policy that partially match the @ViolationTarget@ rule.
mkAWSVPCSecurityGroupViolation ::
  AWSVPCSecurityGroupViolation
mkAWSVPCSecurityGroupViolation =
  AWSVPCSecurityGroupViolation'
    { violationTargetDescription =
        Lude.Nothing,
      possibleSecurityGroupRemediationActions = Lude.Nothing,
      violationTarget = Lude.Nothing,
      partialMatches = Lude.Nothing
    }

-- | A description of the security group that violates the policy.
--
-- /Note:/ Consider using 'violationTargetDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avsgvViolationTargetDescription :: Lens.Lens' AWSVPCSecurityGroupViolation (Lude.Maybe Lude.Text)
avsgvViolationTargetDescription = Lens.lens (violationTargetDescription :: AWSVPCSecurityGroupViolation -> Lude.Maybe Lude.Text) (\s a -> s {violationTargetDescription = a} :: AWSVPCSecurityGroupViolation)
{-# DEPRECATED avsgvViolationTargetDescription "Use generic-lens or generic-optics with 'violationTargetDescription' instead." #-}

-- | Remediation options for the rule specified in the @ViolationTarget@ .
--
-- /Note:/ Consider using 'possibleSecurityGroupRemediationActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avsgvPossibleSecurityGroupRemediationActions :: Lens.Lens' AWSVPCSecurityGroupViolation (Lude.Maybe [SecurityGroupRemediationAction])
avsgvPossibleSecurityGroupRemediationActions = Lens.lens (possibleSecurityGroupRemediationActions :: AWSVPCSecurityGroupViolation -> Lude.Maybe [SecurityGroupRemediationAction]) (\s a -> s {possibleSecurityGroupRemediationActions = a} :: AWSVPCSecurityGroupViolation)
{-# DEPRECATED avsgvPossibleSecurityGroupRemediationActions "Use generic-lens or generic-optics with 'possibleSecurityGroupRemediationActions' instead." #-}

-- | The security group rule that is being evaluated.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avsgvViolationTarget :: Lens.Lens' AWSVPCSecurityGroupViolation (Lude.Maybe Lude.Text)
avsgvViolationTarget = Lens.lens (violationTarget :: AWSVPCSecurityGroupViolation -> Lude.Maybe Lude.Text) (\s a -> s {violationTarget = a} :: AWSVPCSecurityGroupViolation)
{-# DEPRECATED avsgvViolationTarget "Use generic-lens or generic-optics with 'violationTarget' instead." #-}

-- | List of rules specified in the security group of the AWS Firewall Manager policy that partially match the @ViolationTarget@ rule.
--
-- /Note:/ Consider using 'partialMatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avsgvPartialMatches :: Lens.Lens' AWSVPCSecurityGroupViolation (Lude.Maybe [PartialMatch])
avsgvPartialMatches = Lens.lens (partialMatches :: AWSVPCSecurityGroupViolation -> Lude.Maybe [PartialMatch]) (\s a -> s {partialMatches = a} :: AWSVPCSecurityGroupViolation)
{-# DEPRECATED avsgvPartialMatches "Use generic-lens or generic-optics with 'partialMatches' instead." #-}

instance Lude.FromJSON AWSVPCSecurityGroupViolation where
  parseJSON =
    Lude.withObject
      "AWSVPCSecurityGroupViolation"
      ( \x ->
          AWSVPCSecurityGroupViolation'
            Lude.<$> (x Lude..:? "ViolationTargetDescription")
            Lude.<*> ( x Lude..:? "PossibleSecurityGroupRemediationActions"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..:? "ViolationTarget")
            Lude.<*> (x Lude..:? "PartialMatches" Lude..!= Lude.mempty)
      )
