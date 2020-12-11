-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AWSEC2NetworkInterfaceViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AWSEC2NetworkInterfaceViolation
  ( AWSEC2NetworkInterfaceViolation (..),

    -- * Smart constructor
    mkAWSEC2NetworkInterfaceViolation,

    -- * Lenses
    aenivViolatingSecurityGroups,
    aenivViolationTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Violations for network interfaces associated with an EC2 instance.
--
-- /See:/ 'mkAWSEC2NetworkInterfaceViolation' smart constructor.
data AWSEC2NetworkInterfaceViolation = AWSEC2NetworkInterfaceViolation'
  { violatingSecurityGroups ::
      Lude.Maybe [Lude.Text],
    violationTarget ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSEC2NetworkInterfaceViolation' with the minimum fields required to make a request.
--
-- * 'violatingSecurityGroups' - List of security groups that violate the rules specified in the master security group of the AWS Firewall Manager policy.
-- * 'violationTarget' - The resource ID of the network interface.
mkAWSEC2NetworkInterfaceViolation ::
  AWSEC2NetworkInterfaceViolation
mkAWSEC2NetworkInterfaceViolation =
  AWSEC2NetworkInterfaceViolation'
    { violatingSecurityGroups =
        Lude.Nothing,
      violationTarget = Lude.Nothing
    }

-- | List of security groups that violate the rules specified in the master security group of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'violatingSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aenivViolatingSecurityGroups :: Lens.Lens' AWSEC2NetworkInterfaceViolation (Lude.Maybe [Lude.Text])
aenivViolatingSecurityGroups = Lens.lens (violatingSecurityGroups :: AWSEC2NetworkInterfaceViolation -> Lude.Maybe [Lude.Text]) (\s a -> s {violatingSecurityGroups = a} :: AWSEC2NetworkInterfaceViolation)
{-# DEPRECATED aenivViolatingSecurityGroups "Use generic-lens or generic-optics with 'violatingSecurityGroups' instead." #-}

-- | The resource ID of the network interface.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aenivViolationTarget :: Lens.Lens' AWSEC2NetworkInterfaceViolation (Lude.Maybe Lude.Text)
aenivViolationTarget = Lens.lens (violationTarget :: AWSEC2NetworkInterfaceViolation -> Lude.Maybe Lude.Text) (\s a -> s {violationTarget = a} :: AWSEC2NetworkInterfaceViolation)
{-# DEPRECATED aenivViolationTarget "Use generic-lens or generic-optics with 'violationTarget' instead." #-}

instance Lude.FromJSON AWSEC2NetworkInterfaceViolation where
  parseJSON =
    Lude.withObject
      "AWSEC2NetworkInterfaceViolation"
      ( \x ->
          AWSEC2NetworkInterfaceViolation'
            Lude.<$> (x Lude..:? "ViolatingSecurityGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ViolationTarget")
      )
