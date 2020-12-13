{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AWSEC2InstanceViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AWSEC2InstanceViolation
  ( AWSEC2InstanceViolation (..),

    -- * Smart constructor
    mkAWSEC2InstanceViolation,

    -- * Lenses
    aeivViolationTarget,
    aeivAWSEC2NetworkInterfaceViolations,
  )
where

import Network.AWS.FMS.Types.AWSEC2NetworkInterfaceViolation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Violations for an EC2 instance resource.
--
-- /See:/ 'mkAWSEC2InstanceViolation' smart constructor.
data AWSEC2InstanceViolation = AWSEC2InstanceViolation'
  { -- | The resource ID of the EC2 instance.
    violationTarget :: Lude.Maybe Lude.Text,
    -- | Violations for network interfaces associated with the EC2 instance.
    awsEC2NetworkInterfaceViolations :: Lude.Maybe [AWSEC2NetworkInterfaceViolation]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSEC2InstanceViolation' with the minimum fields required to make a request.
--
-- * 'violationTarget' - The resource ID of the EC2 instance.
-- * 'awsEC2NetworkInterfaceViolations' - Violations for network interfaces associated with the EC2 instance.
mkAWSEC2InstanceViolation ::
  AWSEC2InstanceViolation
mkAWSEC2InstanceViolation =
  AWSEC2InstanceViolation'
    { violationTarget = Lude.Nothing,
      awsEC2NetworkInterfaceViolations = Lude.Nothing
    }

-- | The resource ID of the EC2 instance.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeivViolationTarget :: Lens.Lens' AWSEC2InstanceViolation (Lude.Maybe Lude.Text)
aeivViolationTarget = Lens.lens (violationTarget :: AWSEC2InstanceViolation -> Lude.Maybe Lude.Text) (\s a -> s {violationTarget = a} :: AWSEC2InstanceViolation)
{-# DEPRECATED aeivViolationTarget "Use generic-lens or generic-optics with 'violationTarget' instead." #-}

-- | Violations for network interfaces associated with the EC2 instance.
--
-- /Note:/ Consider using 'awsEC2NetworkInterfaceViolations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeivAWSEC2NetworkInterfaceViolations :: Lens.Lens' AWSEC2InstanceViolation (Lude.Maybe [AWSEC2NetworkInterfaceViolation])
aeivAWSEC2NetworkInterfaceViolations = Lens.lens (awsEC2NetworkInterfaceViolations :: AWSEC2InstanceViolation -> Lude.Maybe [AWSEC2NetworkInterfaceViolation]) (\s a -> s {awsEC2NetworkInterfaceViolations = a} :: AWSEC2InstanceViolation)
{-# DEPRECATED aeivAWSEC2NetworkInterfaceViolations "Use generic-lens or generic-optics with 'awsEC2NetworkInterfaceViolations' instead." #-}

instance Lude.FromJSON AWSEC2InstanceViolation where
  parseJSON =
    Lude.withObject
      "AWSEC2InstanceViolation"
      ( \x ->
          AWSEC2InstanceViolation'
            Lude.<$> (x Lude..:? "ViolationTarget")
            Lude.<*> ( x Lude..:? "AwsEc2NetworkInterfaceViolations"
                         Lude..!= Lude.mempty
                     )
      )
