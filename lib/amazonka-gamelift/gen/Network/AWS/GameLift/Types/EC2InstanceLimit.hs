{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EC2InstanceLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.EC2InstanceLimit
  ( EC2InstanceLimit (..),

    -- * Smart constructor
    mkEC2InstanceLimit,

    -- * Lenses
    eilEC2InstanceType,
    eilCurrentInstances,
    eilInstanceLimit,
  )
where

import Network.AWS.GameLift.Types.EC2InstanceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The maximum number of instances allowed based on the Amazon Elastic Compute Cloud (Amazon EC2) instance type. Instance limits can be retrieved by calling 'DescribeEC2InstanceLimits' .
--
-- /See:/ 'mkEC2InstanceLimit' smart constructor.
data EC2InstanceLimit = EC2InstanceLimit'
  { -- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
    ec2InstanceType :: Lude.Maybe EC2InstanceType,
    -- | Number of instances of the specified type that are currently in use by this AWS account.
    currentInstances :: Lude.Maybe Lude.Natural,
    -- | Number of instances allowed.
    instanceLimit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EC2InstanceLimit' with the minimum fields required to make a request.
--
-- * 'ec2InstanceType' - Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
-- * 'currentInstances' - Number of instances of the specified type that are currently in use by this AWS account.
-- * 'instanceLimit' - Number of instances allowed.
mkEC2InstanceLimit ::
  EC2InstanceLimit
mkEC2InstanceLimit =
  EC2InstanceLimit'
    { ec2InstanceType = Lude.Nothing,
      currentInstances = Lude.Nothing,
      instanceLimit = Lude.Nothing
    }

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- /Note:/ Consider using 'ec2InstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eilEC2InstanceType :: Lens.Lens' EC2InstanceLimit (Lude.Maybe EC2InstanceType)
eilEC2InstanceType = Lens.lens (ec2InstanceType :: EC2InstanceLimit -> Lude.Maybe EC2InstanceType) (\s a -> s {ec2InstanceType = a} :: EC2InstanceLimit)
{-# DEPRECATED eilEC2InstanceType "Use generic-lens or generic-optics with 'ec2InstanceType' instead." #-}

-- | Number of instances of the specified type that are currently in use by this AWS account.
--
-- /Note:/ Consider using 'currentInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eilCurrentInstances :: Lens.Lens' EC2InstanceLimit (Lude.Maybe Lude.Natural)
eilCurrentInstances = Lens.lens (currentInstances :: EC2InstanceLimit -> Lude.Maybe Lude.Natural) (\s a -> s {currentInstances = a} :: EC2InstanceLimit)
{-# DEPRECATED eilCurrentInstances "Use generic-lens or generic-optics with 'currentInstances' instead." #-}

-- | Number of instances allowed.
--
-- /Note:/ Consider using 'instanceLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eilInstanceLimit :: Lens.Lens' EC2InstanceLimit (Lude.Maybe Lude.Natural)
eilInstanceLimit = Lens.lens (instanceLimit :: EC2InstanceLimit -> Lude.Maybe Lude.Natural) (\s a -> s {instanceLimit = a} :: EC2InstanceLimit)
{-# DEPRECATED eilInstanceLimit "Use generic-lens or generic-optics with 'instanceLimit' instead." #-}

instance Lude.FromJSON EC2InstanceLimit where
  parseJSON =
    Lude.withObject
      "EC2InstanceLimit"
      ( \x ->
          EC2InstanceLimit'
            Lude.<$> (x Lude..:? "EC2InstanceType")
            Lude.<*> (x Lude..:? "CurrentInstances")
            Lude.<*> (x Lude..:? "InstanceLimit")
      )
