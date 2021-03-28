{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EC2InstanceLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.EC2InstanceLimit
  ( EC2InstanceLimit (..)
  -- * Smart constructor
  , mkEC2InstanceLimit
  -- * Lenses
  , ecilCurrentInstances
  , ecilEC2InstanceType
  , ecilInstanceLimit
  ) where

import qualified Network.AWS.GameLift.Types.EC2InstanceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The maximum number of instances allowed based on the Amazon Elastic Compute Cloud (Amazon EC2) instance type. Instance limits can be retrieved by calling 'DescribeEC2InstanceLimits' .
--
-- /See:/ 'mkEC2InstanceLimit' smart constructor.
data EC2InstanceLimit = EC2InstanceLimit'
  { currentInstances :: Core.Maybe Core.Natural
    -- ^ Number of instances of the specified type that are currently in use by this AWS account.
  , eC2InstanceType :: Core.Maybe Types.EC2InstanceType
    -- ^ Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
  , instanceLimit :: Core.Maybe Core.Natural
    -- ^ Number of instances allowed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EC2InstanceLimit' value with any optional fields omitted.
mkEC2InstanceLimit
    :: EC2InstanceLimit
mkEC2InstanceLimit
  = EC2InstanceLimit'{currentInstances = Core.Nothing,
                      eC2InstanceType = Core.Nothing, instanceLimit = Core.Nothing}

-- | Number of instances of the specified type that are currently in use by this AWS account.
--
-- /Note:/ Consider using 'currentInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecilCurrentInstances :: Lens.Lens' EC2InstanceLimit (Core.Maybe Core.Natural)
ecilCurrentInstances = Lens.field @"currentInstances"
{-# INLINEABLE ecilCurrentInstances #-}
{-# DEPRECATED currentInstances "Use generic-lens or generic-optics with 'currentInstances' instead"  #-}

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- /Note:/ Consider using 'eC2InstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecilEC2InstanceType :: Lens.Lens' EC2InstanceLimit (Core.Maybe Types.EC2InstanceType)
ecilEC2InstanceType = Lens.field @"eC2InstanceType"
{-# INLINEABLE ecilEC2InstanceType #-}
{-# DEPRECATED eC2InstanceType "Use generic-lens or generic-optics with 'eC2InstanceType' instead"  #-}

-- | Number of instances allowed.
--
-- /Note:/ Consider using 'instanceLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecilInstanceLimit :: Lens.Lens' EC2InstanceLimit (Core.Maybe Core.Natural)
ecilInstanceLimit = Lens.field @"instanceLimit"
{-# INLINEABLE ecilInstanceLimit #-}
{-# DEPRECATED instanceLimit "Use generic-lens or generic-optics with 'instanceLimit' instead"  #-}

instance Core.FromJSON EC2InstanceLimit where
        parseJSON
          = Core.withObject "EC2InstanceLimit" Core.$
              \ x ->
                EC2InstanceLimit' Core.<$>
                  (x Core..:? "CurrentInstances") Core.<*>
                    x Core..:? "EC2InstanceType"
                    Core.<*> x Core..:? "InstanceLimit"
