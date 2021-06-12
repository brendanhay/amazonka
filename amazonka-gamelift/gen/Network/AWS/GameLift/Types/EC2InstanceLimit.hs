{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EC2InstanceLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.EC2InstanceLimit where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.EC2InstanceType
import qualified Network.AWS.Lens as Lens

-- | The maximum number of instances allowed based on the Amazon Elastic
-- Compute Cloud (Amazon EC2) instance type. Instance limits can be
-- retrieved by calling DescribeEC2InstanceLimits.
--
-- /See:/ 'newEC2InstanceLimit' smart constructor.
data EC2InstanceLimit = EC2InstanceLimit'
  { -- | Number of instances allowed.
    instanceLimit :: Core.Maybe Core.Natural,
    -- | Number of instances of the specified type that are currently in use by
    -- this AWS account.
    currentInstances :: Core.Maybe Core.Natural,
    -- | Name of an EC2 instance type that is supported in Amazon GameLift. A
    -- fleet instance type determines the computing resources of each instance
    -- in the fleet, including CPU, memory, storage, and networking capacity.
    -- Amazon GameLift supports the following EC2 instance types. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
    -- for detailed descriptions.
    eC2InstanceType :: Core.Maybe EC2InstanceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EC2InstanceLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceLimit', 'eC2InstanceLimit_instanceLimit' - Number of instances allowed.
--
-- 'currentInstances', 'eC2InstanceLimit_currentInstances' - Number of instances of the specified type that are currently in use by
-- this AWS account.
--
-- 'eC2InstanceType', 'eC2InstanceLimit_eC2InstanceType' - Name of an EC2 instance type that is supported in Amazon GameLift. A
-- fleet instance type determines the computing resources of each instance
-- in the fleet, including CPU, memory, storage, and networking capacity.
-- Amazon GameLift supports the following EC2 instance types. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions.
newEC2InstanceLimit ::
  EC2InstanceLimit
newEC2InstanceLimit =
  EC2InstanceLimit'
    { instanceLimit = Core.Nothing,
      currentInstances = Core.Nothing,
      eC2InstanceType = Core.Nothing
    }

-- | Number of instances allowed.
eC2InstanceLimit_instanceLimit :: Lens.Lens' EC2InstanceLimit (Core.Maybe Core.Natural)
eC2InstanceLimit_instanceLimit = Lens.lens (\EC2InstanceLimit' {instanceLimit} -> instanceLimit) (\s@EC2InstanceLimit' {} a -> s {instanceLimit = a} :: EC2InstanceLimit)

-- | Number of instances of the specified type that are currently in use by
-- this AWS account.
eC2InstanceLimit_currentInstances :: Lens.Lens' EC2InstanceLimit (Core.Maybe Core.Natural)
eC2InstanceLimit_currentInstances = Lens.lens (\EC2InstanceLimit' {currentInstances} -> currentInstances) (\s@EC2InstanceLimit' {} a -> s {currentInstances = a} :: EC2InstanceLimit)

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A
-- fleet instance type determines the computing resources of each instance
-- in the fleet, including CPU, memory, storage, and networking capacity.
-- Amazon GameLift supports the following EC2 instance types. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions.
eC2InstanceLimit_eC2InstanceType :: Lens.Lens' EC2InstanceLimit (Core.Maybe EC2InstanceType)
eC2InstanceLimit_eC2InstanceType = Lens.lens (\EC2InstanceLimit' {eC2InstanceType} -> eC2InstanceType) (\s@EC2InstanceLimit' {} a -> s {eC2InstanceType = a} :: EC2InstanceLimit)

instance Core.FromJSON EC2InstanceLimit where
  parseJSON =
    Core.withObject
      "EC2InstanceLimit"
      ( \x ->
          EC2InstanceLimit'
            Core.<$> (x Core..:? "InstanceLimit")
            Core.<*> (x Core..:? "CurrentInstances")
            Core.<*> (x Core..:? "EC2InstanceType")
      )

instance Core.Hashable EC2InstanceLimit

instance Core.NFData EC2InstanceLimit
