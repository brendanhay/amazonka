{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceTypeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceTypeInfo
  ( InstanceTypeInfo (..),

    -- * Smart constructor
    mkInstanceTypeInfo,

    -- * Lenses
    itiAutoRecoverySupported,
    itiBareMetal,
    itiBurstablePerformanceSupported,
    itiCurrentGeneration,
    itiDedicatedHostsSupported,
    itiEbsInfo,
    itiFpgaInfo,
    itiFreeTierEligible,
    itiGpuInfo,
    itiHibernationSupported,
    itiHypervisor,
    itiInferenceAcceleratorInfo,
    itiInstanceStorageInfo,
    itiInstanceStorageSupported,
    itiInstanceType,
    itiMemoryInfo,
    itiNetworkInfo,
    itiPlacementGroupInfo,
    itiProcessorInfo,
    itiSupportedRootDeviceTypes,
    itiSupportedUsageClasses,
    itiSupportedVirtualizationTypes,
    itiVCpuInfo,
  )
where

import qualified Network.AWS.EC2.Types.EbsInfo as Types
import qualified Network.AWS.EC2.Types.FpgaInfo as Types
import qualified Network.AWS.EC2.Types.GpuInfo as Types
import qualified Network.AWS.EC2.Types.InferenceAcceleratorInfo as Types
import qualified Network.AWS.EC2.Types.InstanceStorageInfo as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.InstanceTypeHypervisor as Types
import qualified Network.AWS.EC2.Types.MemoryInfo as Types
import qualified Network.AWS.EC2.Types.NetworkInfo as Types
import qualified Network.AWS.EC2.Types.PlacementGroupInfo as Types
import qualified Network.AWS.EC2.Types.ProcessorInfo as Types
import qualified Network.AWS.EC2.Types.RootDeviceType as Types
import qualified Network.AWS.EC2.Types.UsageClassType as Types
import qualified Network.AWS.EC2.Types.VCpuInfo as Types
import qualified Network.AWS.EC2.Types.VirtualizationType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the instance type.
--
-- /See:/ 'mkInstanceTypeInfo' smart constructor.
data InstanceTypeInfo = InstanceTypeInfo'
  { -- | Indicates whether auto recovery is supported.
    autoRecoverySupported :: Core.Maybe Core.Bool,
    -- | Indicates whether the instance is a bare metal instance type.
    bareMetal :: Core.Maybe Core.Bool,
    -- | Indicates whether the instance type is a burstable performance instance type.
    burstablePerformanceSupported :: Core.Maybe Core.Bool,
    -- | Indicates whether the instance type is current generation.
    currentGeneration :: Core.Maybe Core.Bool,
    -- | Indicates whether Dedicated Hosts are supported on the instance type.
    dedicatedHostsSupported :: Core.Maybe Core.Bool,
    -- | Describes the Amazon EBS settings for the instance type.
    ebsInfo :: Core.Maybe Types.EbsInfo,
    -- | Describes the FPGA accelerator settings for the instance type.
    fpgaInfo :: Core.Maybe Types.FpgaInfo,
    -- | Indicates whether the instance type is eligible for the free tier.
    freeTierEligible :: Core.Maybe Core.Bool,
    -- | Describes the GPU accelerator settings for the instance type.
    gpuInfo :: Core.Maybe Types.GpuInfo,
    -- | Indicates whether On-Demand hibernation is supported.
    hibernationSupported :: Core.Maybe Core.Bool,
    -- | The hypervisor for the instance type.
    hypervisor :: Core.Maybe Types.InstanceTypeHypervisor,
    -- | Describes the Inference accelerator settings for the instance type.
    inferenceAcceleratorInfo :: Core.Maybe Types.InferenceAcceleratorInfo,
    -- | Describes the instance storage for the instance type.
    instanceStorageInfo :: Core.Maybe Types.InstanceStorageInfo,
    -- | Indicates whether instance storage is supported.
    instanceStorageSupported :: Core.Maybe Core.Bool,
    -- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
    instanceType :: Core.Maybe Types.InstanceType,
    -- | Describes the memory for the instance type.
    memoryInfo :: Core.Maybe Types.MemoryInfo,
    -- | Describes the network settings for the instance type.
    networkInfo :: Core.Maybe Types.NetworkInfo,
    -- | Describes the placement group settings for the instance type.
    placementGroupInfo :: Core.Maybe Types.PlacementGroupInfo,
    -- | Describes the processor.
    processorInfo :: Core.Maybe Types.ProcessorInfo,
    -- | The supported root device types.
    supportedRootDeviceTypes :: Core.Maybe [Types.RootDeviceType],
    -- | Indicates whether the instance type is offered for spot or On-Demand.
    supportedUsageClasses :: Core.Maybe [Types.UsageClassType],
    -- | The supported virtualization types.
    supportedVirtualizationTypes :: Core.Maybe [Types.VirtualizationType],
    -- | Describes the vCPU configurations for the instance type.
    vCpuInfo :: Core.Maybe Types.VCpuInfo
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceTypeInfo' value with any optional fields omitted.
mkInstanceTypeInfo ::
  InstanceTypeInfo
mkInstanceTypeInfo =
  InstanceTypeInfo'
    { autoRecoverySupported = Core.Nothing,
      bareMetal = Core.Nothing,
      burstablePerformanceSupported = Core.Nothing,
      currentGeneration = Core.Nothing,
      dedicatedHostsSupported = Core.Nothing,
      ebsInfo = Core.Nothing,
      fpgaInfo = Core.Nothing,
      freeTierEligible = Core.Nothing,
      gpuInfo = Core.Nothing,
      hibernationSupported = Core.Nothing,
      hypervisor = Core.Nothing,
      inferenceAcceleratorInfo = Core.Nothing,
      instanceStorageInfo = Core.Nothing,
      instanceStorageSupported = Core.Nothing,
      instanceType = Core.Nothing,
      memoryInfo = Core.Nothing,
      networkInfo = Core.Nothing,
      placementGroupInfo = Core.Nothing,
      processorInfo = Core.Nothing,
      supportedRootDeviceTypes = Core.Nothing,
      supportedUsageClasses = Core.Nothing,
      supportedVirtualizationTypes = Core.Nothing,
      vCpuInfo = Core.Nothing
    }

-- | Indicates whether auto recovery is supported.
--
-- /Note:/ Consider using 'autoRecoverySupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiAutoRecoverySupported :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
itiAutoRecoverySupported = Lens.field @"autoRecoverySupported"
{-# DEPRECATED itiAutoRecoverySupported "Use generic-lens or generic-optics with 'autoRecoverySupported' instead." #-}

-- | Indicates whether the instance is a bare metal instance type.
--
-- /Note:/ Consider using 'bareMetal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiBareMetal :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
itiBareMetal = Lens.field @"bareMetal"
{-# DEPRECATED itiBareMetal "Use generic-lens or generic-optics with 'bareMetal' instead." #-}

-- | Indicates whether the instance type is a burstable performance instance type.
--
-- /Note:/ Consider using 'burstablePerformanceSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiBurstablePerformanceSupported :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
itiBurstablePerformanceSupported = Lens.field @"burstablePerformanceSupported"
{-# DEPRECATED itiBurstablePerformanceSupported "Use generic-lens or generic-optics with 'burstablePerformanceSupported' instead." #-}

-- | Indicates whether the instance type is current generation.
--
-- /Note:/ Consider using 'currentGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiCurrentGeneration :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
itiCurrentGeneration = Lens.field @"currentGeneration"
{-# DEPRECATED itiCurrentGeneration "Use generic-lens or generic-optics with 'currentGeneration' instead." #-}

-- | Indicates whether Dedicated Hosts are supported on the instance type.
--
-- /Note:/ Consider using 'dedicatedHostsSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiDedicatedHostsSupported :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
itiDedicatedHostsSupported = Lens.field @"dedicatedHostsSupported"
{-# DEPRECATED itiDedicatedHostsSupported "Use generic-lens or generic-optics with 'dedicatedHostsSupported' instead." #-}

-- | Describes the Amazon EBS settings for the instance type.
--
-- /Note:/ Consider using 'ebsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiEbsInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe Types.EbsInfo)
itiEbsInfo = Lens.field @"ebsInfo"
{-# DEPRECATED itiEbsInfo "Use generic-lens or generic-optics with 'ebsInfo' instead." #-}

-- | Describes the FPGA accelerator settings for the instance type.
--
-- /Note:/ Consider using 'fpgaInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiFpgaInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe Types.FpgaInfo)
itiFpgaInfo = Lens.field @"fpgaInfo"
{-# DEPRECATED itiFpgaInfo "Use generic-lens or generic-optics with 'fpgaInfo' instead." #-}

-- | Indicates whether the instance type is eligible for the free tier.
--
-- /Note:/ Consider using 'freeTierEligible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiFreeTierEligible :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
itiFreeTierEligible = Lens.field @"freeTierEligible"
{-# DEPRECATED itiFreeTierEligible "Use generic-lens or generic-optics with 'freeTierEligible' instead." #-}

-- | Describes the GPU accelerator settings for the instance type.
--
-- /Note:/ Consider using 'gpuInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiGpuInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe Types.GpuInfo)
itiGpuInfo = Lens.field @"gpuInfo"
{-# DEPRECATED itiGpuInfo "Use generic-lens or generic-optics with 'gpuInfo' instead." #-}

-- | Indicates whether On-Demand hibernation is supported.
--
-- /Note:/ Consider using 'hibernationSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiHibernationSupported :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
itiHibernationSupported = Lens.field @"hibernationSupported"
{-# DEPRECATED itiHibernationSupported "Use generic-lens or generic-optics with 'hibernationSupported' instead." #-}

-- | The hypervisor for the instance type.
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiHypervisor :: Lens.Lens' InstanceTypeInfo (Core.Maybe Types.InstanceTypeHypervisor)
itiHypervisor = Lens.field @"hypervisor"
{-# DEPRECATED itiHypervisor "Use generic-lens or generic-optics with 'hypervisor' instead." #-}

-- | Describes the Inference accelerator settings for the instance type.
--
-- /Note:/ Consider using 'inferenceAcceleratorInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiInferenceAcceleratorInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe Types.InferenceAcceleratorInfo)
itiInferenceAcceleratorInfo = Lens.field @"inferenceAcceleratorInfo"
{-# DEPRECATED itiInferenceAcceleratorInfo "Use generic-lens or generic-optics with 'inferenceAcceleratorInfo' instead." #-}

-- | Describes the instance storage for the instance type.
--
-- /Note:/ Consider using 'instanceStorageInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiInstanceStorageInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe Types.InstanceStorageInfo)
itiInstanceStorageInfo = Lens.field @"instanceStorageInfo"
{-# DEPRECATED itiInstanceStorageInfo "Use generic-lens or generic-optics with 'instanceStorageInfo' instead." #-}

-- | Indicates whether instance storage is supported.
--
-- /Note:/ Consider using 'instanceStorageSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiInstanceStorageSupported :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
itiInstanceStorageSupported = Lens.field @"instanceStorageSupported"
{-# DEPRECATED itiInstanceStorageSupported "Use generic-lens or generic-optics with 'instanceStorageSupported' instead." #-}

-- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiInstanceType :: Lens.Lens' InstanceTypeInfo (Core.Maybe Types.InstanceType)
itiInstanceType = Lens.field @"instanceType"
{-# DEPRECATED itiInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Describes the memory for the instance type.
--
-- /Note:/ Consider using 'memoryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiMemoryInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe Types.MemoryInfo)
itiMemoryInfo = Lens.field @"memoryInfo"
{-# DEPRECATED itiMemoryInfo "Use generic-lens or generic-optics with 'memoryInfo' instead." #-}

-- | Describes the network settings for the instance type.
--
-- /Note:/ Consider using 'networkInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiNetworkInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe Types.NetworkInfo)
itiNetworkInfo = Lens.field @"networkInfo"
{-# DEPRECATED itiNetworkInfo "Use generic-lens or generic-optics with 'networkInfo' instead." #-}

-- | Describes the placement group settings for the instance type.
--
-- /Note:/ Consider using 'placementGroupInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiPlacementGroupInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe Types.PlacementGroupInfo)
itiPlacementGroupInfo = Lens.field @"placementGroupInfo"
{-# DEPRECATED itiPlacementGroupInfo "Use generic-lens or generic-optics with 'placementGroupInfo' instead." #-}

-- | Describes the processor.
--
-- /Note:/ Consider using 'processorInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiProcessorInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe Types.ProcessorInfo)
itiProcessorInfo = Lens.field @"processorInfo"
{-# DEPRECATED itiProcessorInfo "Use generic-lens or generic-optics with 'processorInfo' instead." #-}

-- | The supported root device types.
--
-- /Note:/ Consider using 'supportedRootDeviceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiSupportedRootDeviceTypes :: Lens.Lens' InstanceTypeInfo (Core.Maybe [Types.RootDeviceType])
itiSupportedRootDeviceTypes = Lens.field @"supportedRootDeviceTypes"
{-# DEPRECATED itiSupportedRootDeviceTypes "Use generic-lens or generic-optics with 'supportedRootDeviceTypes' instead." #-}

-- | Indicates whether the instance type is offered for spot or On-Demand.
--
-- /Note:/ Consider using 'supportedUsageClasses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiSupportedUsageClasses :: Lens.Lens' InstanceTypeInfo (Core.Maybe [Types.UsageClassType])
itiSupportedUsageClasses = Lens.field @"supportedUsageClasses"
{-# DEPRECATED itiSupportedUsageClasses "Use generic-lens or generic-optics with 'supportedUsageClasses' instead." #-}

-- | The supported virtualization types.
--
-- /Note:/ Consider using 'supportedVirtualizationTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiSupportedVirtualizationTypes :: Lens.Lens' InstanceTypeInfo (Core.Maybe [Types.VirtualizationType])
itiSupportedVirtualizationTypes = Lens.field @"supportedVirtualizationTypes"
{-# DEPRECATED itiSupportedVirtualizationTypes "Use generic-lens or generic-optics with 'supportedVirtualizationTypes' instead." #-}

-- | Describes the vCPU configurations for the instance type.
--
-- /Note:/ Consider using 'vCpuInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiVCpuInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe Types.VCpuInfo)
itiVCpuInfo = Lens.field @"vCpuInfo"
{-# DEPRECATED itiVCpuInfo "Use generic-lens or generic-optics with 'vCpuInfo' instead." #-}

instance Core.FromXML InstanceTypeInfo where
  parseXML x =
    InstanceTypeInfo'
      Core.<$> (x Core..@? "autoRecoverySupported")
      Core.<*> (x Core..@? "bareMetal")
      Core.<*> (x Core..@? "burstablePerformanceSupported")
      Core.<*> (x Core..@? "currentGeneration")
      Core.<*> (x Core..@? "dedicatedHostsSupported")
      Core.<*> (x Core..@? "ebsInfo")
      Core.<*> (x Core..@? "fpgaInfo")
      Core.<*> (x Core..@? "freeTierEligible")
      Core.<*> (x Core..@? "gpuInfo")
      Core.<*> (x Core..@? "hibernationSupported")
      Core.<*> (x Core..@? "hypervisor")
      Core.<*> (x Core..@? "inferenceAcceleratorInfo")
      Core.<*> (x Core..@? "instanceStorageInfo")
      Core.<*> (x Core..@? "instanceStorageSupported")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "memoryInfo")
      Core.<*> (x Core..@? "networkInfo")
      Core.<*> (x Core..@? "placementGroupInfo")
      Core.<*> (x Core..@? "processorInfo")
      Core.<*> ( x Core..@? "supportedRootDeviceTypes"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> ( x Core..@? "supportedUsageClasses"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> ( x Core..@? "supportedVirtualizationTypes"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "vCpuInfo")
