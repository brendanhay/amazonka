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
    itiHypervisor,
    itiCurrentGeneration,
    itiMemoryInfo,
    itiPlacementGroupInfo,
    itiSupportedRootDeviceTypes,
    itiSupportedUsageClasses,
    itiInstanceStorageSupported,
    itiFpgaInfo,
    itiBurstablePerformanceSupported,
    itiInstanceType,
    itiGpuInfo,
    itiSupportedVirtualizationTypes,
    itiEBSInfo,
    itiAutoRecoverySupported,
    itiInferenceAcceleratorInfo,
    itiBareMetal,
    itiNetworkInfo,
    itiProcessorInfo,
    itiFreeTierEligible,
    itiVCPUInfo,
    itiInstanceStorageInfo,
    itiDedicatedHostsSupported,
    itiHibernationSupported,
  )
where

import Network.AWS.EC2.Types.EBSInfo
import Network.AWS.EC2.Types.FpgaInfo
import Network.AWS.EC2.Types.GpuInfo
import Network.AWS.EC2.Types.InferenceAcceleratorInfo
import Network.AWS.EC2.Types.InstanceStorageInfo
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.InstanceTypeHypervisor
import Network.AWS.EC2.Types.MemoryInfo
import Network.AWS.EC2.Types.NetworkInfo
import Network.AWS.EC2.Types.PlacementGroupInfo
import Network.AWS.EC2.Types.ProcessorInfo
import Network.AWS.EC2.Types.RootDeviceType
import Network.AWS.EC2.Types.UsageClassType
import Network.AWS.EC2.Types.VCPUInfo
import Network.AWS.EC2.Types.VirtualizationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the instance type.
--
-- /See:/ 'mkInstanceTypeInfo' smart constructor.
data InstanceTypeInfo = InstanceTypeInfo'
  { -- | The hypervisor for the instance type.
    hypervisor :: Lude.Maybe InstanceTypeHypervisor,
    -- | Indicates whether the instance type is current generation.
    currentGeneration :: Lude.Maybe Lude.Bool,
    -- | Describes the memory for the instance type.
    memoryInfo :: Lude.Maybe MemoryInfo,
    -- | Describes the placement group settings for the instance type.
    placementGroupInfo :: Lude.Maybe PlacementGroupInfo,
    -- | The supported root device types.
    supportedRootDeviceTypes :: Lude.Maybe [RootDeviceType],
    -- | Indicates whether the instance type is offered for spot or On-Demand.
    supportedUsageClasses :: Lude.Maybe [UsageClassType],
    -- | Indicates whether instance storage is supported.
    instanceStorageSupported :: Lude.Maybe Lude.Bool,
    -- | Describes the FPGA accelerator settings for the instance type.
    fpgaInfo :: Lude.Maybe FpgaInfo,
    -- | Indicates whether the instance type is a burstable performance instance type.
    burstablePerformanceSupported :: Lude.Maybe Lude.Bool,
    -- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
    instanceType :: Lude.Maybe InstanceType,
    -- | Describes the GPU accelerator settings for the instance type.
    gpuInfo :: Lude.Maybe GpuInfo,
    -- | The supported virtualization types.
    supportedVirtualizationTypes :: Lude.Maybe [VirtualizationType],
    -- | Describes the Amazon EBS settings for the instance type.
    ebsInfo :: Lude.Maybe EBSInfo,
    -- | Indicates whether auto recovery is supported.
    autoRecoverySupported :: Lude.Maybe Lude.Bool,
    -- | Describes the Inference accelerator settings for the instance type.
    inferenceAcceleratorInfo :: Lude.Maybe InferenceAcceleratorInfo,
    -- | Indicates whether the instance is a bare metal instance type.
    bareMetal :: Lude.Maybe Lude.Bool,
    -- | Describes the network settings for the instance type.
    networkInfo :: Lude.Maybe NetworkInfo,
    -- | Describes the processor.
    processorInfo :: Lude.Maybe ProcessorInfo,
    -- | Indicates whether the instance type is eligible for the free tier.
    freeTierEligible :: Lude.Maybe Lude.Bool,
    -- | Describes the vCPU configurations for the instance type.
    vCPUInfo :: Lude.Maybe VCPUInfo,
    -- | Describes the instance storage for the instance type.
    instanceStorageInfo :: Lude.Maybe InstanceStorageInfo,
    -- | Indicates whether Dedicated Hosts are supported on the instance type.
    dedicatedHostsSupported :: Lude.Maybe Lude.Bool,
    -- | Indicates whether On-Demand hibernation is supported.
    hibernationSupported :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceTypeInfo' with the minimum fields required to make a request.
--
-- * 'hypervisor' - The hypervisor for the instance type.
-- * 'currentGeneration' - Indicates whether the instance type is current generation.
-- * 'memoryInfo' - Describes the memory for the instance type.
-- * 'placementGroupInfo' - Describes the placement group settings for the instance type.
-- * 'supportedRootDeviceTypes' - The supported root device types.
-- * 'supportedUsageClasses' - Indicates whether the instance type is offered for spot or On-Demand.
-- * 'instanceStorageSupported' - Indicates whether instance storage is supported.
-- * 'fpgaInfo' - Describes the FPGA accelerator settings for the instance type.
-- * 'burstablePerformanceSupported' - Indicates whether the instance type is a burstable performance instance type.
-- * 'instanceType' - The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'gpuInfo' - Describes the GPU accelerator settings for the instance type.
-- * 'supportedVirtualizationTypes' - The supported virtualization types.
-- * 'ebsInfo' - Describes the Amazon EBS settings for the instance type.
-- * 'autoRecoverySupported' - Indicates whether auto recovery is supported.
-- * 'inferenceAcceleratorInfo' - Describes the Inference accelerator settings for the instance type.
-- * 'bareMetal' - Indicates whether the instance is a bare metal instance type.
-- * 'networkInfo' - Describes the network settings for the instance type.
-- * 'processorInfo' - Describes the processor.
-- * 'freeTierEligible' - Indicates whether the instance type is eligible for the free tier.
-- * 'vCPUInfo' - Describes the vCPU configurations for the instance type.
-- * 'instanceStorageInfo' - Describes the instance storage for the instance type.
-- * 'dedicatedHostsSupported' - Indicates whether Dedicated Hosts are supported on the instance type.
-- * 'hibernationSupported' - Indicates whether On-Demand hibernation is supported.
mkInstanceTypeInfo ::
  InstanceTypeInfo
mkInstanceTypeInfo =
  InstanceTypeInfo'
    { hypervisor = Lude.Nothing,
      currentGeneration = Lude.Nothing,
      memoryInfo = Lude.Nothing,
      placementGroupInfo = Lude.Nothing,
      supportedRootDeviceTypes = Lude.Nothing,
      supportedUsageClasses = Lude.Nothing,
      instanceStorageSupported = Lude.Nothing,
      fpgaInfo = Lude.Nothing,
      burstablePerformanceSupported = Lude.Nothing,
      instanceType = Lude.Nothing,
      gpuInfo = Lude.Nothing,
      supportedVirtualizationTypes = Lude.Nothing,
      ebsInfo = Lude.Nothing,
      autoRecoverySupported = Lude.Nothing,
      inferenceAcceleratorInfo = Lude.Nothing,
      bareMetal = Lude.Nothing,
      networkInfo = Lude.Nothing,
      processorInfo = Lude.Nothing,
      freeTierEligible = Lude.Nothing,
      vCPUInfo = Lude.Nothing,
      instanceStorageInfo = Lude.Nothing,
      dedicatedHostsSupported = Lude.Nothing,
      hibernationSupported = Lude.Nothing
    }

-- | The hypervisor for the instance type.
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiHypervisor :: Lens.Lens' InstanceTypeInfo (Lude.Maybe InstanceTypeHypervisor)
itiHypervisor = Lens.lens (hypervisor :: InstanceTypeInfo -> Lude.Maybe InstanceTypeHypervisor) (\s a -> s {hypervisor = a} :: InstanceTypeInfo)
{-# DEPRECATED itiHypervisor "Use generic-lens or generic-optics with 'hypervisor' instead." #-}

-- | Indicates whether the instance type is current generation.
--
-- /Note:/ Consider using 'currentGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiCurrentGeneration :: Lens.Lens' InstanceTypeInfo (Lude.Maybe Lude.Bool)
itiCurrentGeneration = Lens.lens (currentGeneration :: InstanceTypeInfo -> Lude.Maybe Lude.Bool) (\s a -> s {currentGeneration = a} :: InstanceTypeInfo)
{-# DEPRECATED itiCurrentGeneration "Use generic-lens or generic-optics with 'currentGeneration' instead." #-}

-- | Describes the memory for the instance type.
--
-- /Note:/ Consider using 'memoryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiMemoryInfo :: Lens.Lens' InstanceTypeInfo (Lude.Maybe MemoryInfo)
itiMemoryInfo = Lens.lens (memoryInfo :: InstanceTypeInfo -> Lude.Maybe MemoryInfo) (\s a -> s {memoryInfo = a} :: InstanceTypeInfo)
{-# DEPRECATED itiMemoryInfo "Use generic-lens or generic-optics with 'memoryInfo' instead." #-}

-- | Describes the placement group settings for the instance type.
--
-- /Note:/ Consider using 'placementGroupInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiPlacementGroupInfo :: Lens.Lens' InstanceTypeInfo (Lude.Maybe PlacementGroupInfo)
itiPlacementGroupInfo = Lens.lens (placementGroupInfo :: InstanceTypeInfo -> Lude.Maybe PlacementGroupInfo) (\s a -> s {placementGroupInfo = a} :: InstanceTypeInfo)
{-# DEPRECATED itiPlacementGroupInfo "Use generic-lens or generic-optics with 'placementGroupInfo' instead." #-}

-- | The supported root device types.
--
-- /Note:/ Consider using 'supportedRootDeviceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiSupportedRootDeviceTypes :: Lens.Lens' InstanceTypeInfo (Lude.Maybe [RootDeviceType])
itiSupportedRootDeviceTypes = Lens.lens (supportedRootDeviceTypes :: InstanceTypeInfo -> Lude.Maybe [RootDeviceType]) (\s a -> s {supportedRootDeviceTypes = a} :: InstanceTypeInfo)
{-# DEPRECATED itiSupportedRootDeviceTypes "Use generic-lens or generic-optics with 'supportedRootDeviceTypes' instead." #-}

-- | Indicates whether the instance type is offered for spot or On-Demand.
--
-- /Note:/ Consider using 'supportedUsageClasses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiSupportedUsageClasses :: Lens.Lens' InstanceTypeInfo (Lude.Maybe [UsageClassType])
itiSupportedUsageClasses = Lens.lens (supportedUsageClasses :: InstanceTypeInfo -> Lude.Maybe [UsageClassType]) (\s a -> s {supportedUsageClasses = a} :: InstanceTypeInfo)
{-# DEPRECATED itiSupportedUsageClasses "Use generic-lens or generic-optics with 'supportedUsageClasses' instead." #-}

-- | Indicates whether instance storage is supported.
--
-- /Note:/ Consider using 'instanceStorageSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiInstanceStorageSupported :: Lens.Lens' InstanceTypeInfo (Lude.Maybe Lude.Bool)
itiInstanceStorageSupported = Lens.lens (instanceStorageSupported :: InstanceTypeInfo -> Lude.Maybe Lude.Bool) (\s a -> s {instanceStorageSupported = a} :: InstanceTypeInfo)
{-# DEPRECATED itiInstanceStorageSupported "Use generic-lens or generic-optics with 'instanceStorageSupported' instead." #-}

-- | Describes the FPGA accelerator settings for the instance type.
--
-- /Note:/ Consider using 'fpgaInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiFpgaInfo :: Lens.Lens' InstanceTypeInfo (Lude.Maybe FpgaInfo)
itiFpgaInfo = Lens.lens (fpgaInfo :: InstanceTypeInfo -> Lude.Maybe FpgaInfo) (\s a -> s {fpgaInfo = a} :: InstanceTypeInfo)
{-# DEPRECATED itiFpgaInfo "Use generic-lens or generic-optics with 'fpgaInfo' instead." #-}

-- | Indicates whether the instance type is a burstable performance instance type.
--
-- /Note:/ Consider using 'burstablePerformanceSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiBurstablePerformanceSupported :: Lens.Lens' InstanceTypeInfo (Lude.Maybe Lude.Bool)
itiBurstablePerformanceSupported = Lens.lens (burstablePerformanceSupported :: InstanceTypeInfo -> Lude.Maybe Lude.Bool) (\s a -> s {burstablePerformanceSupported = a} :: InstanceTypeInfo)
{-# DEPRECATED itiBurstablePerformanceSupported "Use generic-lens or generic-optics with 'burstablePerformanceSupported' instead." #-}

-- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiInstanceType :: Lens.Lens' InstanceTypeInfo (Lude.Maybe InstanceType)
itiInstanceType = Lens.lens (instanceType :: InstanceTypeInfo -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: InstanceTypeInfo)
{-# DEPRECATED itiInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Describes the GPU accelerator settings for the instance type.
--
-- /Note:/ Consider using 'gpuInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiGpuInfo :: Lens.Lens' InstanceTypeInfo (Lude.Maybe GpuInfo)
itiGpuInfo = Lens.lens (gpuInfo :: InstanceTypeInfo -> Lude.Maybe GpuInfo) (\s a -> s {gpuInfo = a} :: InstanceTypeInfo)
{-# DEPRECATED itiGpuInfo "Use generic-lens or generic-optics with 'gpuInfo' instead." #-}

-- | The supported virtualization types.
--
-- /Note:/ Consider using 'supportedVirtualizationTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiSupportedVirtualizationTypes :: Lens.Lens' InstanceTypeInfo (Lude.Maybe [VirtualizationType])
itiSupportedVirtualizationTypes = Lens.lens (supportedVirtualizationTypes :: InstanceTypeInfo -> Lude.Maybe [VirtualizationType]) (\s a -> s {supportedVirtualizationTypes = a} :: InstanceTypeInfo)
{-# DEPRECATED itiSupportedVirtualizationTypes "Use generic-lens or generic-optics with 'supportedVirtualizationTypes' instead." #-}

-- | Describes the Amazon EBS settings for the instance type.
--
-- /Note:/ Consider using 'ebsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiEBSInfo :: Lens.Lens' InstanceTypeInfo (Lude.Maybe EBSInfo)
itiEBSInfo = Lens.lens (ebsInfo :: InstanceTypeInfo -> Lude.Maybe EBSInfo) (\s a -> s {ebsInfo = a} :: InstanceTypeInfo)
{-# DEPRECATED itiEBSInfo "Use generic-lens or generic-optics with 'ebsInfo' instead." #-}

-- | Indicates whether auto recovery is supported.
--
-- /Note:/ Consider using 'autoRecoverySupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiAutoRecoverySupported :: Lens.Lens' InstanceTypeInfo (Lude.Maybe Lude.Bool)
itiAutoRecoverySupported = Lens.lens (autoRecoverySupported :: InstanceTypeInfo -> Lude.Maybe Lude.Bool) (\s a -> s {autoRecoverySupported = a} :: InstanceTypeInfo)
{-# DEPRECATED itiAutoRecoverySupported "Use generic-lens or generic-optics with 'autoRecoverySupported' instead." #-}

-- | Describes the Inference accelerator settings for the instance type.
--
-- /Note:/ Consider using 'inferenceAcceleratorInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiInferenceAcceleratorInfo :: Lens.Lens' InstanceTypeInfo (Lude.Maybe InferenceAcceleratorInfo)
itiInferenceAcceleratorInfo = Lens.lens (inferenceAcceleratorInfo :: InstanceTypeInfo -> Lude.Maybe InferenceAcceleratorInfo) (\s a -> s {inferenceAcceleratorInfo = a} :: InstanceTypeInfo)
{-# DEPRECATED itiInferenceAcceleratorInfo "Use generic-lens or generic-optics with 'inferenceAcceleratorInfo' instead." #-}

-- | Indicates whether the instance is a bare metal instance type.
--
-- /Note:/ Consider using 'bareMetal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiBareMetal :: Lens.Lens' InstanceTypeInfo (Lude.Maybe Lude.Bool)
itiBareMetal = Lens.lens (bareMetal :: InstanceTypeInfo -> Lude.Maybe Lude.Bool) (\s a -> s {bareMetal = a} :: InstanceTypeInfo)
{-# DEPRECATED itiBareMetal "Use generic-lens or generic-optics with 'bareMetal' instead." #-}

-- | Describes the network settings for the instance type.
--
-- /Note:/ Consider using 'networkInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiNetworkInfo :: Lens.Lens' InstanceTypeInfo (Lude.Maybe NetworkInfo)
itiNetworkInfo = Lens.lens (networkInfo :: InstanceTypeInfo -> Lude.Maybe NetworkInfo) (\s a -> s {networkInfo = a} :: InstanceTypeInfo)
{-# DEPRECATED itiNetworkInfo "Use generic-lens or generic-optics with 'networkInfo' instead." #-}

-- | Describes the processor.
--
-- /Note:/ Consider using 'processorInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiProcessorInfo :: Lens.Lens' InstanceTypeInfo (Lude.Maybe ProcessorInfo)
itiProcessorInfo = Lens.lens (processorInfo :: InstanceTypeInfo -> Lude.Maybe ProcessorInfo) (\s a -> s {processorInfo = a} :: InstanceTypeInfo)
{-# DEPRECATED itiProcessorInfo "Use generic-lens or generic-optics with 'processorInfo' instead." #-}

-- | Indicates whether the instance type is eligible for the free tier.
--
-- /Note:/ Consider using 'freeTierEligible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiFreeTierEligible :: Lens.Lens' InstanceTypeInfo (Lude.Maybe Lude.Bool)
itiFreeTierEligible = Lens.lens (freeTierEligible :: InstanceTypeInfo -> Lude.Maybe Lude.Bool) (\s a -> s {freeTierEligible = a} :: InstanceTypeInfo)
{-# DEPRECATED itiFreeTierEligible "Use generic-lens or generic-optics with 'freeTierEligible' instead." #-}

-- | Describes the vCPU configurations for the instance type.
--
-- /Note:/ Consider using 'vCPUInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiVCPUInfo :: Lens.Lens' InstanceTypeInfo (Lude.Maybe VCPUInfo)
itiVCPUInfo = Lens.lens (vCPUInfo :: InstanceTypeInfo -> Lude.Maybe VCPUInfo) (\s a -> s {vCPUInfo = a} :: InstanceTypeInfo)
{-# DEPRECATED itiVCPUInfo "Use generic-lens or generic-optics with 'vCPUInfo' instead." #-}

-- | Describes the instance storage for the instance type.
--
-- /Note:/ Consider using 'instanceStorageInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiInstanceStorageInfo :: Lens.Lens' InstanceTypeInfo (Lude.Maybe InstanceStorageInfo)
itiInstanceStorageInfo = Lens.lens (instanceStorageInfo :: InstanceTypeInfo -> Lude.Maybe InstanceStorageInfo) (\s a -> s {instanceStorageInfo = a} :: InstanceTypeInfo)
{-# DEPRECATED itiInstanceStorageInfo "Use generic-lens or generic-optics with 'instanceStorageInfo' instead." #-}

-- | Indicates whether Dedicated Hosts are supported on the instance type.
--
-- /Note:/ Consider using 'dedicatedHostsSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiDedicatedHostsSupported :: Lens.Lens' InstanceTypeInfo (Lude.Maybe Lude.Bool)
itiDedicatedHostsSupported = Lens.lens (dedicatedHostsSupported :: InstanceTypeInfo -> Lude.Maybe Lude.Bool) (\s a -> s {dedicatedHostsSupported = a} :: InstanceTypeInfo)
{-# DEPRECATED itiDedicatedHostsSupported "Use generic-lens or generic-optics with 'dedicatedHostsSupported' instead." #-}

-- | Indicates whether On-Demand hibernation is supported.
--
-- /Note:/ Consider using 'hibernationSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itiHibernationSupported :: Lens.Lens' InstanceTypeInfo (Lude.Maybe Lude.Bool)
itiHibernationSupported = Lens.lens (hibernationSupported :: InstanceTypeInfo -> Lude.Maybe Lude.Bool) (\s a -> s {hibernationSupported = a} :: InstanceTypeInfo)
{-# DEPRECATED itiHibernationSupported "Use generic-lens or generic-optics with 'hibernationSupported' instead." #-}

instance Lude.FromXML InstanceTypeInfo where
  parseXML x =
    InstanceTypeInfo'
      Lude.<$> (x Lude..@? "hypervisor")
      Lude.<*> (x Lude..@? "currentGeneration")
      Lude.<*> (x Lude..@? "memoryInfo")
      Lude.<*> (x Lude..@? "placementGroupInfo")
      Lude.<*> ( x Lude..@? "supportedRootDeviceTypes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "supportedUsageClasses" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "instanceStorageSupported")
      Lude.<*> (x Lude..@? "fpgaInfo")
      Lude.<*> (x Lude..@? "burstablePerformanceSupported")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "gpuInfo")
      Lude.<*> ( x Lude..@? "supportedVirtualizationTypes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "ebsInfo")
      Lude.<*> (x Lude..@? "autoRecoverySupported")
      Lude.<*> (x Lude..@? "inferenceAcceleratorInfo")
      Lude.<*> (x Lude..@? "bareMetal")
      Lude.<*> (x Lude..@? "networkInfo")
      Lude.<*> (x Lude..@? "processorInfo")
      Lude.<*> (x Lude..@? "freeTierEligible")
      Lude.<*> (x Lude..@? "vCpuInfo")
      Lude.<*> (x Lude..@? "instanceStorageInfo")
      Lude.<*> (x Lude..@? "dedicatedHostsSupported")
      Lude.<*> (x Lude..@? "hibernationSupported")
