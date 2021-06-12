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
-- Module      : Network.AWS.EC2.Types.InstanceTypeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceTypeInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EbsInfo
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
import Network.AWS.EC2.Types.VCpuInfo
import Network.AWS.EC2.Types.VirtualizationType
import qualified Network.AWS.Lens as Lens

-- | Describes the instance type.
--
-- /See:/ 'newInstanceTypeInfo' smart constructor.
data InstanceTypeInfo = InstanceTypeInfo'
  { -- | Describes the memory for the instance type.
    memoryInfo :: Core.Maybe MemoryInfo,
    -- | The hypervisor for the instance type.
    hypervisor :: Core.Maybe InstanceTypeHypervisor,
    -- | Describes the GPU accelerator settings for the instance type.
    gpuInfo :: Core.Maybe GpuInfo,
    -- | The instance type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/.
    instanceType :: Core.Maybe InstanceType,
    -- | Indicates whether the instance type is a burstable performance instance
    -- type.
    burstablePerformanceSupported :: Core.Maybe Core.Bool,
    -- | Describes the vCPU configurations for the instance type.
    vCpuInfo :: Core.Maybe VCpuInfo,
    -- | The supported root device types.
    supportedRootDeviceTypes :: Core.Maybe [RootDeviceType],
    -- | Describes the placement group settings for the instance type.
    placementGroupInfo :: Core.Maybe PlacementGroupInfo,
    -- | Describes the network settings for the instance type.
    networkInfo :: Core.Maybe NetworkInfo,
    -- | Describes the processor.
    processorInfo :: Core.Maybe ProcessorInfo,
    -- | Describes the Amazon EBS settings for the instance type.
    ebsInfo :: Core.Maybe EbsInfo,
    -- | Indicates whether auto recovery is supported.
    autoRecoverySupported :: Core.Maybe Core.Bool,
    -- | Indicates whether the instance type is current generation.
    currentGeneration :: Core.Maybe Core.Bool,
    -- | Indicates whether On-Demand hibernation is supported.
    hibernationSupported :: Core.Maybe Core.Bool,
    -- | Indicates whether Dedicated Hosts are supported on the instance type.
    dedicatedHostsSupported :: Core.Maybe Core.Bool,
    -- | Indicates whether instance storage is supported.
    instanceStorageSupported :: Core.Maybe Core.Bool,
    -- | Describes the instance storage for the instance type.
    instanceStorageInfo :: Core.Maybe InstanceStorageInfo,
    -- | Describes the FPGA accelerator settings for the instance type.
    fpgaInfo :: Core.Maybe FpgaInfo,
    -- | Indicates whether the instance type is offered for spot or On-Demand.
    supportedUsageClasses :: Core.Maybe [UsageClassType],
    -- | Indicates whether the instance type is eligible for the free tier.
    freeTierEligible :: Core.Maybe Core.Bool,
    -- | Indicates whether the instance is a bare metal instance type.
    bareMetal :: Core.Maybe Core.Bool,
    -- | Describes the Inference accelerator settings for the instance type.
    inferenceAcceleratorInfo :: Core.Maybe InferenceAcceleratorInfo,
    -- | The supported virtualization types.
    supportedVirtualizationTypes :: Core.Maybe [VirtualizationType]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceTypeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memoryInfo', 'instanceTypeInfo_memoryInfo' - Describes the memory for the instance type.
--
-- 'hypervisor', 'instanceTypeInfo_hypervisor' - The hypervisor for the instance type.
--
-- 'gpuInfo', 'instanceTypeInfo_gpuInfo' - Describes the GPU accelerator settings for the instance type.
--
-- 'instanceType', 'instanceTypeInfo_instanceType' - The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
--
-- 'burstablePerformanceSupported', 'instanceTypeInfo_burstablePerformanceSupported' - Indicates whether the instance type is a burstable performance instance
-- type.
--
-- 'vCpuInfo', 'instanceTypeInfo_vCpuInfo' - Describes the vCPU configurations for the instance type.
--
-- 'supportedRootDeviceTypes', 'instanceTypeInfo_supportedRootDeviceTypes' - The supported root device types.
--
-- 'placementGroupInfo', 'instanceTypeInfo_placementGroupInfo' - Describes the placement group settings for the instance type.
--
-- 'networkInfo', 'instanceTypeInfo_networkInfo' - Describes the network settings for the instance type.
--
-- 'processorInfo', 'instanceTypeInfo_processorInfo' - Describes the processor.
--
-- 'ebsInfo', 'instanceTypeInfo_ebsInfo' - Describes the Amazon EBS settings for the instance type.
--
-- 'autoRecoverySupported', 'instanceTypeInfo_autoRecoverySupported' - Indicates whether auto recovery is supported.
--
-- 'currentGeneration', 'instanceTypeInfo_currentGeneration' - Indicates whether the instance type is current generation.
--
-- 'hibernationSupported', 'instanceTypeInfo_hibernationSupported' - Indicates whether On-Demand hibernation is supported.
--
-- 'dedicatedHostsSupported', 'instanceTypeInfo_dedicatedHostsSupported' - Indicates whether Dedicated Hosts are supported on the instance type.
--
-- 'instanceStorageSupported', 'instanceTypeInfo_instanceStorageSupported' - Indicates whether instance storage is supported.
--
-- 'instanceStorageInfo', 'instanceTypeInfo_instanceStorageInfo' - Describes the instance storage for the instance type.
--
-- 'fpgaInfo', 'instanceTypeInfo_fpgaInfo' - Describes the FPGA accelerator settings for the instance type.
--
-- 'supportedUsageClasses', 'instanceTypeInfo_supportedUsageClasses' - Indicates whether the instance type is offered for spot or On-Demand.
--
-- 'freeTierEligible', 'instanceTypeInfo_freeTierEligible' - Indicates whether the instance type is eligible for the free tier.
--
-- 'bareMetal', 'instanceTypeInfo_bareMetal' - Indicates whether the instance is a bare metal instance type.
--
-- 'inferenceAcceleratorInfo', 'instanceTypeInfo_inferenceAcceleratorInfo' - Describes the Inference accelerator settings for the instance type.
--
-- 'supportedVirtualizationTypes', 'instanceTypeInfo_supportedVirtualizationTypes' - The supported virtualization types.
newInstanceTypeInfo ::
  InstanceTypeInfo
newInstanceTypeInfo =
  InstanceTypeInfo'
    { memoryInfo = Core.Nothing,
      hypervisor = Core.Nothing,
      gpuInfo = Core.Nothing,
      instanceType = Core.Nothing,
      burstablePerformanceSupported = Core.Nothing,
      vCpuInfo = Core.Nothing,
      supportedRootDeviceTypes = Core.Nothing,
      placementGroupInfo = Core.Nothing,
      networkInfo = Core.Nothing,
      processorInfo = Core.Nothing,
      ebsInfo = Core.Nothing,
      autoRecoverySupported = Core.Nothing,
      currentGeneration = Core.Nothing,
      hibernationSupported = Core.Nothing,
      dedicatedHostsSupported = Core.Nothing,
      instanceStorageSupported = Core.Nothing,
      instanceStorageInfo = Core.Nothing,
      fpgaInfo = Core.Nothing,
      supportedUsageClasses = Core.Nothing,
      freeTierEligible = Core.Nothing,
      bareMetal = Core.Nothing,
      inferenceAcceleratorInfo = Core.Nothing,
      supportedVirtualizationTypes = Core.Nothing
    }

-- | Describes the memory for the instance type.
instanceTypeInfo_memoryInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe MemoryInfo)
instanceTypeInfo_memoryInfo = Lens.lens (\InstanceTypeInfo' {memoryInfo} -> memoryInfo) (\s@InstanceTypeInfo' {} a -> s {memoryInfo = a} :: InstanceTypeInfo)

-- | The hypervisor for the instance type.
instanceTypeInfo_hypervisor :: Lens.Lens' InstanceTypeInfo (Core.Maybe InstanceTypeHypervisor)
instanceTypeInfo_hypervisor = Lens.lens (\InstanceTypeInfo' {hypervisor} -> hypervisor) (\s@InstanceTypeInfo' {} a -> s {hypervisor = a} :: InstanceTypeInfo)

-- | Describes the GPU accelerator settings for the instance type.
instanceTypeInfo_gpuInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe GpuInfo)
instanceTypeInfo_gpuInfo = Lens.lens (\InstanceTypeInfo' {gpuInfo} -> gpuInfo) (\s@InstanceTypeInfo' {} a -> s {gpuInfo = a} :: InstanceTypeInfo)

-- | The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
instanceTypeInfo_instanceType :: Lens.Lens' InstanceTypeInfo (Core.Maybe InstanceType)
instanceTypeInfo_instanceType = Lens.lens (\InstanceTypeInfo' {instanceType} -> instanceType) (\s@InstanceTypeInfo' {} a -> s {instanceType = a} :: InstanceTypeInfo)

-- | Indicates whether the instance type is a burstable performance instance
-- type.
instanceTypeInfo_burstablePerformanceSupported :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
instanceTypeInfo_burstablePerformanceSupported = Lens.lens (\InstanceTypeInfo' {burstablePerformanceSupported} -> burstablePerformanceSupported) (\s@InstanceTypeInfo' {} a -> s {burstablePerformanceSupported = a} :: InstanceTypeInfo)

-- | Describes the vCPU configurations for the instance type.
instanceTypeInfo_vCpuInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe VCpuInfo)
instanceTypeInfo_vCpuInfo = Lens.lens (\InstanceTypeInfo' {vCpuInfo} -> vCpuInfo) (\s@InstanceTypeInfo' {} a -> s {vCpuInfo = a} :: InstanceTypeInfo)

-- | The supported root device types.
instanceTypeInfo_supportedRootDeviceTypes :: Lens.Lens' InstanceTypeInfo (Core.Maybe [RootDeviceType])
instanceTypeInfo_supportedRootDeviceTypes = Lens.lens (\InstanceTypeInfo' {supportedRootDeviceTypes} -> supportedRootDeviceTypes) (\s@InstanceTypeInfo' {} a -> s {supportedRootDeviceTypes = a} :: InstanceTypeInfo) Core.. Lens.mapping Lens._Coerce

-- | Describes the placement group settings for the instance type.
instanceTypeInfo_placementGroupInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe PlacementGroupInfo)
instanceTypeInfo_placementGroupInfo = Lens.lens (\InstanceTypeInfo' {placementGroupInfo} -> placementGroupInfo) (\s@InstanceTypeInfo' {} a -> s {placementGroupInfo = a} :: InstanceTypeInfo)

-- | Describes the network settings for the instance type.
instanceTypeInfo_networkInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe NetworkInfo)
instanceTypeInfo_networkInfo = Lens.lens (\InstanceTypeInfo' {networkInfo} -> networkInfo) (\s@InstanceTypeInfo' {} a -> s {networkInfo = a} :: InstanceTypeInfo)

-- | Describes the processor.
instanceTypeInfo_processorInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe ProcessorInfo)
instanceTypeInfo_processorInfo = Lens.lens (\InstanceTypeInfo' {processorInfo} -> processorInfo) (\s@InstanceTypeInfo' {} a -> s {processorInfo = a} :: InstanceTypeInfo)

-- | Describes the Amazon EBS settings for the instance type.
instanceTypeInfo_ebsInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe EbsInfo)
instanceTypeInfo_ebsInfo = Lens.lens (\InstanceTypeInfo' {ebsInfo} -> ebsInfo) (\s@InstanceTypeInfo' {} a -> s {ebsInfo = a} :: InstanceTypeInfo)

-- | Indicates whether auto recovery is supported.
instanceTypeInfo_autoRecoverySupported :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
instanceTypeInfo_autoRecoverySupported = Lens.lens (\InstanceTypeInfo' {autoRecoverySupported} -> autoRecoverySupported) (\s@InstanceTypeInfo' {} a -> s {autoRecoverySupported = a} :: InstanceTypeInfo)

-- | Indicates whether the instance type is current generation.
instanceTypeInfo_currentGeneration :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
instanceTypeInfo_currentGeneration = Lens.lens (\InstanceTypeInfo' {currentGeneration} -> currentGeneration) (\s@InstanceTypeInfo' {} a -> s {currentGeneration = a} :: InstanceTypeInfo)

-- | Indicates whether On-Demand hibernation is supported.
instanceTypeInfo_hibernationSupported :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
instanceTypeInfo_hibernationSupported = Lens.lens (\InstanceTypeInfo' {hibernationSupported} -> hibernationSupported) (\s@InstanceTypeInfo' {} a -> s {hibernationSupported = a} :: InstanceTypeInfo)

-- | Indicates whether Dedicated Hosts are supported on the instance type.
instanceTypeInfo_dedicatedHostsSupported :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
instanceTypeInfo_dedicatedHostsSupported = Lens.lens (\InstanceTypeInfo' {dedicatedHostsSupported} -> dedicatedHostsSupported) (\s@InstanceTypeInfo' {} a -> s {dedicatedHostsSupported = a} :: InstanceTypeInfo)

-- | Indicates whether instance storage is supported.
instanceTypeInfo_instanceStorageSupported :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
instanceTypeInfo_instanceStorageSupported = Lens.lens (\InstanceTypeInfo' {instanceStorageSupported} -> instanceStorageSupported) (\s@InstanceTypeInfo' {} a -> s {instanceStorageSupported = a} :: InstanceTypeInfo)

-- | Describes the instance storage for the instance type.
instanceTypeInfo_instanceStorageInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe InstanceStorageInfo)
instanceTypeInfo_instanceStorageInfo = Lens.lens (\InstanceTypeInfo' {instanceStorageInfo} -> instanceStorageInfo) (\s@InstanceTypeInfo' {} a -> s {instanceStorageInfo = a} :: InstanceTypeInfo)

-- | Describes the FPGA accelerator settings for the instance type.
instanceTypeInfo_fpgaInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe FpgaInfo)
instanceTypeInfo_fpgaInfo = Lens.lens (\InstanceTypeInfo' {fpgaInfo} -> fpgaInfo) (\s@InstanceTypeInfo' {} a -> s {fpgaInfo = a} :: InstanceTypeInfo)

-- | Indicates whether the instance type is offered for spot or On-Demand.
instanceTypeInfo_supportedUsageClasses :: Lens.Lens' InstanceTypeInfo (Core.Maybe [UsageClassType])
instanceTypeInfo_supportedUsageClasses = Lens.lens (\InstanceTypeInfo' {supportedUsageClasses} -> supportedUsageClasses) (\s@InstanceTypeInfo' {} a -> s {supportedUsageClasses = a} :: InstanceTypeInfo) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether the instance type is eligible for the free tier.
instanceTypeInfo_freeTierEligible :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
instanceTypeInfo_freeTierEligible = Lens.lens (\InstanceTypeInfo' {freeTierEligible} -> freeTierEligible) (\s@InstanceTypeInfo' {} a -> s {freeTierEligible = a} :: InstanceTypeInfo)

-- | Indicates whether the instance is a bare metal instance type.
instanceTypeInfo_bareMetal :: Lens.Lens' InstanceTypeInfo (Core.Maybe Core.Bool)
instanceTypeInfo_bareMetal = Lens.lens (\InstanceTypeInfo' {bareMetal} -> bareMetal) (\s@InstanceTypeInfo' {} a -> s {bareMetal = a} :: InstanceTypeInfo)

-- | Describes the Inference accelerator settings for the instance type.
instanceTypeInfo_inferenceAcceleratorInfo :: Lens.Lens' InstanceTypeInfo (Core.Maybe InferenceAcceleratorInfo)
instanceTypeInfo_inferenceAcceleratorInfo = Lens.lens (\InstanceTypeInfo' {inferenceAcceleratorInfo} -> inferenceAcceleratorInfo) (\s@InstanceTypeInfo' {} a -> s {inferenceAcceleratorInfo = a} :: InstanceTypeInfo)

-- | The supported virtualization types.
instanceTypeInfo_supportedVirtualizationTypes :: Lens.Lens' InstanceTypeInfo (Core.Maybe [VirtualizationType])
instanceTypeInfo_supportedVirtualizationTypes = Lens.lens (\InstanceTypeInfo' {supportedVirtualizationTypes} -> supportedVirtualizationTypes) (\s@InstanceTypeInfo' {} a -> s {supportedVirtualizationTypes = a} :: InstanceTypeInfo) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML InstanceTypeInfo where
  parseXML x =
    InstanceTypeInfo'
      Core.<$> (x Core..@? "memoryInfo")
      Core.<*> (x Core..@? "hypervisor")
      Core.<*> (x Core..@? "gpuInfo")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "burstablePerformanceSupported")
      Core.<*> (x Core..@? "vCpuInfo")
      Core.<*> ( x Core..@? "supportedRootDeviceTypes"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "placementGroupInfo")
      Core.<*> (x Core..@? "networkInfo")
      Core.<*> (x Core..@? "processorInfo")
      Core.<*> (x Core..@? "ebsInfo")
      Core.<*> (x Core..@? "autoRecoverySupported")
      Core.<*> (x Core..@? "currentGeneration")
      Core.<*> (x Core..@? "hibernationSupported")
      Core.<*> (x Core..@? "dedicatedHostsSupported")
      Core.<*> (x Core..@? "instanceStorageSupported")
      Core.<*> (x Core..@? "instanceStorageInfo")
      Core.<*> (x Core..@? "fpgaInfo")
      Core.<*> ( x Core..@? "supportedUsageClasses"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "freeTierEligible")
      Core.<*> (x Core..@? "bareMetal")
      Core.<*> (x Core..@? "inferenceAcceleratorInfo")
      Core.<*> ( x Core..@? "supportedVirtualizationTypes"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable InstanceTypeInfo

instance Core.NFData InstanceTypeInfo
