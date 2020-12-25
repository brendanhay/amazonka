{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MitigationActionParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MitigationActionParams
  ( MitigationActionParams (..),

    -- * Smart constructor
    mkMitigationActionParams,

    -- * Lenses
    mapAddThingsToThingGroupParams,
    mapEnableIoTLoggingParams,
    mapPublishFindingToSnsParams,
    mapReplaceDefaultPolicyVersionParams,
    mapUpdateCACertificateParams,
    mapUpdateDeviceCertificateParams,
  )
where

import qualified Network.AWS.IoT.Types.AddThingsToThingGroupParams as Types
import qualified Network.AWS.IoT.Types.EnableIoTLoggingParams as Types
import qualified Network.AWS.IoT.Types.PublishFindingToSnsParams as Types
import qualified Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams as Types
import qualified Network.AWS.IoT.Types.UpdateCACertificateParams as Types
import qualified Network.AWS.IoT.Types.UpdateDeviceCertificateParams as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The set of parameters for this mitigation action. You can specify only one type of parameter (in other words, you can apply only one action for each defined mitigation action).
--
-- /See:/ 'mkMitigationActionParams' smart constructor.
data MitigationActionParams = MitigationActionParams'
  { -- | Parameters to define a mitigation action that moves devices associated with a certificate to one or more specified thing groups, typically for quarantine.
    addThingsToThingGroupParams :: Core.Maybe Types.AddThingsToThingGroupParams,
    -- | Parameters to define a mitigation action that enables AWS IoT logging at a specified level of detail.
    enableIoTLoggingParams :: Core.Maybe Types.EnableIoTLoggingParams,
    -- | Parameters to define a mitigation action that publishes findings to Amazon SNS. You can implement your own custom actions in response to the Amazon SNS messages.
    publishFindingToSnsParams :: Core.Maybe Types.PublishFindingToSnsParams,
    -- | Parameters to define a mitigation action that adds a blank policy to restrict permissions.
    replaceDefaultPolicyVersionParams :: Core.Maybe Types.ReplaceDefaultPolicyVersionParams,
    -- | Parameters to define a mitigation action that changes the state of the CA certificate to inactive.
    updateCACertificateParams :: Core.Maybe Types.UpdateCACertificateParams,
    -- | Parameters to define a mitigation action that changes the state of the device certificate to inactive.
    updateDeviceCertificateParams :: Core.Maybe Types.UpdateDeviceCertificateParams
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MitigationActionParams' value with any optional fields omitted.
mkMitigationActionParams ::
  MitigationActionParams
mkMitigationActionParams =
  MitigationActionParams'
    { addThingsToThingGroupParams =
        Core.Nothing,
      enableIoTLoggingParams = Core.Nothing,
      publishFindingToSnsParams = Core.Nothing,
      replaceDefaultPolicyVersionParams = Core.Nothing,
      updateCACertificateParams = Core.Nothing,
      updateDeviceCertificateParams = Core.Nothing
    }

-- | Parameters to define a mitigation action that moves devices associated with a certificate to one or more specified thing groups, typically for quarantine.
--
-- /Note:/ Consider using 'addThingsToThingGroupParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mapAddThingsToThingGroupParams :: Lens.Lens' MitigationActionParams (Core.Maybe Types.AddThingsToThingGroupParams)
mapAddThingsToThingGroupParams = Lens.field @"addThingsToThingGroupParams"
{-# DEPRECATED mapAddThingsToThingGroupParams "Use generic-lens or generic-optics with 'addThingsToThingGroupParams' instead." #-}

-- | Parameters to define a mitigation action that enables AWS IoT logging at a specified level of detail.
--
-- /Note:/ Consider using 'enableIoTLoggingParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mapEnableIoTLoggingParams :: Lens.Lens' MitigationActionParams (Core.Maybe Types.EnableIoTLoggingParams)
mapEnableIoTLoggingParams = Lens.field @"enableIoTLoggingParams"
{-# DEPRECATED mapEnableIoTLoggingParams "Use generic-lens or generic-optics with 'enableIoTLoggingParams' instead." #-}

-- | Parameters to define a mitigation action that publishes findings to Amazon SNS. You can implement your own custom actions in response to the Amazon SNS messages.
--
-- /Note:/ Consider using 'publishFindingToSnsParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mapPublishFindingToSnsParams :: Lens.Lens' MitigationActionParams (Core.Maybe Types.PublishFindingToSnsParams)
mapPublishFindingToSnsParams = Lens.field @"publishFindingToSnsParams"
{-# DEPRECATED mapPublishFindingToSnsParams "Use generic-lens or generic-optics with 'publishFindingToSnsParams' instead." #-}

-- | Parameters to define a mitigation action that adds a blank policy to restrict permissions.
--
-- /Note:/ Consider using 'replaceDefaultPolicyVersionParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mapReplaceDefaultPolicyVersionParams :: Lens.Lens' MitigationActionParams (Core.Maybe Types.ReplaceDefaultPolicyVersionParams)
mapReplaceDefaultPolicyVersionParams = Lens.field @"replaceDefaultPolicyVersionParams"
{-# DEPRECATED mapReplaceDefaultPolicyVersionParams "Use generic-lens or generic-optics with 'replaceDefaultPolicyVersionParams' instead." #-}

-- | Parameters to define a mitigation action that changes the state of the CA certificate to inactive.
--
-- /Note:/ Consider using 'updateCACertificateParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mapUpdateCACertificateParams :: Lens.Lens' MitigationActionParams (Core.Maybe Types.UpdateCACertificateParams)
mapUpdateCACertificateParams = Lens.field @"updateCACertificateParams"
{-# DEPRECATED mapUpdateCACertificateParams "Use generic-lens or generic-optics with 'updateCACertificateParams' instead." #-}

-- | Parameters to define a mitigation action that changes the state of the device certificate to inactive.
--
-- /Note:/ Consider using 'updateDeviceCertificateParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mapUpdateDeviceCertificateParams :: Lens.Lens' MitigationActionParams (Core.Maybe Types.UpdateDeviceCertificateParams)
mapUpdateDeviceCertificateParams = Lens.field @"updateDeviceCertificateParams"
{-# DEPRECATED mapUpdateDeviceCertificateParams "Use generic-lens or generic-optics with 'updateDeviceCertificateParams' instead." #-}

instance Core.FromJSON MitigationActionParams where
  toJSON MitigationActionParams {..} =
    Core.object
      ( Core.catMaybes
          [ ("addThingsToThingGroupParams" Core..=)
              Core.<$> addThingsToThingGroupParams,
            ("enableIoTLoggingParams" Core..=) Core.<$> enableIoTLoggingParams,
            ("publishFindingToSnsParams" Core..=)
              Core.<$> publishFindingToSnsParams,
            ("replaceDefaultPolicyVersionParams" Core..=)
              Core.<$> replaceDefaultPolicyVersionParams,
            ("updateCACertificateParams" Core..=)
              Core.<$> updateCACertificateParams,
            ("updateDeviceCertificateParams" Core..=)
              Core.<$> updateDeviceCertificateParams
          ]
      )

instance Core.FromJSON MitigationActionParams where
  parseJSON =
    Core.withObject "MitigationActionParams" Core.$
      \x ->
        MitigationActionParams'
          Core.<$> (x Core..:? "addThingsToThingGroupParams")
          Core.<*> (x Core..:? "enableIoTLoggingParams")
          Core.<*> (x Core..:? "publishFindingToSnsParams")
          Core.<*> (x Core..:? "replaceDefaultPolicyVersionParams")
          Core.<*> (x Core..:? "updateCACertificateParams")
          Core.<*> (x Core..:? "updateDeviceCertificateParams")
