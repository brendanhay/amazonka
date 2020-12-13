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
    mapEnableIOTLoggingParams,
    mapAddThingsToThingGroupParams,
    mapUpdateCACertificateParams,
    mapUpdateDeviceCertificateParams,
    mapReplaceDefaultPolicyVersionParams,
    mapPublishFindingToSNSParams,
  )
where

import Network.AWS.IoT.Types.AddThingsToThingGroupParams
import Network.AWS.IoT.Types.EnableIOTLoggingParams
import Network.AWS.IoT.Types.PublishFindingToSNSParams
import Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams
import Network.AWS.IoT.Types.UpdateCACertificateParams
import Network.AWS.IoT.Types.UpdateDeviceCertificateParams
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The set of parameters for this mitigation action. You can specify only one type of parameter (in other words, you can apply only one action for each defined mitigation action).
--
-- /See:/ 'mkMitigationActionParams' smart constructor.
data MitigationActionParams = MitigationActionParams'
  { -- | Parameters to define a mitigation action that enables AWS IoT logging at a specified level of detail.
    enableIOTLoggingParams :: Lude.Maybe EnableIOTLoggingParams,
    -- | Parameters to define a mitigation action that moves devices associated with a certificate to one or more specified thing groups, typically for quarantine.
    addThingsToThingGroupParams :: Lude.Maybe AddThingsToThingGroupParams,
    -- | Parameters to define a mitigation action that changes the state of the CA certificate to inactive.
    updateCACertificateParams :: Lude.Maybe UpdateCACertificateParams,
    -- | Parameters to define a mitigation action that changes the state of the device certificate to inactive.
    updateDeviceCertificateParams :: Lude.Maybe UpdateDeviceCertificateParams,
    -- | Parameters to define a mitigation action that adds a blank policy to restrict permissions.
    replaceDefaultPolicyVersionParams :: Lude.Maybe ReplaceDefaultPolicyVersionParams,
    -- | Parameters to define a mitigation action that publishes findings to Amazon SNS. You can implement your own custom actions in response to the Amazon SNS messages.
    publishFindingToSNSParams :: Lude.Maybe PublishFindingToSNSParams
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MitigationActionParams' with the minimum fields required to make a request.
--
-- * 'enableIOTLoggingParams' - Parameters to define a mitigation action that enables AWS IoT logging at a specified level of detail.
-- * 'addThingsToThingGroupParams' - Parameters to define a mitigation action that moves devices associated with a certificate to one or more specified thing groups, typically for quarantine.
-- * 'updateCACertificateParams' - Parameters to define a mitigation action that changes the state of the CA certificate to inactive.
-- * 'updateDeviceCertificateParams' - Parameters to define a mitigation action that changes the state of the device certificate to inactive.
-- * 'replaceDefaultPolicyVersionParams' - Parameters to define a mitigation action that adds a blank policy to restrict permissions.
-- * 'publishFindingToSNSParams' - Parameters to define a mitigation action that publishes findings to Amazon SNS. You can implement your own custom actions in response to the Amazon SNS messages.
mkMitigationActionParams ::
  MitigationActionParams
mkMitigationActionParams =
  MitigationActionParams'
    { enableIOTLoggingParams = Lude.Nothing,
      addThingsToThingGroupParams = Lude.Nothing,
      updateCACertificateParams = Lude.Nothing,
      updateDeviceCertificateParams = Lude.Nothing,
      replaceDefaultPolicyVersionParams = Lude.Nothing,
      publishFindingToSNSParams = Lude.Nothing
    }

-- | Parameters to define a mitigation action that enables AWS IoT logging at a specified level of detail.
--
-- /Note:/ Consider using 'enableIOTLoggingParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mapEnableIOTLoggingParams :: Lens.Lens' MitigationActionParams (Lude.Maybe EnableIOTLoggingParams)
mapEnableIOTLoggingParams = Lens.lens (enableIOTLoggingParams :: MitigationActionParams -> Lude.Maybe EnableIOTLoggingParams) (\s a -> s {enableIOTLoggingParams = a} :: MitigationActionParams)
{-# DEPRECATED mapEnableIOTLoggingParams "Use generic-lens or generic-optics with 'enableIOTLoggingParams' instead." #-}

-- | Parameters to define a mitigation action that moves devices associated with a certificate to one or more specified thing groups, typically for quarantine.
--
-- /Note:/ Consider using 'addThingsToThingGroupParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mapAddThingsToThingGroupParams :: Lens.Lens' MitigationActionParams (Lude.Maybe AddThingsToThingGroupParams)
mapAddThingsToThingGroupParams = Lens.lens (addThingsToThingGroupParams :: MitigationActionParams -> Lude.Maybe AddThingsToThingGroupParams) (\s a -> s {addThingsToThingGroupParams = a} :: MitigationActionParams)
{-# DEPRECATED mapAddThingsToThingGroupParams "Use generic-lens or generic-optics with 'addThingsToThingGroupParams' instead." #-}

-- | Parameters to define a mitigation action that changes the state of the CA certificate to inactive.
--
-- /Note:/ Consider using 'updateCACertificateParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mapUpdateCACertificateParams :: Lens.Lens' MitigationActionParams (Lude.Maybe UpdateCACertificateParams)
mapUpdateCACertificateParams = Lens.lens (updateCACertificateParams :: MitigationActionParams -> Lude.Maybe UpdateCACertificateParams) (\s a -> s {updateCACertificateParams = a} :: MitigationActionParams)
{-# DEPRECATED mapUpdateCACertificateParams "Use generic-lens or generic-optics with 'updateCACertificateParams' instead." #-}

-- | Parameters to define a mitigation action that changes the state of the device certificate to inactive.
--
-- /Note:/ Consider using 'updateDeviceCertificateParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mapUpdateDeviceCertificateParams :: Lens.Lens' MitigationActionParams (Lude.Maybe UpdateDeviceCertificateParams)
mapUpdateDeviceCertificateParams = Lens.lens (updateDeviceCertificateParams :: MitigationActionParams -> Lude.Maybe UpdateDeviceCertificateParams) (\s a -> s {updateDeviceCertificateParams = a} :: MitigationActionParams)
{-# DEPRECATED mapUpdateDeviceCertificateParams "Use generic-lens or generic-optics with 'updateDeviceCertificateParams' instead." #-}

-- | Parameters to define a mitigation action that adds a blank policy to restrict permissions.
--
-- /Note:/ Consider using 'replaceDefaultPolicyVersionParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mapReplaceDefaultPolicyVersionParams :: Lens.Lens' MitigationActionParams (Lude.Maybe ReplaceDefaultPolicyVersionParams)
mapReplaceDefaultPolicyVersionParams = Lens.lens (replaceDefaultPolicyVersionParams :: MitigationActionParams -> Lude.Maybe ReplaceDefaultPolicyVersionParams) (\s a -> s {replaceDefaultPolicyVersionParams = a} :: MitigationActionParams)
{-# DEPRECATED mapReplaceDefaultPolicyVersionParams "Use generic-lens or generic-optics with 'replaceDefaultPolicyVersionParams' instead." #-}

-- | Parameters to define a mitigation action that publishes findings to Amazon SNS. You can implement your own custom actions in response to the Amazon SNS messages.
--
-- /Note:/ Consider using 'publishFindingToSNSParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mapPublishFindingToSNSParams :: Lens.Lens' MitigationActionParams (Lude.Maybe PublishFindingToSNSParams)
mapPublishFindingToSNSParams = Lens.lens (publishFindingToSNSParams :: MitigationActionParams -> Lude.Maybe PublishFindingToSNSParams) (\s a -> s {publishFindingToSNSParams = a} :: MitigationActionParams)
{-# DEPRECATED mapPublishFindingToSNSParams "Use generic-lens or generic-optics with 'publishFindingToSNSParams' instead." #-}

instance Lude.FromJSON MitigationActionParams where
  parseJSON =
    Lude.withObject
      "MitigationActionParams"
      ( \x ->
          MitigationActionParams'
            Lude.<$> (x Lude..:? "enableIoTLoggingParams")
            Lude.<*> (x Lude..:? "addThingsToThingGroupParams")
            Lude.<*> (x Lude..:? "updateCACertificateParams")
            Lude.<*> (x Lude..:? "updateDeviceCertificateParams")
            Lude.<*> (x Lude..:? "replaceDefaultPolicyVersionParams")
            Lude.<*> (x Lude..:? "publishFindingToSnsParams")
      )

instance Lude.ToJSON MitigationActionParams where
  toJSON MitigationActionParams' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("enableIoTLoggingParams" Lude..=)
              Lude.<$> enableIOTLoggingParams,
            ("addThingsToThingGroupParams" Lude..=)
              Lude.<$> addThingsToThingGroupParams,
            ("updateCACertificateParams" Lude..=)
              Lude.<$> updateCACertificateParams,
            ("updateDeviceCertificateParams" Lude..=)
              Lude.<$> updateDeviceCertificateParams,
            ("replaceDefaultPolicyVersionParams" Lude..=)
              Lude.<$> replaceDefaultPolicyVersionParams,
            ("publishFindingToSnsParams" Lude..=)
              Lude.<$> publishFindingToSNSParams
          ]
      )
