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
-- Module      : Network.AWS.IoT.Types.MitigationActionParams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MitigationActionParams where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AddThingsToThingGroupParams
import Network.AWS.IoT.Types.EnableIoTLoggingParams
import Network.AWS.IoT.Types.PublishFindingToSnsParams
import Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams
import Network.AWS.IoT.Types.UpdateCACertificateParams
import Network.AWS.IoT.Types.UpdateDeviceCertificateParams
import qualified Network.AWS.Lens as Lens

-- | The set of parameters for this mitigation action. You can specify only
-- one type of parameter (in other words, you can apply only one action for
-- each defined mitigation action).
--
-- /See:/ 'newMitigationActionParams' smart constructor.
data MitigationActionParams = MitigationActionParams'
  { -- | Parameters to define a mitigation action that enables AWS IoT logging at
    -- a specified level of detail.
    enableIoTLoggingParams :: Core.Maybe EnableIoTLoggingParams,
    -- | Parameters to define a mitigation action that adds a blank policy to
    -- restrict permissions.
    replaceDefaultPolicyVersionParams :: Core.Maybe ReplaceDefaultPolicyVersionParams,
    -- | Parameters to define a mitigation action that changes the state of the
    -- device certificate to inactive.
    updateDeviceCertificateParams :: Core.Maybe UpdateDeviceCertificateParams,
    -- | Parameters to define a mitigation action that publishes findings to
    -- Amazon Simple Notification Service (Amazon SNS. You can implement your
    -- own custom actions in response to the Amazon SNS messages.
    publishFindingToSnsParams :: Core.Maybe PublishFindingToSnsParams,
    -- | Parameters to define a mitigation action that moves devices associated
    -- with a certificate to one or more specified thing groups, typically for
    -- quarantine.
    addThingsToThingGroupParams :: Core.Maybe AddThingsToThingGroupParams,
    -- | Parameters to define a mitigation action that changes the state of the
    -- CA certificate to inactive.
    updateCACertificateParams :: Core.Maybe UpdateCACertificateParams
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MitigationActionParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableIoTLoggingParams', 'mitigationActionParams_enableIoTLoggingParams' - Parameters to define a mitigation action that enables AWS IoT logging at
-- a specified level of detail.
--
-- 'replaceDefaultPolicyVersionParams', 'mitigationActionParams_replaceDefaultPolicyVersionParams' - Parameters to define a mitigation action that adds a blank policy to
-- restrict permissions.
--
-- 'updateDeviceCertificateParams', 'mitigationActionParams_updateDeviceCertificateParams' - Parameters to define a mitigation action that changes the state of the
-- device certificate to inactive.
--
-- 'publishFindingToSnsParams', 'mitigationActionParams_publishFindingToSnsParams' - Parameters to define a mitigation action that publishes findings to
-- Amazon Simple Notification Service (Amazon SNS. You can implement your
-- own custom actions in response to the Amazon SNS messages.
--
-- 'addThingsToThingGroupParams', 'mitigationActionParams_addThingsToThingGroupParams' - Parameters to define a mitigation action that moves devices associated
-- with a certificate to one or more specified thing groups, typically for
-- quarantine.
--
-- 'updateCACertificateParams', 'mitigationActionParams_updateCACertificateParams' - Parameters to define a mitigation action that changes the state of the
-- CA certificate to inactive.
newMitigationActionParams ::
  MitigationActionParams
newMitigationActionParams =
  MitigationActionParams'
    { enableIoTLoggingParams =
        Core.Nothing,
      replaceDefaultPolicyVersionParams = Core.Nothing,
      updateDeviceCertificateParams = Core.Nothing,
      publishFindingToSnsParams = Core.Nothing,
      addThingsToThingGroupParams = Core.Nothing,
      updateCACertificateParams = Core.Nothing
    }

-- | Parameters to define a mitigation action that enables AWS IoT logging at
-- a specified level of detail.
mitigationActionParams_enableIoTLoggingParams :: Lens.Lens' MitigationActionParams (Core.Maybe EnableIoTLoggingParams)
mitigationActionParams_enableIoTLoggingParams = Lens.lens (\MitigationActionParams' {enableIoTLoggingParams} -> enableIoTLoggingParams) (\s@MitigationActionParams' {} a -> s {enableIoTLoggingParams = a} :: MitigationActionParams)

-- | Parameters to define a mitigation action that adds a blank policy to
-- restrict permissions.
mitigationActionParams_replaceDefaultPolicyVersionParams :: Lens.Lens' MitigationActionParams (Core.Maybe ReplaceDefaultPolicyVersionParams)
mitigationActionParams_replaceDefaultPolicyVersionParams = Lens.lens (\MitigationActionParams' {replaceDefaultPolicyVersionParams} -> replaceDefaultPolicyVersionParams) (\s@MitigationActionParams' {} a -> s {replaceDefaultPolicyVersionParams = a} :: MitigationActionParams)

-- | Parameters to define a mitigation action that changes the state of the
-- device certificate to inactive.
mitigationActionParams_updateDeviceCertificateParams :: Lens.Lens' MitigationActionParams (Core.Maybe UpdateDeviceCertificateParams)
mitigationActionParams_updateDeviceCertificateParams = Lens.lens (\MitigationActionParams' {updateDeviceCertificateParams} -> updateDeviceCertificateParams) (\s@MitigationActionParams' {} a -> s {updateDeviceCertificateParams = a} :: MitigationActionParams)

-- | Parameters to define a mitigation action that publishes findings to
-- Amazon Simple Notification Service (Amazon SNS. You can implement your
-- own custom actions in response to the Amazon SNS messages.
mitigationActionParams_publishFindingToSnsParams :: Lens.Lens' MitigationActionParams (Core.Maybe PublishFindingToSnsParams)
mitigationActionParams_publishFindingToSnsParams = Lens.lens (\MitigationActionParams' {publishFindingToSnsParams} -> publishFindingToSnsParams) (\s@MitigationActionParams' {} a -> s {publishFindingToSnsParams = a} :: MitigationActionParams)

-- | Parameters to define a mitigation action that moves devices associated
-- with a certificate to one or more specified thing groups, typically for
-- quarantine.
mitigationActionParams_addThingsToThingGroupParams :: Lens.Lens' MitigationActionParams (Core.Maybe AddThingsToThingGroupParams)
mitigationActionParams_addThingsToThingGroupParams = Lens.lens (\MitigationActionParams' {addThingsToThingGroupParams} -> addThingsToThingGroupParams) (\s@MitigationActionParams' {} a -> s {addThingsToThingGroupParams = a} :: MitigationActionParams)

-- | Parameters to define a mitigation action that changes the state of the
-- CA certificate to inactive.
mitigationActionParams_updateCACertificateParams :: Lens.Lens' MitigationActionParams (Core.Maybe UpdateCACertificateParams)
mitigationActionParams_updateCACertificateParams = Lens.lens (\MitigationActionParams' {updateCACertificateParams} -> updateCACertificateParams) (\s@MitigationActionParams' {} a -> s {updateCACertificateParams = a} :: MitigationActionParams)

instance Core.FromJSON MitigationActionParams where
  parseJSON =
    Core.withObject
      "MitigationActionParams"
      ( \x ->
          MitigationActionParams'
            Core.<$> (x Core..:? "enableIoTLoggingParams")
            Core.<*> (x Core..:? "replaceDefaultPolicyVersionParams")
            Core.<*> (x Core..:? "updateDeviceCertificateParams")
            Core.<*> (x Core..:? "publishFindingToSnsParams")
            Core.<*> (x Core..:? "addThingsToThingGroupParams")
            Core.<*> (x Core..:? "updateCACertificateParams")
      )

instance Core.Hashable MitigationActionParams

instance Core.NFData MitigationActionParams

instance Core.ToJSON MitigationActionParams where
  toJSON MitigationActionParams' {..} =
    Core.object
      ( Core.catMaybes
          [ ("enableIoTLoggingParams" Core..=)
              Core.<$> enableIoTLoggingParams,
            ("replaceDefaultPolicyVersionParams" Core..=)
              Core.<$> replaceDefaultPolicyVersionParams,
            ("updateDeviceCertificateParams" Core..=)
              Core.<$> updateDeviceCertificateParams,
            ("publishFindingToSnsParams" Core..=)
              Core.<$> publishFindingToSnsParams,
            ("addThingsToThingGroupParams" Core..=)
              Core.<$> addThingsToThingGroupParams,
            ("updateCACertificateParams" Core..=)
              Core.<$> updateCACertificateParams
          ]
      )
