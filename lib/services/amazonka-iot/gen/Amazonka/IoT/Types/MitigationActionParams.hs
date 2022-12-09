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
-- Module      : Amazonka.IoT.Types.MitigationActionParams
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.MitigationActionParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AddThingsToThingGroupParams
import Amazonka.IoT.Types.EnableIoTLoggingParams
import Amazonka.IoT.Types.PublishFindingToSnsParams
import Amazonka.IoT.Types.ReplaceDefaultPolicyVersionParams
import Amazonka.IoT.Types.UpdateCACertificateParams
import Amazonka.IoT.Types.UpdateDeviceCertificateParams
import qualified Amazonka.Prelude as Prelude

-- | The set of parameters for this mitigation action. You can specify only
-- one type of parameter (in other words, you can apply only one action for
-- each defined mitigation action).
--
-- /See:/ 'newMitigationActionParams' smart constructor.
data MitigationActionParams = MitigationActionParams'
  { -- | Parameters to define a mitigation action that moves devices associated
    -- with a certificate to one or more specified thing groups, typically for
    -- quarantine.
    addThingsToThingGroupParams :: Prelude.Maybe AddThingsToThingGroupParams,
    -- | Parameters to define a mitigation action that enables Amazon Web
    -- Services IoT Core logging at a specified level of detail.
    enableIoTLoggingParams :: Prelude.Maybe EnableIoTLoggingParams,
    -- | Parameters to define a mitigation action that publishes findings to
    -- Amazon Simple Notification Service (Amazon SNS. You can implement your
    -- own custom actions in response to the Amazon SNS messages.
    publishFindingToSnsParams :: Prelude.Maybe PublishFindingToSnsParams,
    -- | Parameters to define a mitigation action that adds a blank policy to
    -- restrict permissions.
    replaceDefaultPolicyVersionParams :: Prelude.Maybe ReplaceDefaultPolicyVersionParams,
    -- | Parameters to define a mitigation action that changes the state of the
    -- CA certificate to inactive.
    updateCACertificateParams :: Prelude.Maybe UpdateCACertificateParams,
    -- | Parameters to define a mitigation action that changes the state of the
    -- device certificate to inactive.
    updateDeviceCertificateParams :: Prelude.Maybe UpdateDeviceCertificateParams
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MitigationActionParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addThingsToThingGroupParams', 'mitigationActionParams_addThingsToThingGroupParams' - Parameters to define a mitigation action that moves devices associated
-- with a certificate to one or more specified thing groups, typically for
-- quarantine.
--
-- 'enableIoTLoggingParams', 'mitigationActionParams_enableIoTLoggingParams' - Parameters to define a mitigation action that enables Amazon Web
-- Services IoT Core logging at a specified level of detail.
--
-- 'publishFindingToSnsParams', 'mitigationActionParams_publishFindingToSnsParams' - Parameters to define a mitigation action that publishes findings to
-- Amazon Simple Notification Service (Amazon SNS. You can implement your
-- own custom actions in response to the Amazon SNS messages.
--
-- 'replaceDefaultPolicyVersionParams', 'mitigationActionParams_replaceDefaultPolicyVersionParams' - Parameters to define a mitigation action that adds a blank policy to
-- restrict permissions.
--
-- 'updateCACertificateParams', 'mitigationActionParams_updateCACertificateParams' - Parameters to define a mitigation action that changes the state of the
-- CA certificate to inactive.
--
-- 'updateDeviceCertificateParams', 'mitigationActionParams_updateDeviceCertificateParams' - Parameters to define a mitigation action that changes the state of the
-- device certificate to inactive.
newMitigationActionParams ::
  MitigationActionParams
newMitigationActionParams =
  MitigationActionParams'
    { addThingsToThingGroupParams =
        Prelude.Nothing,
      enableIoTLoggingParams = Prelude.Nothing,
      publishFindingToSnsParams = Prelude.Nothing,
      replaceDefaultPolicyVersionParams = Prelude.Nothing,
      updateCACertificateParams = Prelude.Nothing,
      updateDeviceCertificateParams = Prelude.Nothing
    }

-- | Parameters to define a mitigation action that moves devices associated
-- with a certificate to one or more specified thing groups, typically for
-- quarantine.
mitigationActionParams_addThingsToThingGroupParams :: Lens.Lens' MitigationActionParams (Prelude.Maybe AddThingsToThingGroupParams)
mitigationActionParams_addThingsToThingGroupParams = Lens.lens (\MitigationActionParams' {addThingsToThingGroupParams} -> addThingsToThingGroupParams) (\s@MitigationActionParams' {} a -> s {addThingsToThingGroupParams = a} :: MitigationActionParams)

-- | Parameters to define a mitigation action that enables Amazon Web
-- Services IoT Core logging at a specified level of detail.
mitigationActionParams_enableIoTLoggingParams :: Lens.Lens' MitigationActionParams (Prelude.Maybe EnableIoTLoggingParams)
mitigationActionParams_enableIoTLoggingParams = Lens.lens (\MitigationActionParams' {enableIoTLoggingParams} -> enableIoTLoggingParams) (\s@MitigationActionParams' {} a -> s {enableIoTLoggingParams = a} :: MitigationActionParams)

-- | Parameters to define a mitigation action that publishes findings to
-- Amazon Simple Notification Service (Amazon SNS. You can implement your
-- own custom actions in response to the Amazon SNS messages.
mitigationActionParams_publishFindingToSnsParams :: Lens.Lens' MitigationActionParams (Prelude.Maybe PublishFindingToSnsParams)
mitigationActionParams_publishFindingToSnsParams = Lens.lens (\MitigationActionParams' {publishFindingToSnsParams} -> publishFindingToSnsParams) (\s@MitigationActionParams' {} a -> s {publishFindingToSnsParams = a} :: MitigationActionParams)

-- | Parameters to define a mitigation action that adds a blank policy to
-- restrict permissions.
mitigationActionParams_replaceDefaultPolicyVersionParams :: Lens.Lens' MitigationActionParams (Prelude.Maybe ReplaceDefaultPolicyVersionParams)
mitigationActionParams_replaceDefaultPolicyVersionParams = Lens.lens (\MitigationActionParams' {replaceDefaultPolicyVersionParams} -> replaceDefaultPolicyVersionParams) (\s@MitigationActionParams' {} a -> s {replaceDefaultPolicyVersionParams = a} :: MitigationActionParams)

-- | Parameters to define a mitigation action that changes the state of the
-- CA certificate to inactive.
mitigationActionParams_updateCACertificateParams :: Lens.Lens' MitigationActionParams (Prelude.Maybe UpdateCACertificateParams)
mitigationActionParams_updateCACertificateParams = Lens.lens (\MitigationActionParams' {updateCACertificateParams} -> updateCACertificateParams) (\s@MitigationActionParams' {} a -> s {updateCACertificateParams = a} :: MitigationActionParams)

-- | Parameters to define a mitigation action that changes the state of the
-- device certificate to inactive.
mitigationActionParams_updateDeviceCertificateParams :: Lens.Lens' MitigationActionParams (Prelude.Maybe UpdateDeviceCertificateParams)
mitigationActionParams_updateDeviceCertificateParams = Lens.lens (\MitigationActionParams' {updateDeviceCertificateParams} -> updateDeviceCertificateParams) (\s@MitigationActionParams' {} a -> s {updateDeviceCertificateParams = a} :: MitigationActionParams)

instance Data.FromJSON MitigationActionParams where
  parseJSON =
    Data.withObject
      "MitigationActionParams"
      ( \x ->
          MitigationActionParams'
            Prelude.<$> (x Data..:? "addThingsToThingGroupParams")
            Prelude.<*> (x Data..:? "enableIoTLoggingParams")
            Prelude.<*> (x Data..:? "publishFindingToSnsParams")
            Prelude.<*> (x Data..:? "replaceDefaultPolicyVersionParams")
            Prelude.<*> (x Data..:? "updateCACertificateParams")
            Prelude.<*> (x Data..:? "updateDeviceCertificateParams")
      )

instance Prelude.Hashable MitigationActionParams where
  hashWithSalt _salt MitigationActionParams' {..} =
    _salt
      `Prelude.hashWithSalt` addThingsToThingGroupParams
      `Prelude.hashWithSalt` enableIoTLoggingParams
      `Prelude.hashWithSalt` publishFindingToSnsParams
      `Prelude.hashWithSalt` replaceDefaultPolicyVersionParams
      `Prelude.hashWithSalt` updateCACertificateParams
      `Prelude.hashWithSalt` updateDeviceCertificateParams

instance Prelude.NFData MitigationActionParams where
  rnf MitigationActionParams' {..} =
    Prelude.rnf addThingsToThingGroupParams
      `Prelude.seq` Prelude.rnf enableIoTLoggingParams
      `Prelude.seq` Prelude.rnf publishFindingToSnsParams
      `Prelude.seq` Prelude.rnf replaceDefaultPolicyVersionParams
      `Prelude.seq` Prelude.rnf updateCACertificateParams
      `Prelude.seq` Prelude.rnf updateDeviceCertificateParams

instance Data.ToJSON MitigationActionParams where
  toJSON MitigationActionParams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addThingsToThingGroupParams" Data..=)
              Prelude.<$> addThingsToThingGroupParams,
            ("enableIoTLoggingParams" Data..=)
              Prelude.<$> enableIoTLoggingParams,
            ("publishFindingToSnsParams" Data..=)
              Prelude.<$> publishFindingToSnsParams,
            ("replaceDefaultPolicyVersionParams" Data..=)
              Prelude.<$> replaceDefaultPolicyVersionParams,
            ("updateCACertificateParams" Data..=)
              Prelude.<$> updateCACertificateParams,
            ("updateDeviceCertificateParams" Data..=)
              Prelude.<$> updateDeviceCertificateParams
          ]
      )
