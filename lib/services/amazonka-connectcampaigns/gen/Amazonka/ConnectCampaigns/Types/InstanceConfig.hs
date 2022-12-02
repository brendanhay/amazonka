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
-- Module      : Amazonka.ConnectCampaigns.Types.InstanceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.InstanceConfig where

import Amazonka.ConnectCampaigns.Types.EncryptionConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Instance config object
--
-- /See:/ 'newInstanceConfig' smart constructor.
data InstanceConfig = InstanceConfig'
  { connectInstanceId :: Prelude.Text,
    encryptionConfig :: EncryptionConfig,
    serviceLinkedRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectInstanceId', 'instanceConfig_connectInstanceId' - Undocumented member.
--
-- 'encryptionConfig', 'instanceConfig_encryptionConfig' - Undocumented member.
--
-- 'serviceLinkedRoleArn', 'instanceConfig_serviceLinkedRoleArn' - Undocumented member.
newInstanceConfig ::
  -- | 'connectInstanceId'
  Prelude.Text ->
  -- | 'encryptionConfig'
  EncryptionConfig ->
  -- | 'serviceLinkedRoleArn'
  Prelude.Text ->
  InstanceConfig
newInstanceConfig
  pConnectInstanceId_
  pEncryptionConfig_
  pServiceLinkedRoleArn_ =
    InstanceConfig'
      { connectInstanceId =
          pConnectInstanceId_,
        encryptionConfig = pEncryptionConfig_,
        serviceLinkedRoleArn = pServiceLinkedRoleArn_
      }

-- | Undocumented member.
instanceConfig_connectInstanceId :: Lens.Lens' InstanceConfig Prelude.Text
instanceConfig_connectInstanceId = Lens.lens (\InstanceConfig' {connectInstanceId} -> connectInstanceId) (\s@InstanceConfig' {} a -> s {connectInstanceId = a} :: InstanceConfig)

-- | Undocumented member.
instanceConfig_encryptionConfig :: Lens.Lens' InstanceConfig EncryptionConfig
instanceConfig_encryptionConfig = Lens.lens (\InstanceConfig' {encryptionConfig} -> encryptionConfig) (\s@InstanceConfig' {} a -> s {encryptionConfig = a} :: InstanceConfig)

-- | Undocumented member.
instanceConfig_serviceLinkedRoleArn :: Lens.Lens' InstanceConfig Prelude.Text
instanceConfig_serviceLinkedRoleArn = Lens.lens (\InstanceConfig' {serviceLinkedRoleArn} -> serviceLinkedRoleArn) (\s@InstanceConfig' {} a -> s {serviceLinkedRoleArn = a} :: InstanceConfig)

instance Data.FromJSON InstanceConfig where
  parseJSON =
    Data.withObject
      "InstanceConfig"
      ( \x ->
          InstanceConfig'
            Prelude.<$> (x Data..: "connectInstanceId")
            Prelude.<*> (x Data..: "encryptionConfig")
            Prelude.<*> (x Data..: "serviceLinkedRoleArn")
      )

instance Prelude.Hashable InstanceConfig where
  hashWithSalt _salt InstanceConfig' {..} =
    _salt `Prelude.hashWithSalt` connectInstanceId
      `Prelude.hashWithSalt` encryptionConfig
      `Prelude.hashWithSalt` serviceLinkedRoleArn

instance Prelude.NFData InstanceConfig where
  rnf InstanceConfig' {..} =
    Prelude.rnf connectInstanceId
      `Prelude.seq` Prelude.rnf encryptionConfig
      `Prelude.seq` Prelude.rnf serviceLinkedRoleArn
