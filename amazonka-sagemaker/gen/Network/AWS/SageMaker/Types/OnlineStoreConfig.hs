{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.OnlineStoreConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OnlineStoreConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.OnlineStoreSecurityConfig

-- | Use this to specify the AWS Key Management Service (KMS) Key ID, or
-- @KMSKeyId@, for at rest data encryption. You can turn @OnlineStore@ on
-- or off by specifying the @EnableOnlineStore@ flag at General Assembly;
-- the default value is @False@.
--
-- /See:/ 'newOnlineStoreConfig' smart constructor.
data OnlineStoreConfig = OnlineStoreConfig'
  { -- | Use to specify KMS Key ID (@KMSKeyId@) for at-rest encryption of your
    -- @OnlineStore@.
    securityConfig :: Prelude.Maybe OnlineStoreSecurityConfig,
    -- | Turn @OnlineStore@ off by specifying @False@ for the @EnableOnlineStore@
    -- flag. Turn @OnlineStore@ on by specifying @True@ for the
    -- @EnableOnlineStore@ flag.
    --
    -- The default value is @False@.
    enableOnlineStore :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OnlineStoreConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfig', 'onlineStoreConfig_securityConfig' - Use to specify KMS Key ID (@KMSKeyId@) for at-rest encryption of your
-- @OnlineStore@.
--
-- 'enableOnlineStore', 'onlineStoreConfig_enableOnlineStore' - Turn @OnlineStore@ off by specifying @False@ for the @EnableOnlineStore@
-- flag. Turn @OnlineStore@ on by specifying @True@ for the
-- @EnableOnlineStore@ flag.
--
-- The default value is @False@.
newOnlineStoreConfig ::
  OnlineStoreConfig
newOnlineStoreConfig =
  OnlineStoreConfig'
    { securityConfig =
        Prelude.Nothing,
      enableOnlineStore = Prelude.Nothing
    }

-- | Use to specify KMS Key ID (@KMSKeyId@) for at-rest encryption of your
-- @OnlineStore@.
onlineStoreConfig_securityConfig :: Lens.Lens' OnlineStoreConfig (Prelude.Maybe OnlineStoreSecurityConfig)
onlineStoreConfig_securityConfig = Lens.lens (\OnlineStoreConfig' {securityConfig} -> securityConfig) (\s@OnlineStoreConfig' {} a -> s {securityConfig = a} :: OnlineStoreConfig)

-- | Turn @OnlineStore@ off by specifying @False@ for the @EnableOnlineStore@
-- flag. Turn @OnlineStore@ on by specifying @True@ for the
-- @EnableOnlineStore@ flag.
--
-- The default value is @False@.
onlineStoreConfig_enableOnlineStore :: Lens.Lens' OnlineStoreConfig (Prelude.Maybe Prelude.Bool)
onlineStoreConfig_enableOnlineStore = Lens.lens (\OnlineStoreConfig' {enableOnlineStore} -> enableOnlineStore) (\s@OnlineStoreConfig' {} a -> s {enableOnlineStore = a} :: OnlineStoreConfig)

instance Prelude.FromJSON OnlineStoreConfig where
  parseJSON =
    Prelude.withObject
      "OnlineStoreConfig"
      ( \x ->
          OnlineStoreConfig'
            Prelude.<$> (x Prelude..:? "SecurityConfig")
            Prelude.<*> (x Prelude..:? "EnableOnlineStore")
      )

instance Prelude.Hashable OnlineStoreConfig

instance Prelude.NFData OnlineStoreConfig

instance Prelude.ToJSON OnlineStoreConfig where
  toJSON OnlineStoreConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SecurityConfig" Prelude..=)
              Prelude.<$> securityConfig,
            ("EnableOnlineStore" Prelude..=)
              Prelude.<$> enableOnlineStore
          ]
      )
