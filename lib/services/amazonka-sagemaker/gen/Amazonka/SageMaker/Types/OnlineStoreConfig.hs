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
-- Module      : Amazonka.SageMaker.Types.OnlineStoreConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.OnlineStoreConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.OnlineStoreSecurityConfig

-- | Use this to specify the Amazon Web Services Key Management Service (KMS)
-- Key ID, or @KMSKeyId@, for at rest data encryption. You can turn
-- @OnlineStore@ on or off by specifying the @EnableOnlineStore@ flag at
-- General Assembly; the default value is @False@.
--
-- /See:/ 'newOnlineStoreConfig' smart constructor.
data OnlineStoreConfig = OnlineStoreConfig'
  { -- | Turn @OnlineStore@ off by specifying @False@ for the @EnableOnlineStore@
    -- flag. Turn @OnlineStore@ on by specifying @True@ for the
    -- @EnableOnlineStore@ flag.
    --
    -- The default value is @False@.
    enableOnlineStore :: Prelude.Maybe Prelude.Bool,
    -- | Use to specify KMS Key ID (@KMSKeyId@) for at-rest encryption of your
    -- @OnlineStore@.
    securityConfig :: Prelude.Maybe OnlineStoreSecurityConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnlineStoreConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableOnlineStore', 'onlineStoreConfig_enableOnlineStore' - Turn @OnlineStore@ off by specifying @False@ for the @EnableOnlineStore@
-- flag. Turn @OnlineStore@ on by specifying @True@ for the
-- @EnableOnlineStore@ flag.
--
-- The default value is @False@.
--
-- 'securityConfig', 'onlineStoreConfig_securityConfig' - Use to specify KMS Key ID (@KMSKeyId@) for at-rest encryption of your
-- @OnlineStore@.
newOnlineStoreConfig ::
  OnlineStoreConfig
newOnlineStoreConfig =
  OnlineStoreConfig'
    { enableOnlineStore =
        Prelude.Nothing,
      securityConfig = Prelude.Nothing
    }

-- | Turn @OnlineStore@ off by specifying @False@ for the @EnableOnlineStore@
-- flag. Turn @OnlineStore@ on by specifying @True@ for the
-- @EnableOnlineStore@ flag.
--
-- The default value is @False@.
onlineStoreConfig_enableOnlineStore :: Lens.Lens' OnlineStoreConfig (Prelude.Maybe Prelude.Bool)
onlineStoreConfig_enableOnlineStore = Lens.lens (\OnlineStoreConfig' {enableOnlineStore} -> enableOnlineStore) (\s@OnlineStoreConfig' {} a -> s {enableOnlineStore = a} :: OnlineStoreConfig)

-- | Use to specify KMS Key ID (@KMSKeyId@) for at-rest encryption of your
-- @OnlineStore@.
onlineStoreConfig_securityConfig :: Lens.Lens' OnlineStoreConfig (Prelude.Maybe OnlineStoreSecurityConfig)
onlineStoreConfig_securityConfig = Lens.lens (\OnlineStoreConfig' {securityConfig} -> securityConfig) (\s@OnlineStoreConfig' {} a -> s {securityConfig = a} :: OnlineStoreConfig)

instance Core.FromJSON OnlineStoreConfig where
  parseJSON =
    Core.withObject
      "OnlineStoreConfig"
      ( \x ->
          OnlineStoreConfig'
            Prelude.<$> (x Core..:? "EnableOnlineStore")
            Prelude.<*> (x Core..:? "SecurityConfig")
      )

instance Prelude.Hashable OnlineStoreConfig where
  hashWithSalt _salt OnlineStoreConfig' {..} =
    _salt `Prelude.hashWithSalt` enableOnlineStore
      `Prelude.hashWithSalt` securityConfig

instance Prelude.NFData OnlineStoreConfig where
  rnf OnlineStoreConfig' {..} =
    Prelude.rnf enableOnlineStore
      `Prelude.seq` Prelude.rnf securityConfig

instance Core.ToJSON OnlineStoreConfig where
  toJSON OnlineStoreConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EnableOnlineStore" Core..=)
              Prelude.<$> enableOnlineStore,
            ("SecurityConfig" Core..=)
              Prelude.<$> securityConfig
          ]
      )
