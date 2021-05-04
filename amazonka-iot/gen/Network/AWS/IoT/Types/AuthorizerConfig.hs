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
-- Module      : Network.AWS.IoT.Types.AuthorizerConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthorizerConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that specifies the authorization service for a domain.
--
-- /See:/ 'newAuthorizerConfig' smart constructor.
data AuthorizerConfig = AuthorizerConfig'
  { -- | A Boolean that specifies whether the domain configuration\'s
    -- authorization service can be overridden.
    allowAuthorizerOverride :: Prelude.Maybe Prelude.Bool,
    -- | The name of the authorization service for a domain configuration.
    defaultAuthorizerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AuthorizerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowAuthorizerOverride', 'authorizerConfig_allowAuthorizerOverride' - A Boolean that specifies whether the domain configuration\'s
-- authorization service can be overridden.
--
-- 'defaultAuthorizerName', 'authorizerConfig_defaultAuthorizerName' - The name of the authorization service for a domain configuration.
newAuthorizerConfig ::
  AuthorizerConfig
newAuthorizerConfig =
  AuthorizerConfig'
    { allowAuthorizerOverride =
        Prelude.Nothing,
      defaultAuthorizerName = Prelude.Nothing
    }

-- | A Boolean that specifies whether the domain configuration\'s
-- authorization service can be overridden.
authorizerConfig_allowAuthorizerOverride :: Lens.Lens' AuthorizerConfig (Prelude.Maybe Prelude.Bool)
authorizerConfig_allowAuthorizerOverride = Lens.lens (\AuthorizerConfig' {allowAuthorizerOverride} -> allowAuthorizerOverride) (\s@AuthorizerConfig' {} a -> s {allowAuthorizerOverride = a} :: AuthorizerConfig)

-- | The name of the authorization service for a domain configuration.
authorizerConfig_defaultAuthorizerName :: Lens.Lens' AuthorizerConfig (Prelude.Maybe Prelude.Text)
authorizerConfig_defaultAuthorizerName = Lens.lens (\AuthorizerConfig' {defaultAuthorizerName} -> defaultAuthorizerName) (\s@AuthorizerConfig' {} a -> s {defaultAuthorizerName = a} :: AuthorizerConfig)

instance Prelude.FromJSON AuthorizerConfig where
  parseJSON =
    Prelude.withObject
      "AuthorizerConfig"
      ( \x ->
          AuthorizerConfig'
            Prelude.<$> (x Prelude..:? "allowAuthorizerOverride")
            Prelude.<*> (x Prelude..:? "defaultAuthorizerName")
      )

instance Prelude.Hashable AuthorizerConfig

instance Prelude.NFData AuthorizerConfig

instance Prelude.ToJSON AuthorizerConfig where
  toJSON AuthorizerConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("allowAuthorizerOverride" Prelude..=)
              Prelude.<$> allowAuthorizerOverride,
            ("defaultAuthorizerName" Prelude..=)
              Prelude.<$> defaultAuthorizerName
          ]
      )
